
fit_metamodel <- function(df, lambda, param_sets = NULL, hidden_layers = c(1,1), strategies = c("NONE","SUBSIDY","MEDICAID"), train_frac = 0.75, seed_val = 123) {
  
  # Identify LHS and RHS
  
  nwb_strategies = paste0("NWB_",strategies)
  
  nwb <- 
    df %>% 
    select(psa_id,contains("dWELF"),contains("dCOST")) %>%
    mutate(psa_id=row_number()) %>% 
    reshape2::melt(id.vars='psa_id') %>%
    tidyr::separate(variable,c("outcome","strategy"),"_") %>%
    reshape2::dcast(psa_id+strategy~outcome) %>%
    mutate(NWB = dWELF - dCOST * lambda ) %>% 
    mutate(strategy = factor(strategy,levels = strategies)) %>% 
    arrange(psa_id,strategy) %>% 
    select(psa_id,strategy,NWB) %>% 
    mutate(variable="NWB") %>% 
    reshape2::dcast(psa_id~variable+strategy,value.var="NWB") %>% 
    select(-psa_id) %>% 
    select( nwb_strategies)
  
  pi_tmp <- 
    df %>% 
    select(-contains("dWELF"),-contains("dCOST"),-contains("NWB"),-psa_id)
  
  # subset to only parameters that vary 
  pi_var <- 
    pi_tmp %>% 
    summarise_all(sd) %>% 
    gather(key,value) %>% 
    filter(value>0) %>% 
    pull(key)
  pi <- 
    pi_tmp %>% 
    select(pi_var)
  
  K <- nrow(pi) # number of simulations
  
  alpha_star <- which.max(colMeans(nwb)) # Dominant strategy (given specified level of lambda)
  
  loss <- nwb - nwb[, alpha_star] # Welfare loss
  
  evpi <- mean(rowMaxs(as.matrix(loss))) # Expected value of perfect information
  
  # Part 1. EVPPI for Single Parameters
  
  # xx <- names(pi)[1] #### TEMPOROARY
  # 
  set.seed(seed_val)
  index <- sample(1:nrow(df),round(train_frac*nrow(df)))
  
  train <- pi[index,]
  test <- pi[-index,]
  
  fit_linear_metamodel <- function(pi,nwb,strategies,xx,index) {
    lm_fit <- 
      strategies %>% 
      map(~(glm(as.formula(paste0("nwb[index,][['",.x,"']] ~ .")),data = pi[index,xx]))) %>% 
      set_names(strategies)
    
    pr_lm <- 
      lm_fit %>% 
      map(~(predict(.x,pi[-index,xx],type="response"))) %>% 
      set_names(strategies)
    
    MSE_lm <- 
      strategies %>% 
      map(~(sum((pr_lm[[.x]] - nwb[-index,][[.x]])^2)/nrow(test) )) %>% 
      set_names(strategies) %>% 
      bind_rows()
    
    perfect.info <- mean(do.call(pmax, pr_lm)) 
    baseline <- max(unlist(lapply(pr_lm, mean)))
    partial.evpi.lm <- perfect.info - baseline ## estimate EVPI
    
    mm_lm <- 
      data.frame(evppi = partial.evpi.lm) %>% 
      bind_cols(MSE_lm %>% set_names(paste0("MSE_",names(.)))) %>% 
      mutate(method = "linear") %>% 
      mutate(parameter = xx) %>% 
      select(parameter,everything())
    
    return(mm_lm)
  }
  
  cat("...fitting linear model\n")
  mm_lm <- 
    names(pi) %>% 
    map(~(
      fit_linear_metamodel(pi = pi, nwb = nwb, strategies = nwb_strategies, xx = .x, index = index)
    )) %>% 
    bind_rows() %>% 
    arrange(desc(evppi))
  
  fit_gam_metamodel <- function(pi,nwb,strategies,xx,index) {
    mm_gam <- 
      strategies %>% 
      map(~(gam(as.formula(paste0("nwb[index,][['",.x,"']] ~ ",paste0(paste0("s(",colnames(pi[,xx]),")"),collapse="+"))),
                data = pi[index,xx])
      )) %>% 
      set_names(strategies)
    
    pr_gam <- 
      mm_gam %>% 
      map(~(.x$fitted.values)) 
    
    MSE_gam <- 
      strategies %>% 
      map(~(sum((pr_gam[[.x]] - nwb[-index,][[.x]])^2)/nrow(test) )) %>% 
      set_names(strategies) %>% 
      bind_rows()
    
    perfect.info <- mean(do.call(pmax, pr_gam)) 
    baseline <- max(unlist(lapply(pr_gam, mean)))
    partial.evpi.gam <- perfect.info - baseline ## estimate EVPI
    
    mm_gam <- 
      data.frame(evppi = partial.evpi.gam) %>% 
      bind_cols(MSE_gam %>% set_names(paste0("MSE_",names(.)))) %>% 
      mutate(method = "gam") %>% 
      mutate(parameter = xx) %>% 
      select(parameter,everything())
    
    return(mm_gam)
  }
  cat("...fitting GAM\n")
  mm_gam <- 
    names(pi) %>% 
    map(~(
      fit_gam_metamodel(pi = pi, nwb = nwb, strategies = nwb_strategies, xx = .x, index = index)
    )) %>% 
    bind_rows() %>% 
    arrange(desc(evppi))
  
  fit_nn_metamodel <- function(pi,nwb,strategies,xx,index) {
    # Preprocess: Normalize data
    
    maxs <- apply(cbind(nwb,pi[,xx]), 2, max) 
    mins <- apply(cbind(nwb,pi[,xx]), 2, min)
    scaled <- as.data.frame(scale(cbind(nwb,pi[,xx]), center = mins, scale = maxs - mins))
    train_ <- scaled[index,]
    test_ <- scaled[-index,]
    
    train_[is.na(train_)] <- 0
    test_[is.na(test_)] <- 0
    
    fit_nn <- 
      strategies %>% 
      map(~(
        neuralnet(as.formula(paste0(.x,"~",paste0(setdiff(names(train_),strategies), collapse = "+"))), 
                  data = train_, 
                  hidden = hidden_layers, 
                  linear.output = TRUE)
      )) %>% 
      set_names(strategies)
    
    pr_nn <- 
      strategies %>% 
      map(~(
        compute(fit_nn[[.x]],scaled)   # normally would do on test_ data set, but doing on the full for now
      )) %>% 
      set_names(strategies)
    
    # rescale back to original scale
    pr_nn_ <- 
      strategies %>% 
      map(~(
        pr_nn[[.x]]$net.result * (max(nwb[[.x]]) - min(nwb[[.x]])) + min(nwb[[.x]])
      )) %>% 
      set_names(strategies)
    
    test_r <- 
      strategies %>% 
      map(~(
        (test_[[.x]])*(max(nwb[[.x]])-min(nwb[[.x]]))+min(nwb[[.x]])
      )) %>% 
      set_names(strategies)
    
    MSE_nn <- 
      strategies %>% 
      map(~(
        sum((test_r[[.x]] - pr_nn_[[.x]])^2)/nrow(test_)
      )) %>% 
      set_names(strategies)
    
    perfect.info <- mean(do.call(pmax, pr_nn_)) 
    baseline <- max(unlist(lapply(pr_nn_, mean)))
    
    loss <- do.call(pmax, pr_nn_) - pr_nn_[[which.max(colMeans(pr_nn_ %>% bind_cols()))]]
    
    partial.evpi <- perfect.info - baseline ## estimate EVPI
    
    mm_nn <- 
      data.frame(evppi = partial.evpi) %>% 
      bind_cols(MSE_nn %>% set_names(paste0("MSE_",names(.)))) %>% 
      mutate(method = "nn") %>% 
      mutate(parameter = xx) %>% 
      select(parameter,everything())
    
    return(mm_nn)
  }
  cat("...fitting neural network\n")
  set.seed(seed_val)
  
  mm_nn <- 
    names(pi) %>% 
    map(~(
      fit_nn_metamodel(pi = pi, nwb = nwb, strategies = nwb_strategies, xx = .x, index = index)
    )) %>% 
    bind_rows() %>% 
    arrange(desc(evppi))
  
  
  out <- mm_nn %>% bind_rows(mm_gam) %>% bind_rows(mm_lm) %>% 
    gather(measure,value,-parameter,-method) %>% 
    unite(measure,measure,method) %>% 
    spread(measure,value) %>% 
    mutate_at(vars(starts_with("evppi_")),list(norm = function(x) x / sum(x))) %>% 
    mutate(lambda = lambda) %>% 
    select(parameter,lambda,starts_with("evppi_"),everything()) 
  
  
  return(out)
  
}
