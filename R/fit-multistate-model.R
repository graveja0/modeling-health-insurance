# TK incorporate survey weights into parametric regression models

fit_multistate_model <-
  function(df,
           tmat,
           fit_type = "weibull",
           ff,
           idvar ,
           weight = NULL,
           prediction_vals = data.frame(tx = c(1, 0))) {
    id <- enquo(idvar)
    
    rhs <- colnames(prediction_vals)
    
    # Get the transition types
    transitions <- df$trans %>% unique()
    
    # Lookup table maps the transtiion number to the type
    transition_lut = tmat[1, ] %>% data.frame() %>% rownames_to_column() %>% set_names(c("transition_type", "transition_num")) %>%
      filter(!is.na(transition_num)) %>% arrange(transition_num)
    if (fit_type %in% c("kaplan-meier")) {
      tmp_fit <-
        transitions %>%
        map( ~ (
          df %>%
            filter(trans == .x) %>%
            survfit(ff, data = .,type="kaplan-meier", weights = weight)
        )) %>%
        set_names(transition_lut$transition_type)
      params = NULL
      coeffs = NULL
      lp <- tmp_fit %>% map(~(summary(.x)))
    } else if (fit_type %in% c("weibull")) {
      tmp_fit <-
        transitions %>%
        map( ~ (
          df %>%
            filter(trans == .x) %>%
            phreg(ff, data = ., dist = "weibull")
        )) %>%
        set_names(transition_lut$transition_type)
      params <- tmp_fit %>% map(~(.x$coefficients[-which(names(.x$coefficients) %in% rhs)] ))
      coeffs <- tmp_fit %>% map(~(.x$coefficients[which(names(.x$coefficients) %in% rhs)] ))
      lp <- tmp_fit %>% map(~(as.matrix(prediction_vals) %*% .x$coefficients[which(names(.x$coefficients) %in% rhs)]))
    } else if (fit_type %in% c("exp")) {
      tmp_fit <-
        transitions %>%
        map( ~ (
          df %>%
            filter(trans == .x) %>%
            phreg(
              ff,
              data = .,
              dist = "weibull",
              shape = 1
            )
        )) %>%
        set_names(transition_lut$transition_type)
      
      params <- tmp_fit %>% map(~(.x$coefficients[-which(names(.x$coefficients) %in% rhs)] ))
      coeffs <- tmp_fit %>% map(~(.x$coefficients[which(names(.x$coefficients) %in% rhs)] ))
      lp <- tmp_fit %>% map(~(as.matrix(prediction_vals) %*% .x$coefficients[which(names(.x$coefficients) %in% rhs)] ))
      
    } else if (fit_type %in% c("gompertz")) {
      tmp_fit <-
        transitions %>%
        map( ~ (
          df %>%
            filter(trans == .x) %>%
            phreg(
              ff,
              data = .,
              dist = "gompertz",
              param = "rate"
            )
        )) %>%
        set_names(transition_lut$transition_type)
      params <- tmp_fit %>% map(~(.x$coefficients[-which(names(.x$coefficients) %in% rhs)] ))
      coeffs <- tmp_fit %>% map(~(.x$coefficients[which(names(.x$coefficients) %in% rhs)] ))
      lp <- tmp_fit %>% map(~(as.matrix(prediction_vals) %*% .x$coefficients[which(names(.x$coefficients) %in% rhs)] ))
    } else if (fit_type %in% c("loglogistic","lognormal")) {
      tmp_fit <-
        transitions %>%
        map( ~ (
          df %>%
            filter(trans == .x) %>%
            aftreg(
              ff,
              data = .,
              dist = fit_type
            )
        )) %>%
        set_names(transition_lut$transition_type)
      params <- tmp_fit %>% map(~(.x$coefficients[-which(names(.x$coefficients) %in% rhs)] ))
      coeffs <- tmp_fit %>% map(~(.x$coefficients[which(names(.x$coefficients) %in% rhs)] ))
      lp <- tmp_fit %>% map(~(as.matrix(prediction_vals) %*% .x$coefficients[which(names(.x$coefficients) %in% rhs)] ))
    } else if (fit_type %in% c("gengamma")) {
      tmp_fit <-
        transitions %>%
        map( ~ (
          df %>%
            filter(trans == .x) %>%
            flexsurvreg(
              ff,
              data = .,
              dist = "gengamma"
            )
        )) %>%
        set_names(transition_lut$transition_type)
      
      kappa <- transitions %>% 
        map(~(
          tmp_fit[[.x]]$res[3,1] 
        )) %>% 
        set_names(transition_lut$transition_type)
      
      gamma <- 
        transitions %>% 
        map(~(
          (abs(kappa[[.x]]))^(-2) 
        )) %>% 
        set_names(transition_lut$transition_type)
      
      coeffs <- transitions %>% 
        map(~(
          tmp_fit[[.x]]$res %>% data.frame() %>% rownames_to_column() %>% 
            filter(rowname %in% colnames(prediction_vals)) %>% pull(est) 
        )) %>% 
        set_names(transition_lut$transition_type)
      
      lp <- coeffs %>% map( ~ (as.matrix(cbind(
        prediction_vals
      )) %*% as.matrix(.x) %>% as.vector())) %>% 
        set_names(transition_lut$transition_type)
      
      mu <- transitions %>% map(~(tmp_fit[[.x]]$res[1,1] + lp[[.x]] )) %>% 
        set_names(transition_lut$transition_type)
      
      sigma <- transitions %>% map(~(tmp_fit[[.x]]$res[2,1] )) %>% 
        set_names(transition_lut$transition_type)
      
      # z[[i]]<-rep(0,length(tt))
      # z[[i]]<- sign(kappa[[i]])*((log(tt)-mu[[i]])/sigma[[i]])
      # u[[i]]<-gamma[[i]]*exp((abs(kappa[[i]]))*z[[i]])
      # 
      
    }
    
    if (!fit_type %in% c("gengamma")) { 
      out <-
        list(
          fits = tmp_fit,
          params = params,
          coeffs = coeffs,
          lp = lp,
          lut = transition_lut,
          fit_type = fit_type
        )
    } else {
      out <-
        list(
          fits = tmp_fit,
          coeffs = coeffs,
          kappa = kappa,
          gamma = gamma,
          lp = lp,
          mu = mu,
          sigma = sigma,
          lut = transition_lut,
          fit_type = fit_type
        )
    }
  }

# weibull_cumHaz <- function(shape,scale,lp,tt) apply(lp,1,function(x) exp(-exp(shape)*scale + x) * tt ^ exp(shape))

# get_cumHaz <- function(ls_fit,tt,lut,dist = "weibull") {
#   times <- tt
# 
#   transitions = names(ls_fit$coeffs)
#   
#   if (dist == "weibull") {
#     est_shape <- transitions %>% map(~ls_fit$coeffs[[.x]][["log(shape)"]]) %>% set_names(transitions)
#     est_scale <- transitions %>% map(~ls_fit$coeffs[[.x]][["log(scale)"]]) %>% set_names(transitions)
#     cumHaz <- transitions %>% map(~weibull_cumHaz(shape = est_shape[[.x]], scale = est_scale[[.x]], lp = ls_fit$lp[[.x]],tt=times) %>% 
#                           tbl_df() %>% mutate(time = times) %>% select(time,everything())) %>% 
#       set_names(transitions)  
#     }
#     
#   out <- cumHaz %>% bind_rows(.id = "transition") %>% select(time,Haz = V1,transition_type = transition) %>% 
#     left_join(lut,c("transition_type")) %>% select(time,Haz,trans=transition_num) %>% data.frame()
#   return(out)
# }












