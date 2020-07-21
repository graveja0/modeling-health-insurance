##################
# Key Functions
##################

estimate_fsh_rd <- function(df, yvar, rd_X = c("aboveYCut1","aboveYCut2","aboveYCut3","income_grp","fpl_X_aboveYCut1","fpl_X_aboveYCut2","fpl_X_aboveYCut3")) {
  
  library(lmtest)
  library(sandwich)
  
  fit <- df %>% 
    #mutate_at(vars(yvar),function(x) x * 5) %>% 
    filter(inRD_sample==1) %>% 
    lm(as.formula(paste0(yvar,"~",paste0(rd_X,collapse="+"))), data = .) 
  
  fit_coef <- coefficients(fit)
  fit_vcov <- vcovHC(fit, type="HC1")
  
  lincom1 <- c(0,1,0,0,0,150,0,0) 
  lincom2 <- c(0,0,1,0,0,0,200,0)
  lincom3 <- c(0,0,0,1,0,0,0,250)
  
  rd1_yvar <- lincom1 %*% fit_coef
  rd1_yvar_se <- seq_along(1:10000) %>% map(~(
    lincom1 %*% MASS::mvrnorm(n = 1, mu = coefficients(fit), Sigma = fit_vcov) 
  )) %>% unlist() %>% sd()
  
  rd2_yvar <- lincom2 %*% fit_coef
  rd2_yvar_se <- seq_along(1:10000) %>% map(~(
    lincom2 %*% MASS::mvrnorm(n = 1, mu = coefficients(fit), Sigma = fit_vcov) 
  )) %>% unlist() %>% sd()
  
  rd3_yvar <- lincom3 %*% fit_coef
  rd3_yvar_se <- seq_along(1:10000) %>% map(~(
    lincom3 %*% MASS::mvrnorm(n = 1, mu = coefficients(fit), Sigma = fit_vcov) 
  )) %>% unlist() %>% sd()
  
  
  
  out <- list(coef = fit_coef, vcov = fit_vcov, fit = fit , rd1 = rd1_yvar, rd1_se = rd1_yvar_se, rd2 = rd2_yvar, rd2_se = rd2_yvar_se, rd3 = rd3_yvar, rd3_se = rd3_yvar_se)
  return(out)
}

get_WTP <- function(coef, 
                    coef_H, 
                    price = c(0,39,77,116), 
                    diff = c(11,19,29,31)) {
  
  debug = FALSE
  
  if (debug) {
    coef <- fit_share_Ins$coef
    coef_H <- fit_share_H$coef
    price = c(0,39,77,116)
    diff = c(11,19,29,31)
  }
  
  # Demand: Any Iny Insurance
  D_150_u <- c(c(1,0,0,0,150,0,0,0) %*% coef, price[1])
  D_150_l <- c(c(1,1,0,0,150,150,0,0) %*% coef , price[2])
  
  D_200_u <- c(c(1,1,0,0,200,200,0,0) %*% coef,price[2])
  D_200_l <- c(c(1,1,1,0,200,200,200,0) %*% coef,price[3])
  
  D_250_u <- c(c(1,1,1,0,250,250,250,0) %*% coef,price[3])
  D_250_l <- c(c(1,1,1,1,250,250,250,250) %*% coef,price[4])
  
  demand_est <- 
    rbind(D_150_u,D_150_l,D_200_u,D_200_l,D_250_u,D_250_l) %>% 
    data.frame() %>% 
    set_names(c("share","price")) %>% 
    rownames_to_column(var = "point") %>% 
    separate(point,into = c("fpl","two"), sep = "_u|_l") %>% 
    mutate(fpl = gsub("D_","",fpl)) %>% 
    mutate(fpl_group = c("135-150","150-200","150-200","200-250","200-250","250-300")) %>% 
    select(-two) %>% 
    select(fpl,fpl_group,share,price)  %>% 
    tbl_df() %>% 
    mutate(fpl = as.numeric(paste0(fpl))) %>% 
    select(-price) %>% 
    spread(fpl,share) %>% 
    set_names(c("fpl_group","fpl150","fpl200","fpl250"))  %>% 
    # Demand at 150% FPL 
    mutate(demand150 = fpl150) %>% 
    mutate(demand150 = ifelse(is.na(demand150),lag(fpl150)-lag(fpl200) + fpl200 ,demand150)) %>% 
    mutate(demand150 = ifelse(is.na(demand150),lag(fpl200)-lag(fpl250) + lag(fpl150,2)-lag(fpl200,2) + fpl250  ,demand150))  %>% 
    # Demand at 200% FPL
    mutate(demand200 = fpl200)  %>% 
    mutate(demand200 = ifelse(is.na(demand200),fpl150 + lead(fpl200) - lead(fpl150),demand200)) %>% 
    mutate(demand200 = ifelse(is.na(demand200), fpl250 + lag(fpl200) - lag(fpl250), demand200)) %>% 
    # Demand at 250% FPL
    mutate(demand250 = fpl250) %>% 
    mutate(demand250 = ifelse(is.na(demand250),lead(fpl250)-lead(fpl200)+fpl200,demand250)) %>% 
    mutate(demand250 = ifelse(is.na(demand250), lead(fpl200)-lead(fpl150)+fpl150, demand250)) %>% 
    select(fpl_group,starts_with("demand")) %>% 
    mutate(price = price) %>% 
    gather(group,demand,-price,-fpl_group) %>% 
    select(-fpl_group,fpl_group = group, price, demand) %>% 
    mutate(demand = pmax(0,pmin(1,demand))) %>% 
    rename(demand_L = demand)
  
  
  # Demand: High Cost Plan
  D_150_u <- c(c(1,0,0,0,150,0,0,0) %*% coef_H, diff[1])
  D_150_l <- c(c(1,1,0,0,150,150,0,0) %*% coef_H , diff[2])
  
  D_200_u <- c(c(1,1,0,0,200,200,0,0) %*% coef_H,diff[2])
  D_200_l <- c(c(1,1,1,0,200,200,200,0) %*% coef_H,diff[3])
  
  D_250_u <- c(c(1,1,1,0,250,250,250,0) %*% coef_H,diff[3])
  D_250_l <- c(c(1,1,1,1,250,250,250,250) %*% coef_H,diff[4])
  
  demand_est_H <- 
    rbind(D_150_u,D_150_l,D_200_u,D_200_l,D_250_u,D_250_l) %>% 
    data.frame() %>% 
    set_names(c("share","price")) %>% 
    rownames_to_column(var = "point") %>% 
    separate(point,into = c("fpl","two"), sep = "_u|_l") %>% 
    mutate(fpl = gsub("D_","",fpl)) %>% 
    mutate(fpl_group = c("135-150","150-200","150-200","200-250","200-250","250-300")) %>% 
    select(-two) %>% 
    select(fpl,fpl_group,share,price)  %>% 
    tbl_df() %>% 
    mutate(fpl = as.numeric(paste0(fpl))) %>% 
    select(-price) %>% 
    spread(fpl,share) %>% 
    set_names(c("fpl_group","fpl150","fpl200","fpl250"))  %>% 
    # Demand at 150% FPL 
    mutate(demand150 = fpl150) %>% 
    mutate(demand150 = ifelse(is.na(demand150),lag(fpl150)-lag(fpl200) + fpl200 ,demand150)) %>% 
    mutate(demand150 = ifelse(is.na(demand150),lag(fpl200)-lag(fpl250) + lag(fpl150,2)-lag(fpl200,2) + fpl250  ,demand150))  %>% 
    # Demand at 200% FPL
    mutate(demand200 = fpl200)  %>% 
    mutate(demand200 = ifelse(is.na(demand200),fpl150 + lead(fpl200) - lead(fpl150),demand200)) %>% 
    mutate(demand200 = ifelse(is.na(demand200), fpl250 + lag(fpl200) - lag(fpl250), demand200)) %>% 
    # Demand at 250% FPL
    mutate(demand250 = fpl250) %>% 
    mutate(demand250 = ifelse(is.na(demand250),lead(fpl250)-lead(fpl200)+fpl200,demand250)) %>% 
    mutate(demand250 = ifelse(is.na(demand250), lead(fpl200)-lead(fpl150)+fpl150, demand250)) %>% 
    select(fpl_group,starts_with("demand")) %>% 
    mutate(price = price,
           diff = diff) %>% 
    gather(group,demand,-price,-fpl_group,-diff) %>% 
    select(-fpl_group,fpl_group = group, price, diff,demand) %>% 
    mutate(demand = pmax(0,pmin(1,demand))) %>% 
    rename(demand_deltaHL = demand)
  
  
  foo <- 
    demand_est %>% 
    group_by(fpl_group) %>% 
    nest() %>% 
    rename(demand_L_est = data) %>% 
    mutate(demand_L_ext = 
             map(demand_L_est,~(
               data.frame(s = seq(0,1,0.01),
                          price = predict(lm(price ~ demand_L + I(demand_L^2) + I(demand_L^3), data = .x), newdata = data.frame(demand_L = seq(0,1,0.01)))) %>% 
                 mutate(price = pmax(0,price)) %>% 
                 tbl_df()
             ))) %>% 
    mutate(demand_L_fn = 
             map(demand_L_ext,
                 ~(approxfun(.x$s,.x$price)))) %>% 
    left_join(
      demand_est_H %>% group_by(fpl_group) %>% nest() %>% rename(demand_HL_est = data),
      "fpl_group"
    ) %>% 
    mutate(demand_H_est = 
             map2(demand_HL_est,demand_L_fn,~(
               data.frame(price = .y(.x$demand_deltaHL) + .x$diff)) %>% 
                 mutate(s = .x$demand_deltaHL)
             )) %>% 
    mutate(demand_H_ext = 
             map(demand_H_est,~(
               data.frame(s = seq(0,1,0.01),
                          price = predict(lm(price ~ s + I(s^2) + I(s^3), data = .x), 
                                          newdata = data.frame(s = seq(0,1,0.01)))) %>% 
                 mutate(price = pmax(0,price)) %>% 
                 tbl_df()
             ))) %>% 
    ungroup() %>% 
    mutate(fpl_group = gsub("demand","fpl",fpl_group))
  
  
  return(foo)
  
  
  
}


get_cost <- function(coef_cost, coef_D) {
  
  # Demand: Any Iny Insurance
  C_150_u <- c(c(1,0,0,0,150,0,0,0) %*% coef_cost, 
               c(1,0,0,0,150,0,0,0) %*% coef_D)
  C_150_l <- c(c(1,1,0,0,150,150,0,0) %*% coef_cost , 
               c(1,1,0,0,150,150,0,0) %*% coef_D)
  
  C_200_u <- c(c(1,1,0,0,200,200,0,0) %*% coef_cost,
               c(1,1,0,0,200,200,0,0) %*% coef_D)
  C_200_l <- c(c(1,1,1,0,200,200,200,0) %*% coef_cost,
               c(1,1,1,0,200,200,200,0) %*% coef_D)
  
  C_250_u <- c(c(1,1,1,0,250,250,250,0) %*% coef_cost,
               c(1,1,1,0,250,250,250,0) %*% coef_D)
  C_250_l <- c(c(1,1,1,1,250,250,250,250) %*% coef_cost,
               c(1,1,1,1,250,250,250,250) %*% coef_D)
  
  
  cost_est <- 
    rbind(C_150_u,C_150_l,C_200_u,C_200_l,C_250_u,C_250_l) %>% 
    data.frame() %>% 
    set_names(c("price","share")) %>% 
    rownames_to_column(var = "point") %>% 
    separate(point,into = c("fpl","two"), sep = "_u|_l") %>% 
    
    # This replicates Figure 11A in Finkelstein, Hendren and Shepard 
    # ggplot(aes(x = share, y = price, colour = fpl)) + geom_point() + ggthemes::theme_clean() + ylim(c(0,500)) + xlim(c(0.2,1))  + geom_line()
    
    mutate(fpl = gsub("C_","",fpl)) %>% 
    mutate(fpl_group = c("135-150","150-200","150-200","200-250","200-250","250-300")) %>%     
    select(-two) %>% 
    select(fpl,fpl_group,share,price)  %>% 
    tbl_df() %>% 
    mutate(fpl = as.numeric(paste0(fpl))) %>% 
    #select(-price) %>% 
    gather(key,value,-fpl,-fpl_group) %>% 
    mutate(key = paste0("fpl",fpl,"_",key)) %>% 
    select(-fpl) %>% 
    spread(key,value) %>% 
    select(fpl_group,fpl150_share,fpl200_share,fpl250_share,fpl150_price,fpl200_price,fpl250_price) %>% 
    # Price at 150% FPL
    mutate(adj_fpl150_price = fpl150_price) %>% 
    mutate(adj_fpl150_price = ifelse(is.na(adj_fpl150_price),lag(fpl150_price)-lag(fpl200_price) + fpl200_price ,adj_fpl150_price)) %>% 
    mutate(adj_fpl150_price = ifelse(is.na(adj_fpl150_price),lag(fpl200_price)-lag(fpl250_price) + lag(fpl150_price,2)-lag(fpl200_price,2) + fpl250_price  ,adj_fpl150_price)) %>% 
    # Price at 200% FPL
    mutate(adj_fpl200_price = fpl200_price)  %>% 
    mutate(adj_fpl200_price= ifelse(is.na(adj_fpl200_price),fpl150_price + lead(fpl200_price) - lead(fpl150_price),adj_fpl200_price)) %>% 
    mutate(adj_fpl200_price = ifelse(is.na(adj_fpl200_price), fpl250_price + lag(fpl200_price) - lag(fpl250_price), adj_fpl200_price)) %>% 
    # Price at 250% FPL
    mutate(adj_fpl250_price = fpl250_price) %>% 
    mutate(adj_fpl250_price = ifelse(is.na(adj_fpl250_price),lead(fpl250_price)-lead(fpl200_price)+fpl200_price,adj_fpl250_price)) %>% 
    mutate(adj_fpl250_price = ifelse(is.na(adj_fpl250_price), lead(fpl200_price)-lead(fpl150_price)+fpl150_price, adj_fpl250_price)) %>% 
    
    # Share Insured at 150% FPL
    mutate(adj_fpl150_share = fpl150_share) %>% 
    mutate(adj_fpl150_share = ifelse(is.na(adj_fpl150_share),lag(fpl150_share)-lag(fpl200_share) + fpl200_share ,adj_fpl150_share)) %>% 
    mutate(adj_fpl150_share = ifelse(is.na(adj_fpl150_share),lag(fpl200_share)-lag(fpl250_share) + lag(fpl150_share,2)-lag(fpl200_share,2) + fpl250_share  ,adj_fpl150_share)) %>% 
    # Share Insured at 200% FPL
    mutate(adj_fpl200_share = fpl200_share)  %>% 
    mutate(adj_fpl200_share= ifelse(is.na(adj_fpl200_share),fpl150_share + lead(fpl200_share) - lead(fpl150_share),adj_fpl200_share)) %>% 
    mutate(adj_fpl200_share = ifelse(is.na(adj_fpl200_share), fpl250_share + lag(fpl200_share) - lag(fpl250_share), adj_fpl200_share)) %>% 
    # Share Insured at 250% FPL
    mutate(adj_fpl250_share = fpl250_share) %>% 
    mutate(adj_fpl250_share = ifelse(is.na(adj_fpl250_share),lead(fpl250_share)-lead(fpl200_share)+fpl200_share,adj_fpl250_share)) %>% 
    mutate(adj_fpl250_share = ifelse(is.na(adj_fpl250_share), lead(fpl200_share)-lead(fpl150_share)+fpl150_share, adj_fpl250_share))  %>% 
    
    # # This replicates Figure 11B in Finkelstein, Hendren and Shepard
    # ggplot(aes(x = adj_fpl150_share, y = adj_fpl150_price)) + geom_point() + ggthemes::theme_clean() + ylim(c(0,500)) + xlim(c(0.2,1))  + geom_line() +
    #   geom_point(aes(x = fpl150_share, y = fpl150_price), colour = "darkblue") +
    #   geom_point(aes(x = fpl200_share, y = fpl200_price), colour = "darkred") +
    #   geom_point(aes(x = fpl250_share, y = fpl250_price), colour = "darkgreen") +
    #   geom_line(aes(x = fpl150_share, y = fpl150_price), colour = "darkblue") +
    #   geom_line(aes(x = fpl200_share, y = fpl200_price), colour = "darkred") +
    #   geom_line(aes(x = fpl250_share, y = fpl250_price), colour = "darkgreen")
    
    select(fpl_group,starts_with("adj")) %>% 
    gather(key,value,-fpl_group) %>% 
    mutate(var = ifelse(grepl("_price",key),"price","share")) %>% 
    separate(key,into=c("fpl","tmp"), sep = "_price|_share",remove = FALSE) %>% 
    select(fpl_group,fpl,var,value) %>% 
    spread(var,value) %>% 
    mutate(fpl = as.numeric(gsub("adj_fpl","",fpl))) %>% 
    
    # This replicates Figure 11B in Finkelstein, Hendren and Shepard  
    # ggplot(aes(x = share, y = price)) + geom_point() + facet_wrap(~fpl) + ggthemes::theme_clean() + ylim(c(0,500)) + xlim(c(0.2,1))  + geom_line() %>% 
    select(fpl_group = fpl, premium = price, share) %>% 
    arrange(fpl_group,share)
  
  
  foo <- 
    cost_est %>% 
    group_by(fpl_group) %>% 
    nest() %>% 
    rename( avg_cost_H_est = data) %>% 
    mutate(cost_H_est = map(avg_cost_H_est,~(
      .x %>% tbl_df() %>% 
        mutate(share2 = (share + lead(share))/2) %>% 
        mutate(premium2 = predict(lm(premium ~ share + I(share^2) + I(share^3), data = .x), newdata = data.frame(share = (.x$share + lead(.x$share))/2))) %>% 
        mutate(premium_delta = predict(lm(premium ~ share + I(share^2) + I(share^3), data = .x), newdata = data.frame(share = .01 + (.x$share + lead(.x$share))/2))) %>% 
        select(share = share2, premium = premium2,premium_delta) %>% 
        na.omit() 
      #mutate(total_cost = share * premium) #%>% 
      #mutate(cht_total_cost = total_cost-lag(total_cost)) %>% 
      #mutate(cost = (total_cost-lag(total_cost))/(share-lag(share)))
    ))) %>% 
    # Cost Curve for High Plan (full curve)
    mutate(cost_H_ext = 
             map(avg_cost_H_est,~(
               data.frame(share = seq(0,1,0.01),
                          premium = predict(lm(premium ~ share + I(share^2) + I(share^3), data = .x), newdata = data.frame(share = seq(0,1,0.01)))) %>% 
                 mutate(premium = pmax(0,premium)) %>% 
                 tbl_df() %>% 
                 mutate(total_cost = share * premium) %>% 
                 mutate(cht_total_cost = total_cost-lag(total_cost)) %>% 
                 mutate(cost = (total_cost-lag(total_cost))/(share-lag(share)))
             ))) %>% 
    # Points from Finkelstein, Hendren and Shepard
    mutate(cost_H_est2 = 
             map2(cost_H_est, cost_H_ext, ~(
               .x %>% 
                 mutate(share = paste0(round(share,2))) %>% 
                 inner_join(.y  %>% mutate(share = paste0(round(share,2))),"share") %>% 
                 select(share,premium = premium.x, cost) %>% 
                 mutate(share = as.numeric(paste0(share)))
             )))
  
  return(foo)
}


fit_wtp_and_costs <- function(coef_wtp_l, coef_wtp_hl, coef_cost_h, coef_cost_demand,
                              fit_fmla = as.formula("Y ~ wtp + I(wtp^2) + I(wtp^3)"),
                              output_type = "predicted"
) {
  
  # Estimate WTP
  est_wtp <- 
    get_WTP(coef = coef_wtp_l,
            coef_H = coef_wtp_hl)
  # Estimate Cost
  est_cost <- 
    get_cost(coef_cost = coef_cost_h,
             coef_D  = coef_cost_demand)
  
  # Extrapolated WTP
  df_est_wtp <- 
    est_wtp %>% 
    select(fpl_group,demand_L_ext,demand_H_ext) %>% 
    unnest() %>% 
    select(fpl_group,s,price_L = price, price_H = price1) %>% 
    gather(type,price,-fpl_group,-s) %>% 
    mutate(type = gsub("price_","",type)) %>% 
    select(fpl_group , type, s , wtp = price)
  
  # Extrapolated Costs
  df_est_cost <- 
    est_cost %>% 
    pluck("cost_H_ext") %>% 
    set_names(c("fpl150","fpl200","fpl250")) %>% 
    bind_rows(.id  = "fpl_group") %>% 
    select(fpl_group, s = share, cost,avg_cost = premium) %>% 
    mutate(type = "H") %>% 
    na.omit() %>% 
    select(fpl_group, type, s, cost, avg_cost)
  
  df_est <- 
    df_est_wtp %>% 
    left_join(df_est_cost,c("fpl_group","type","s"))
  
  if (output_type == "predicted") {
    df_fit <- 
      df_est %>% 
      ungroup() %>% 
      tbl_df()  %>% 
      group_by(type) %>% 
      mutate(fpl = as.numeric(gsub("fpl","",paste0(fpl_group)))) %>% 
      select(type, s, wtp, cost, fpl) %>% 
      gather(outcome,Y,-type,-wtp,-fpl) %>% 
      group_by(type,outcome,fpl) %>% 
      na.omit() %>% 
      nest() %>% 
      mutate(coef = map(data,~(
        do(.x, lm(fit_fmla, data = .) %>% broom::tidy() %>% 
             select(-statistic,-p.value,-std.error) %>% 
             rename(beta = estimate)  %>% 
             spread(term,beta) %>% 
             janitor::clean_names()
        ) 
      ))) %>% 
      # mutate(vcov = map(data,~(
      #   do(.x, lm(fit_fmla, data = .) %>% vcov() %>% data.frame())
      # ))) %>% 
      select(-data) %>% 
      unnest()
    return(df_fit)
  } else {
    return(df_est)
  }
  
}
