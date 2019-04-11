
get_cumHaz <- function(ls_fit,tt,lut,dist = "weibull") {
  km_cumHaz <- function(surv) -log(surv)
  weibull_cumHaz <- function(shape,scale,lp,tt) apply(lp,1,function(x) exp(-exp(shape)*scale + x) * tt ^ exp(shape))
  exp_cumHaz <- function(lp,scale,tt) apply(lp,1,function(x) exp(-scale+x)*tt)
  gompertz_cumHaz <- function(lp,rate,loglev,tt) apply(lp,1,function(x) exp(loglev + x)*(1/rate)*(exp(rate*tt)-1))
  loglogistic_cumHaz <- function(tt,lp,shape,scale) apply(lp,1,function(x) -log(1/(1+(exp(-(scale-x))*tt)^(1/(exp(-shape))))))
  lognormal_cumHaz <- function(tt,lp,shape,scale) apply(lp,1,function(x) -log(1-pnorm((log(tt)-(scale-x))/(exp(-shape)))))
  gengamma_cumHaz <- function(kappa,mu,sigma,gamma,tt) {
    z <- rep(0,length(tt))
    z <- sign(kappa)*((log(tt)-mu)/sigma)
    u <- gamma * exp((abs(kappa))*z)
    cumHaz <- ifelse(kappa>0, -log(1-pgamma(u,gamma)),
                     ifelse(kappa==0, -log(1-pnrom(z)),
                            ifelse(kappa<0,-log(pgamma(u,gamma)),NA)))
    return(cumHaz)
  }
  
  # z[[i]]<-rep(0,length(tt))
  # z[[i]]<- sign(kappa[[i]])*((log(tt)-mu[[i]])/sigma[[i]])
  # u[[i]]<-gamma[[i]]*exp((abs(kappa[[i]]))*z[[i]])
  # if(kappa[[i]]>0){
  #   cumHaz[[i]]<--log(1-pgamma(u[[i]],gamma[[i]]))
  # }
  # if(kappa[[i]]==0){
  #   cumHaz[[i]]<--log(1-pnorm(z[[i]]))
  # }
  # if(kappa[[i]]<0){
  #   cumHaz[[i]]<--log(pgamma(u[[i]],gamma[[i]]))
  # }
  # 
  
  times <- tt
  
  transitions = lut$transition_type
  if (dist == "kaplan-meier") {
    cumHaz = transitions %>% map(~(-log(ls_fit$lp[[.x]]$surv) %>% 
                                     data.frame() %>% set_names("cumHaz") %>% 
                                     tbl_df() %>% mutate(time = ls_fit$lp[[.x]]$time) %>% 
                                     full_join(cbind.data.frame(time = times,cumHaz100=0),"time") %>% 
                                     arrange(time) %>% 
                                    
                                     mutate(to_fill = !is.na(zoo::na.locf(cumHaz, fromLast = FALSE, na.rm = FALSE)),
                                            value = if_else(to_fill,
                                                             zoo::na.locf(cumHaz, na.rm = FALSE),
                                                             pmax(first(cumHaz), zoo::na.locf(cumHaz, na.rm = FALSE))),
                                     ) %>% 
                                     
                                   
                                   
                                     mutate(cumHaz = value) %>% 
                                     arrange(time) %>% 
                                     select(-cumHaz100,-to_fill,-value) %>% 
                                     data.frame()  %>% 
                                     mutate(cumHaz = ifelse(is.na(cumHaz),0,cumHaz))
                                   )) %>% 
      set_names(transitions) 
   
  } else if (dist == "weibull") {
    est_shape <- transitions %>% map(~ls_fit$params[[.x]][["log(shape)"]]) %>% set_names(transitions)
    est_scale <- transitions %>% map(~ls_fit$params[[.x]][["log(scale)"]]) %>% set_names(transitions)
    cumHaz <- transitions %>% map(~weibull_cumHaz(shape = est_shape[[.x]], scale = est_scale[[.x]], lp = ls_fit$lp[[.x]],tt=times) %>% 
                                    data.frame() %>% set_names("cumHaz") %>% 
                                    tbl_df() %>% mutate(time = times) %>% select(time,everything())) %>% 
      set_names(transitions)  
  } else if (dist == "exp") {
    est_scale <- transitions %>% map(~ls_fit$params[[.x]][["log(scale)"]]) %>% set_names(transitions)
    cumHaz <- transitions %>% map(~exp_cumHaz(scale = est_scale[[.x]], lp = ls_fit$lp[[.x]],tt=times) %>% 
                                    data.frame() %>% set_names("cumHaz")  %>% 
                                    tbl_df() %>% mutate(time = times) %>% select(time,everything())) %>% 
      set_names(transitions)  
  } else if (dist == "gompertz") {
    est_rate <- transitions %>% map(~ls_fit$params[[.x]][["rate"]]) %>% set_names(transitions)
    est_loglev <- transitions %>% map(~ls_fit$params[[.x]][["log(level)"]]) %>% set_names(transitions)
    cumHaz <- transitions %>% map(~gompertz_cumHaz(rate = est_rate[[.x]], loglev = est_loglev[[.x]],
                                                   lp = ls_fit$lp[[.x]],
                                                   tt=times) %>% 
                                    data.frame() %>% set_names("cumHaz") %>% 
                                    tbl_df() %>% mutate(time = times) %>% select(time,everything())) %>% 
      set_names(transitions) 
  } else if (dist == "loglogistic") {
    est_shape <- transitions %>% map(~ls_fit$params[[.x]][["log(shape)"]]) %>% set_names(transitions)
    est_scale <- transitions %>% map(~ls_fit$params[[.x]][["log(scale)"]]) %>% set_names(transitions)
    cumHaz <- transitions %>% map(~loglogistic_cumHaz(shape = est_shape[[.x]], 
                                                      scale = est_scale[[.x]], lp = ls_fit$lp[[.x]],tt=times) %>% 
                                    data.frame() %>% set_names("cumHaz") %>% 
                                    tbl_df() %>% mutate(time = times) %>% select(time,everything())) %>% 
      set_names(transitions)  
  } else if (dist == "lognormal") {
    est_shape <- transitions %>% map(~ls_fit$params[[.x]][["log(shape)"]]) %>% set_names(transitions)
    est_scale <- transitions %>% map(~ls_fit$params[[.x]][["log(scale)"]]) %>% set_names(transitions)
    cumHaz <- transitions %>% map(~lognormal_cumHaz(shape = est_shape[[.x]], 
                                                    scale = est_scale[[.x]], lp = ls_fit$lp[[.x]],tt=times) %>% 
                                    data.frame() %>% set_names("cumHaz") %>% 
                                    tbl_df() %>% mutate(time = times) %>% select(time,everything())) %>% 
      set_names(transitions)  
  } else if (dist == "gengamma") {
    est_kappa = ls_fit$kappa
    est_gamma <- ls_fit$gamma
    est_mu <- ls_fit$mu
    est_sigma <- ls_fit$sigma
    
    cumHaz <- transitions %>% map(~gengamma_cumHaz(kappa = est_kappa[[.x]], 
                                                   gamma = est_gamma[[.x]], 
                                                   mu  = est_mu[[.x]], 
                                                   sigma = est_sigma[[.x]], 
                                                   tt=times) %>% 
                                    data.frame() %>% set_names("cumHaz") %>% 
                                    mutate(time = times) %>% select(time,everything())) %>% 
      set_names(transitions)  
    
  }
  

  
  
  out <- cumHaz %>% bind_rows(.id = "transition") %>% select(time,Haz = cumHaz,transition_type = transition) %>% 
    left_join(lut,c("transition_type")) %>% select(time,Haz,trans=transition_num) %>% data.frame()
  return(out)
}
