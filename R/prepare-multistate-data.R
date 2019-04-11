prepare_multistate_data <- function(df, timevar, statevar, idvar) {
  tt <- enquo(timevar)
  ss <- enquo(statevar)
  id <- enquo(idvar)
  
  t0 <- df %>% summarise(min = min(!!tt,na.rm=TRUE))
  
  wide_vars <- c(names(df)[-which(names(df) %in% c(quo_name(tt),quo_name(ss)))],"baseline","max_time")
  
  df_wide <- 
    df %>% 
    group_by(!!id) %>% 
    mutate(max_time = max(!!tt)) %>% 
    mutate(!!quo_name(tt) := paste0("tmp_",!!tt)) %>% 
    arrange(!!id,!!tt) %>%
    ungroup() %>% 
    spread(!!tt,!!ss) %>% 
    mutate(baseline = tmp_1) %>% 
    gather(!!tt,!!ss,-wide_vars) %>% 
    filter(!is.na(baseline)) %>% 
    mutate(!!quo_name(tt) := as.numeric(gsub("tmp_","",!!tt))) %>% 
    arrange(!!id,!!tt) %>% 
    group_by(!!id) %>% 
    mutate(transition = as.integer(baseline != !!ss & !is.na(!!ss))) %>%
    mutate(censored = as.integer(is.na(!!ss))) %>%
    mutate(admin_censor = as.integer(max(transition)==0 & max(censored==0) & row_number()==n())) %>%
    mutate(admin_censor = ifelse(censored==1,0,admin_censor)) %>%
    mutate(type = 1 * max(transition==1) + 2 * max(censored==1) + 3 * (max(admin_censor==1) & max(censored)!=1)) %>%
    filter((admin_censor==1 & type == 3) | (type == 1 & transition==1) | (type==2 & censored==1)) %>% 
    filter(row_number()==1) %>% 
    ungroup() 
  
  transition_types <- df_wide %>% pull(baseline) %>% unique() %>% sort()

  df_wide <- 
    transition_types %>% 
    map(~(
      df_wide %>% mutate(time_to_event = ifelse(transition==1 & !!ss==.x ,!!tt,
                                          ifelse(censored==1,!!tt,max_time))) %>% 
        mutate(status = ifelse(transition==1 & !!ss==.x,1,0)) %>% 
        select(time_to_event,status) %>% 
        set_names(c(paste0("time_",.x),paste0("status_",.x)))
    )) %>% bind_cols() %>% 
    bind_cols(df_wide,.)
  
 

  ls_tmat <- 
    transition_types %>% 
    map(~(
      matrix(t(matrix(c(NA,1:(length(transition_types)-1), rep(rep(NA,length(transition_types)),length(transition_types)-1)),nrow= length(transition_types),ncol= length(transition_types))),
        nrow = length(transition_types),ncol= length(transition_types),
             dimnames = list(from = c(.x,transition_types[-which(transition_types==.x)]), to=c(.x,transition_types[-which(transition_types==.x)]))
               ) 
    )) %>% 
    set_names(transition_types)
  
  ls_ms <- list()

  for (.x in transition_types) {

    tmat <- ls_tmat[[.x]]
    
    df_times <- df_wide %>% filter(baseline==.x) %>% select_at(vars(paste0("time_",colnames(tmat)))) %>% 
      mutate_at(vars(paste0("time_",.x)),function(x) x = NA) %>% data.frame()
    
    df_status <-  df_wide %>% filter(baseline==.x) %>% select_at(vars(paste0("status_",colnames(tmat)))) %>% 
      mutate_at(vars(paste0("status_",.x)),function(x) x = NA) %>% data.frame()
    
    df_covars <- df_wide %>% filter(baseline==.x) %>% select_at(vars(setdiff(wide_vars,c("max_time","baseline")))) %>% 
      data.frame()
    
    df_id <- df_wide %>% filter(baseline==.x) %>% select(!!id) %>% data.frame()
  
    df_ms <- msprep(time = df_times, status = df_status, keep = df_covars, trans = tmat) 
    
    covs <- setdiff(wide_vars,c("max_time","baseline",quo_name(id)))
    df_ms <- expand.covs(df_ms,covs, append = TRUE)
    
    ls_ms[[.x]] = df_ms
  }

  out <- list(df_ms = ls_ms, trans_mat = ls_tmat)
  return(out)
}





# covs <- c("tx")
# tt = 1:24
# ntrans = 4
# models <- 1:ntrans %>% 
#   map(~(
#     phreg(Surv(Tstart,Tstop,status)~tx,dist="weibull",data = ls_ms$df_ms[["public"]] %>% filter(trans==.x))
#   ))
# coeffs <- models %>% map(~(.x$coefficients))
# lp <- models %>% map(~(.x$coeff %*% c(0,0,0)))
# cumHaz <- 1:ntrans %>% 
#   map(~(
#     exp(-exp(coeffs[[.x]][length(covs)+2]) * coeffs[[.x]][length(covs)+1] + lp[[.x]]) * tt^exp(coeffs[[.x]][length(covs)+2])
#   ))
# Haz<-unlist(cumHaz)
# newtrans<-rep(1:ntrans,each=length(tt))
# time<-rep(tt,ntrans)
# Haz<-cbind(time=as.vector(time),Haz=as.vector(Haz),trans=as.vector(newtrans))
# Haz<-as.data.frame(Haz)
# stateprobs <- mssample(Haz=Haz,trans=ls_tmat[["public"]],tvec=tt,clock="reset", M=1000) %>% 
#   set_names(c("time",colnames(ls_tmat[["public"]])))
# stateprobs %>% filter(time==24) 
# tmp_fmla <- as.formula(paste0("tmp~",paste0(colnames(model.frame(ff,data = df)),collapse="+")))
# tmp_fit <- lm(tmp_fmla,data = X) 
# X <- expand.model.frame(tmp_fit,quo_name(id)) %>% 
#   select(-tmp)
