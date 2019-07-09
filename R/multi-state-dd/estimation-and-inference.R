source("~/Dropbox/Projects/tidyri/R/ri-permute.R")
get_DD_hat <- function(ff) {
  DD_hat <- factor_levs %>%
    map(~(
      # First difference is treated_post - treated_pre
      (predict(ff, newdata = mf %>% mutate(!!quo_name(tx) := 1, baseline_outcome = factor(.x, levels = levels(mf$baseline_outcome)), post = 1)) -
         predict(ff, newdata = mf %>% mutate(!!quo_name(tx) := 1, baseline_outcome = factor(.x, levels = levels(mf$baseline_outcome)), post = 0))) -
        
        # second difference is control_post - control_pre
        (predict(ff, newdata = mf %>% mutate(!!quo_name(tx) := 0, baseline_outcome = factor(.x, levels = levels(mf$baseline_outcome)), post = 1)) -
           predict(ff, newdata = mf %>% mutate(!!quo_name(tx) := 0, baseline_outcome = factor(.x, levels = levels(mf$baseline_outcome)), post = 0)))
    )) %>%
    map(~summarise_all(tbl_df(.), function(x) mean(x, na.rm = TRUE))) %>%
    set_names(factor_levs) 
  
  DD_hat %>% 
    bind_rows() %>%
    mutate_at(vars(1:4),function(x) round(x,4)) %>% 
    as.matrix() %>% 
    data.frame() %>% mutate(from = colnames(.)) %>% gather(to,estimate,-from) %>% 
    unite(term,c("from","to"),sep= "_TO_")
}

estimate_ri <- function(df,fn = lm,  fmla = as.formula(Y~Z), ...) {
  
  fit_obs <- fn(fmla, data = df)
  DD_obs <- get_DD_hat(fit_obs)  %>% 
    rename(obs_estimate = estimate)
  
  mf_p <- 
    df %>% 
    ri_permute(...)
  
  fit_ri <-  mf_p %>% 
    do(fit = fn(fmla, data = .)) 
  
  DD_ri <- 
    fit_ri %>% 
    ungroup() %>% 
    mutate(DD = future_map(fit,~(get_DD_hat(.x)),.progress=TRUE)) %>% 
    select(replicate,DD) %>% 
    unnest() %>% 
    left_join(DD_obs,"term")
  
  ri_p  <- 
    DD_ri  %>% 
    mutate(obs_gt_est = as.integer(abs(obs_estimate) < abs(estimate))) %>% 
    group_by(term) %>% 
    summarise(ri_p = mean(obs_gt_est)) 
  
  df_fit_obs_final <- 
    DD_obs %>% 
    left_join(ri_p,"term") 
  
  out <- list(fit = df_fit_obs_final, ri = DD_ri, df = df )
  
  return(out) 
}
library(furrr)
plan(multiprocess)
dd_estimates = estimate_ri(df = mf, 
                   fmla = transitions_model_formula,
                   m = 20, treatment = expansion_state, cluster = state, idvar = idnumber,  outcome = insurance_01_esi)

# dd_estimates %>% 
#   write_rds(here("output/dd-estimates/sipp-dd-estimates-with-inference.rds"))

dd_estimates$ri %>% ggplot(aes(x = estimate)) + geom_density() + facet_wrap(~term) + 
  theme_bw() +
  geom_vline(data= test$fit %>% mutate(col = ifelse(ri_p<0.05,"p<0.05","")),
             aes(xintercept = obs_estimate,colour = col),lty=2) +
  scale_colour_manual(values = c("grey","black")) +
  theme(legend.position ="none")


