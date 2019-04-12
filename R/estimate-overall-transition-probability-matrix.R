
create_sipp_data = FALSE
if (create_sipp_data) source(here("R/read-and-tidy-SIPP-data.R"))

df_sipp_full <- read_rds(here("input/sipp/01_sipp-tidy_v1-0.rds"))

# Survey weight (use value from first month)
df_w <- 
  df_sipp_full %>% 
  mutate(idnumber = id) %>% 
  group_by(idnumber) %>% 
  mutate(year = ifelse(swave ==1 , 2014, 2015)) %>% 
  mutate(month = ifelse(swave ==1 , monthcode, monthcode+12)) %>% 
  filter(month==1) %>% 
  select(idnumber,weight = wpfinwgt) %>% 
  ungroup() 

df_sipp <- 
  df_sipp_full %>% 
  filter(age < 65 & age > 18) %>% 
  mutate(year = ifelse(swave ==1 , 2014, 2015)) %>% 
  mutate(month = ifelse(swave ==1 , monthcode, monthcode+12)) %>% 
  mutate(idnumber = id) %>% 
  mutate(insurance_type = factor(hicov, labels = insurance_sipp_lut)) %>% 
  select(idnumber,month,insurance_type,expansion_state, sex, race, state)  %>% 
  # !!!! Note expansion state is time-varying if someone moves. 
  group_by(idnumber) %>% 
  mutate(expansion_state = max(expansion_state)) %>% 
  ungroup()  %>% 
  left_join(df_w,"idnumber")

ex_ante <- 
  df_sipp %>% 
  filter(month==1) %>% 
  group_by(insurance_type) %>% 
  summarise(n = sum(weight,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(pct = n/sum(n)) 

ex_ante %>%  
  write_rds(here("output/ex-ante-overall-population/ex-ante-distribution.rds"))

# Set Up the Multi-State Data

ls_ms <- 
  df_sipp %>% 
  mutate(insurance_type = paste0(insurance_type)) %>% 
  mutate(constant = 1) %>% 
  prepare_multistate_data(idvar = idnumber,
                          timevar = month, 
                          statevar = insurance_type) 

categories <- names(ls_ms$trans_mat)

fit_transition <- map(categories,
                     ~ (
                       fit_multistate_model(
                         df = ls_ms$df_ms %>% pluck(.x),
                         tmat = ls_ms$trans_mat %>% pluck(.x),
                         fit_type = "kaplan-meier",
                         ff = Surv(Tstart, Tstop, status) ~ 1,
                         idvar = idnumber,
                         prediction_vals = data.frame(constant = 1)
                       ) 
                     )) %>% 
  set_names(categories)

fit_transition_gom <- map(categories,
                      ~ (
                        fit_multistate_model(
                          df = ls_ms$df_ms %>% pluck(.x),
                          tmat = ls_ms$trans_mat %>% pluck(.x),
                          fit_type = "gompertz",
                          ff = Surv(Tstart, Tstop, status) ~ constant,
                          idvar = idnumber,
                          prediction_vals = data.frame(constant = 1)
                        ) 
                      )) %>% 
  set_names(categories)

trans_probs <- map_df(categories,
                      ~(
                        get_cumHaz(dist = fit_transition[[1]]$fit_type ,
                                   ls_fit = fit_transition %>% pluck(.x), 
                                   tt = 1:24,
                                   lut = fit_transition %>% 
                                     pluck(.x) %>% pluck("lut"))  %>% 
                          mssample(Haz=.,trans=ls_ms$trans_mat %>% 
                                     pluck(.x),tvec=unique(.$time),clock="reset", M=1000) %>% 
                          magrittr::set_names(c("time",.x,fit_transition %>% pluck(.x) %>% 
                                                  pluck("lut") %>% pull(transition_type))) %>% 
                          mutate(baseline = .x)
                      )) %>% 
  arrange(time,baseline) %>% 
  select_at(c("time","baseline",categories)) %>% 
  group_by(time) %>% 
  nest()

trans_probs %>% 
  write_rds(here("output/ex-ante-overall-population/transition-probabilities-kaplan-meier.rds"))

trans_probs_gom <- map_df(categories,
                      ~(
                        get_cumHaz(dist = fit_transition_gom[[1]]$fit_type ,
                                   ls_fit = fit_transition_gom %>% pluck(.x), 
                                   tt = 1:24,
                                   lut = fit_transition_gom %>% 
                                     pluck(.x) %>% pluck("lut"))  %>% 
                          mssample(Haz=.,trans=ls_ms$trans_mat %>% 
                                     pluck(.x),tvec=unique(.$time),clock="reset", M=1000) %>% 
                          magrittr::set_names(c("time",.x,fit_transition_gom %>% pluck(.x) %>% 
                                                  pluck("lut") %>% pull(transition_type))) %>% 
                          mutate(baseline = .x)
                      )) %>% 
  arrange(time,baseline) %>% 
  select_at(c("time","baseline",categories)) %>% 
  group_by(time) %>% 
  nest()

trans_probs_gom %>% 
  write_rds(here("output/ex-ante-overall-population/transition-probabilities-gompertz.rds"))



### MEPS

insurance_full_lut <- 
  c("Public" = "public",
    "ESI-Own" = "esiown",
    "ESI-Dependent" = "esidep",
    "Non-Group" = "nongrp",
    "Uninsured" = "uninsured")

df_meps <-
  read_rds("./input/meps/df-meps-long-2014-to-2016.rds") %>% 
  filter(panel==19) %>% 
  mutate(insurance = insurance_full_lut[paste0(insurance)]) %>% 
  group_by(dupersid) %>% 
  mutate(age = min(age)) %>% 
  mutate(tx = as.integer(region==3)) %>% 
  select(dupersid,tx,age,month,insurance) %>% 
  rename(insurance_full = insurance) %>% 
  mutate(insurance = forcats::fct_collapse(insurance_full,
                                           esi = c("esidep","esiown"), 
                                           private = c("nongrp"),
                                           public = c("public"), 
                                           uninsured = c("uninsured"))) %>% 
  mutate(insurance  = ifelse(insurance == "esi","01_esi", 
                             ifelse(insurance == "private","02_priv_oth",
                                    ifelse(insurance == "public","03_public",
                                           ifelse(insurance=="uninsured","04_uninsured","")))))

ls_ms_meps <- 
  df_meps %>% 
  select(-insurance_full) %>% 
  prepare_multistate_data(idvar = dupersid,
                          timevar = month, 
                          statevar = insurance) 

categories <- names(ls_ms_meps$trans_mat)

# Fit the transition models. 
fit_transition_meps <- map(categories,
                      ~ (
                        fit_multistate_model(
                          df = ls_ms_meps$df_ms %>% pluck(.x),
                          tmat = ls_ms_meps$trans_mat %>% pluck(.x),
                          fit_type = "kaplan-meier",
                          ff = Surv(Tstart, Tstop, status) ~ 1,
                          idvar = dupersid,
                          prediction_vals = data.frame(constant = c(1))
                        ) 
                      )) %>% 
  set_names(categories)


