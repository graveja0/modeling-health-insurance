mult_multivar_fmla <- 
  as.formula("cbind(insurance_01_esi, insurance_02_priv_oth, insurance_03_public,insurance_04_uninsured) ~  expansion_state * post ")

# Fit the model
f.mm_marginal <- lm(mult_multivar_fmla, data = mf) 

DD_estimates_marginal <- 
  f.mm_marginal %>% broom::tidy() %>% filter(term=="expansion_state:post") %>% 
  select(estimate) %>%  t() 

DD_marginal_lpm <- 
  # First difference is treated_post - treated_pre
  (predict(f.mm_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 1, post = 1)) -
     predict(f.mm_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 1,  post = 0))) -
  
  # second difference is control_post - control_pre
  (predict(f.mm_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 0, post = 1)) -
     predict(f.mm_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 0,  post = 0))) %>% 
  tbl_df() 

mlogit_fmla <-
  as.formula("insurance ~  expansion_state * post ")

f.mlogit_marginal <- nnet::multinom(mlogit_fmla, data = mf, trace = FALSE, MaxNWts = 4000)

DD_marginal_mlogit <- 
  # First difference is treated_post - treated_pre
  (predict(f.mlogit_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 1, post = 1), type = "probs") -
     predict(f.mlogit_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 1,  post = 0), type = "probs")) -
  
  # second difference is control_post - control_pre
  (predict(f.mlogit_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 0, post = 1), type = "probs") -
     predict(f.mlogit_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 0,  post = 0), type = "probs")) %>% 
  tbl_df() 

compare_lpm_mlogit <- 
  rbind(as.matrix(DD_marginal_lpm[1,]) ,as.matrix(DD_marginal_mlogit[1,])) %>% t() %>%
  magrittr::set_rownames(insurance_lut) %>% as.data.frame() %>% rownames_to_column() 

compare_lpm_mlogit <- compare_lpm_mlogit %>% tbl_df() %>% set_names(c("category","Multiple Multivariate","Multinomial Logit")) %>% 
  mutate_at(vars(2,3),function(x) round(x,3))  %>% 
  mutate(category =  c("Employer-Sponsored Insurance","Other Private Insurance","Public Insurance","Uninsured")) %>% arrange(category)

# Use State and Year Fixed Effects

mult_multivar_fmla <- 
  as.formula("cbind(insurance_01_esi, insurance_02_priv_oth, insurance_03_public,insurance_04_uninsured) ~  state + expansion_state * post ")

# Fit the model
f.mm_marginal <- lm(mult_multivar_fmla, data = mf) 

DD_estimates_marginal <- 
  f.mm_marginal %>% broom::tidy() %>% filter(term=="expansion_state:post") %>% 
  select(estimate) %>%  t() 

DD_marginal_lpm <- 
  # First difference is treated_post - treated_pre
  (predict(f.mm_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 1, post = 1)) -
     predict(f.mm_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 1,  post = 0))) -
  
  # second difference is control_post - control_pre
  (predict(f.mm_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 0, post = 1)) -
     predict(f.mm_marginal, newdata = mf %>% mutate(!!quo_name(tx) := 0,  post = 0))) %>% 
  tbl_df() 


DD_marginal_lpm <- DD_marginal_lpm %>%  summarise_all(funs(mean))
