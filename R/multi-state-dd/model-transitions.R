#########################
# Model the Transitions
#########################

marginal <-
  df %>% mutate(tx = !!tx) %>%
  select(-!!tx) %>%
  group_by(tx) %>%
  count(!!outcome_baseline) %>%
  rename(hhf1 = !!outcome_baseline ) %>%
  reshape2::dcast(tx~hhf1,value.var = "n") %>%
  group_by(tx) %>%
  gather(key,value,-tx) %>%
  mutate(sum = sum(value,na.rm=TRUE)) %>%
  mutate(value = value / sum) %>%
  reshape2::dcast(tx+sum~key,value.var = "value") %>%
  ungroup() %>%
  group_by(tx) %>%
  select(-sum) %>% set_names(c("tx",insurance_lut)) %>%
  ungroup()


# These are used later in the calculations
# Vector of ex ante (i.e., pre-expansion) marginal distribution for treated
M_Tx_ExA <- marginal %>% filter(tx == 1) %>% select(-1) %>% as.matrix()
#M_Tx_ExA <- c(M_Tx_ExA, 0) # Include death 

M_Cx_ExA <- marginal %>% filter(tx == 0) %>% select(-1) %>% as.matrix()
#M_Cx_ExA <- c(M_Cx_ExA, 0) # Include death 

# Ex Post
marginal_exp <-
  df %>% mutate(tx = !!tx) %>%
  select(-!!tx) %>%
  group_by(tx) %>%
  count(!!outcome_fu) %>%
  rename(hlhf3 = !!outcome_fu ) %>%
  spread(hlhf3,n) %>% 
  #reshape2::dcast(tx~hhf3,value.var = "n") %>%
  group_by(tx) %>%
  gather(key,value,-tx) %>%
  mutate(sum = sum(value,na.rm=TRUE)) %>%
  mutate(value = value / sum) %>%
  reshape2::dcast(tx+sum~key,value.var = "value") %>%
  ungroup() %>%
  group_by(tx) %>%
  select(-sum) %>% set_names(c("tx",insurance_lut)) %>%
  ungroup()

# Marginal distribution ex post treatment group.
M_Tx_ExP <- marginal_exp %>% filter(tx == 1) %>% select(-1) %>% as.matrix()


# Unadjusted Difference-in-Differences Transtion Matrix
transitions <-
  df %>%
  filter(idnumber %in% mf$idnumber) %>%
  mutate(
    Y_f0 = !!outcome_baseline,
    Y_f1 = !!outcome_fu,
    Tx = !!tx
  ) %>%
  count(Y_f0, Y_f1, Tx) %>%
  group_by(Y_f0, Tx) %>%
  mutate(n = n / sum(n)) %>%
  rename(pct = n) %>%
  gather(key, value, -Y_f0, -Y_f1, -Tx) %>%
  unite(key, key, Y_f1) %>%
  spread(key, value) %>%
  ungroup() 

tx_transitions <- 
  transitions %>% 
  filter(Tx==1) %>% 
  mutate_at(vars(3:6),function(x) round(x*100,2)) %>% 
  select(-Tx) %>% 
  mutate(Y_f0 = insurance_lut) %>% 
  magrittr::set_colnames(c("",insurance_lut)) 

cx_transitions <- 
  transitions %>% 
  filter(Tx==0) %>% 
  mutate_at(vars(3:6),function(x) round(x*100,2)) %>% 
  select(-Tx) %>% 
  mutate(Y_f0 = insurance_lut) %>% 
  magrittr::set_colnames(c("",insurance_lut)) 


# Observed Transition Matrix: Treated Units
T_Tx <- transitions %>% filter(Tx==1) %>% select(contains("pct")) %>% as.matrix()

T_Cx  <- transitions %>% filter(Tx==0) %>% select(contains("pct")) %>% as.matrix()


sigfig <- function(vec, n=3) formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 

sM_Tx_ExA <- sigfig(M_Tx_ExA,2) %>% gsub("^0$","0.0",.)
sM_Cx_ExA <- sigfig(M_Cx_ExA,2) %>% gsub("^0$","0.0",.)
sT_Tx <- apply(T_Tx,1,sigfig,2) %>% t() %>% apply(.,2,function(x) gsub("^0$","0.0",x))
sT_Cx <- apply(T_Cx,1,sigfig,2) %>% t() %>% apply(.,2,function(x) gsub("^0$","0.0",x))

M_Tx_ExA %*% T_Tx %>% rbind(M_Tx_ExP) %>% 
  tbl_df() %>% 
  mutate(type = c("Markov","Tabulated")) %>% 
  select(type,everything()) %>% 
  magrittr::set_colnames(c("","ESI","Other Private","Public","Uninsured")) %>% 
  pander::pander()

DD_transitions <-
  df %>%
  filter(idnumber %in% mf$idnumber) %>%
  mutate(
    Y_f0 = !!outcome_baseline,
    Y_f1 = !!outcome_fu,
    Tx = !!tx
  ) %>%
  count(Y_f0, Y_f1, Tx) %>%
  group_by(Y_f0, Tx) %>%
  mutate(n = n / sum(n)) %>%
  rename(pct = n) %>%
  gather(key, value, -Y_f0, -Y_f1, -Tx) %>%
  unite(key, key, Y_f1) %>%
  spread(key, value) %>%
  ungroup() %>%
  mutate_at(vars(contains("pct_")), function(x) x - lag(x)) %>%
  filter(Tx == 1) %>%
  mutate(category = Y_f0) %>%
  select(category, contains("pct")) %>%
  magrittr::set_colnames(c("category", c("01_esi","02_priv_oth","03_public","04_uninsured")))

T_DD <- T_Tx - T_Cx

sT_DD <- sigfig(T_DD,3) %>% apply(.,1,function(x) gsub("^0$","0.0",x)) %>% t()

## Now run the model version of this

# Generate the model fit formula
fmla_lhs <- paste0("cbind(", paste0(attr(mf, "binary_variables"), collapse = ","), ")")
fmla_rhs_base <- paste0(quo_name(tx), " * post * baseline_outcome")
fmla_rhs_covariates <- rhs_final %>%
  purrr::discard(function(x) x %in% c("baseline_outcome"))

fmla_rhs <- paste0(c(fmla_rhs_covariates, fmla_rhs_base), collapse = "+")
fmla_rhs <- paste0(fmla_rhs, "+ post * baseline_outcome + state * baseline_outcome")


# Linear Probabilty
mult_multivar_fmla <- transitions_model_formula <- 
  as.formula(paste0(fmla_lhs, "~", fmla_rhs))

# Fit the model
f.mm.full <- lm(mult_multivar_fmla, data = mf)

dd_full_lpm <- factor_levs %>%
  map(~(
    # First difference is treated_post - treated_pre
    (predict(f.mm.full, newdata = mf %>% mutate(!!quo_name(tx) := 1, baseline_outcome = factor(.x, levels = levels(mf$baseline_outcome)), post = 1)) -
       predict(f.mm.full, newdata = mf %>% mutate(!!quo_name(tx) := 1, baseline_outcome = factor(.x, levels = levels(mf$baseline_outcome)), post = 0))) -
      
      # second difference is control_post - control_pre
      (predict(f.mm.full, newdata = mf %>% mutate(!!quo_name(tx) := 0, baseline_outcome = factor(.x, levels = levels(mf$baseline_outcome)), post = 1)) -
         predict(f.mm.full, newdata = mf %>% mutate(!!quo_name(tx) := 0, baseline_outcome = factor(.x, levels = levels(mf$baseline_outcome)), post = 0)))
  )) %>%
  map(~summarise_all(tbl_df(.), function(x) mean(x, na.rm = TRUE))) %>%
  set_names(factor_levs)


dd_full_lpm %>%
  bind_rows() %>%
  mutate_at(vars(1:4),function(x) round(x,4)) %>% 
  mutate(variable = attr(mf, "binary_variables")) %>%
  select(variable, everything()) %>% 
  mutate(variable = gsub("^insurance_","",variable))%>% 
  magrittr::set_colnames(c("",gsub("ins_","",insurance_lut))) %>% 
  pander::pander()


DD_full_lpm <- dd_full_lpm %>% bind_rows() %>% as.matrix() 
# Add death row
DD_full_lpm[is.na(DD_full_lpm)] <- 0

# Observed Ex Post
Y_1 <- M_Tx_ExA %*% T_Tx

# Counterfactual Ex Post
Y_0_full <- (M_Tx_ExA %*% T_Tx)  - ((M_Tx_ExA %*% T_Tx - M_Tx_ExA)  - (M_Cx_ExA %*% (T_Tx - DD_full_lpm) - M_Cx_ExA))
Y_DD_full <- Y_1 - Y_0_full

# Save the estimates for modeling later
markov_estimates <- 
  list(M_Tx_ExA = M_Tx_ExA,
       M_Cx_ExA = M_Cx_ExA, 
       T_Tx = T_Tx, 
       T_Cx = T_Cx,
       T_DD = DD_full_lpm,
       Y_1 = Y_1,
       Y_0 = (M_Tx_ExA %*% T_Tx)  - ((M_Tx_ExA %*% T_Tx - M_Tx_ExA)  - (M_Cx_ExA %*% (T_Tx - DD_full_lpm) - M_Cx_ExA)),
       DD = Y_DD_full)

markov_estimates %>% 
  write_rds(here("output/dd-estimates/sipp-marginal-and-dd-estimates.rds"))

## Fit the Model 

mult_multivar_fmla <- 
  as.formula("cbind(insurance_01_esi, insurance_02_priv_oth, insurance_03_public, insurance_04_uninsured) ~  state + year + expansion_state * post ")

# Fit the model
f.mm_marginal <- lm(mult_multivar_fmla, data = mf) %>% broom::tidy()

DD_estimates_marginal <- 
  f.mm_marginal %>% filter(term=="expansion_state:post") %>% 
  select(estimate) %>%  t() 

# Using the transitions matricies
obs <- (M_Tx_ExA %*% T_Tx) 

# Change over time in treated group:
diff_tx <- (M_Tx_ExA %*% T_Tx - M_Tx_ExA) 

# Change over time in control group
diff_cx <- M_Cx_ExA %*% T_Cx - M_Cx_ExA

diff_tx - diff_cx

diff_diff <- diff_tx - diff_cx

DD_estimates_marginal %>% rbind(diff_diff) %>% 
  magrittr::set_colnames(gsub("^ins_","",insurance_lut)) %>% tbl_df() %>% 
  mutate(type = c("DiD (Equation 1)","DiD Outcome Transitions Model (Equation 2)")) %>% 
  select(type,everything()) %>%   
  mutate_at(2:5,function(x) signif(x,digits = 3)) %>% 
  pander()

