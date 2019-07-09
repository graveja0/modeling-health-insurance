# Fit the Difference-in-Difference Transition Models

source(here::here("R/manifest.R"))

months_to_model <- c(1,24) 

df_sipp_full <- read_rds(here("input/sipp/01_sipp-tidy_v1-0.rds"))

# Get the weight (first month weight)
df_w <- 
  df_sipp_full %>% 
  mutate(idnumber = id) %>% 
  group_by(idnumber) %>% 
  mutate(year = ifelse(swave ==1 , 2014, 2015)) %>% 
  mutate(month = ifelse(swave ==1 , monthcode, monthcode+12)) %>% 
  filter(month==1) %>% 
  # Expansion state varies in the underling data due to moves. Just keeping state at 
  # baseline. 
  select(idnumber,state, expansion_state, sex, race, weight = wpfinwgt) %>% 
  ungroup() 

df_sipp <- 
  df_sipp_full %>%   
  # Expansion state varies in the underling data due to moves. Just keeping state at 
  # baseline. 
  select(-expansion_state,-state) %>% 
  filter(age < 63 & age > 18) %>% 
  mutate(year = ifelse(swave ==1 , 2014, 2015)) %>% 
  mutate(month = ifelse(swave ==1 , monthcode, monthcode+12)) %>% 
  mutate(idnumber = id) %>% 
  mutate(insurance_type = factor(hicov, labels = insurance_sipp_lut)) %>% 
  #mutate(insurance_type = paste0("ins_",insurance_type)) %>% 
  select(idnumber,month,insurance_type)  %>% 
  group_by(idnumber) %>% 
  ungroup()  %>% 
  left_join(df_w,"idnumber") 
  
df <- 
  df_sipp %>% 
  filter(month %in% months_to_model) %>% 
  group_by(idnumber) %>% 
  filter(n()==2) %>% 
  arrange(idnumber,month) %>% 
  ungroup() %>% 
  mutate(month = paste0("insurance_m",month)) %>% 
  spread(month,insurance_type) %>% 
  mutate(year_m1 = 2013) %>% 
  mutate(year_m24 = 2014) 

# Lookup table for the outcome response categories.
insurance_lut <-
  c(
    "cat_1" = "ins_01_esi",
    "cat_2" = "ins_02_priv_oth",
    "cat_3" = "ins_03_public",
    "cat_4" = "ins_04_uninsured"
  )

# Define the pre period
pre <- "_m1"  
# Define the post period
post <- "_m24"
# Define the outcome
outcome <- quo(insurance)
# The pre-expansion outcome variable name in wide format
outcome_baseline <- sym(paste0(quo_name(outcome), pre))
# The post-expansion outcome variable name in wide format
outcome_fu <- sym(paste0(quo_name(outcome), post))


# Define the treatment indicator
tx <- quo(expansion_state)
# Define the unque ID for individuals
id <- quo(idnumber)

# RHS variables that vary over time (should just be questionnaire year)
rhs_varying = c("year")

# The code below identifies the specific variable names that will go into the final model frame. 
# It is somewhat complex as it parses through iteraction terms and the like, and isolates
# only unique variable names that show up in the data.

# Baseline / Non-Varying RHS variables
rhs <- c("state","baseline_outcome")
# RHS of model as a character vector
rhs_chr <- c(rhs_varying,rhs)
# Main effects (i.e., unique variable names to include in model frame)
main_effects_rhs <- unlist(strsplit(gsub("\\*"," ",rhs)," ")) %>% unique()
# Tidy listing of RHS
rhs <- rlang::syms(main_effects_rhs)
rhs_varying <- rlang::syms(rhs_varying)
rhs_varying_chr <- as.character(rhs_varying)

# Variables to include in the data frame extract, but not to include on the RHS of the model
# allows, for example, to allow the model frame to return the ID variable, but not 
# to use the ID variable in the RHS of the fitted model. For now I just set to empty. 
fmla_exclude <- c("")

# The final listing of RHS variables
rhs_final <- as.character(rhs_chr) %>%
  purrr::discard(function(x) x %in% fmla_exclude)

# The RHS of the model (specified as a formula)
fmla_rhs <- rhs_final %>%
  paste0(.,collapse="+") %>%
  gsub("~","",.)

# The LHS of the model. 
fmla_lhs <- quo_text(outcome) %>%
  paste0(.,collapse="") %>%
  gsub("~","",.)

# The variable names of the LHS of the model, when the data are in wide format (i.e., rows unique to individuals)
lhs_wide <- grep(paste0(paste0(pre,"$|"),paste0(post,"$")),grep(fmla_lhs,colnames(df),value = TRUE),value = TRUE)

# The variable names of the RHS of the model, when the data are in wide format (i.e., rows unique to individuals)
rhs_wide <- grep("m1$|m24$$",grep(paste0(rhs_varying_chr,collapse="|"),colnames(df),value=TRUE),value = TRUE)

# The treatment indicator part of the model.
fmla_tx <- quo_text(tx) %>%
  paste0(.,collapse="") %>%
  gsub("~","",.)

# The treatment indicator*post part of the model, as a formula. 
fmla_tx <- paste(c(paste0(fmla_tx,"*post")),collapse="+")

# The DD model formula (should be year + state + expansion_state * post + covariates)
full_fmla <- as.formula(paste0(fmla_lhs,"~",fmla_rhs,"+",fmla_tx))

###############
# Reshape Data
################

# Baseline Data (this data frame contains all non-varying covariates)
df.x <- df %>%
  mutate(baseline_outcome = !!outcome_baseline) %>% 
  select(!!id,!!tx,!!!rhs)

# Final "long" data frame. Code reshapes the time-varying outcomes and RHS variables, then
# merges in the baseline (and other fixed) variables in df.x above. 
df_panel <-
  suppressWarnings(
    suppressMessages(
      df %>% select(!!id,!!!lhs_wide,!!!rhs_wide) %>%
        # Send to super long format (id,variable,value only)
        gather(key,value,-!!id) %>%
        # O/1 defines pre/post
        mutate(post = ifelse(grepl("m1$",key),0,1)) %>%
        separate(key,into=c("key","tk"),sep="_m1$|_m24$") %>%
        arrange(!!id) %>%
        na.omit() %>% 
        # Now need to put variables back as columns
        spread(key,value) %>%
        select(-tk) %>%
        left_join(df.x) %>%
        filter(!is.na(!!outcome)) %>%
        group_by(!!id) %>%
        # Some people will not get two observations
        mutate(num_obs = n()) %>%
        arrange(!!id) %>%
        mutate(df_row_number = row_number())  %>%
        tbl_df()
    )
  )

# Add binary indicators for the categorical outcome.
add_binary_indicators <- function(df, variable) {
  var <- enquo(variable)
  df_tmp <-
    df %>%
    tbl_df() %>%
    # select(!!var) %>%
    mutate(!!quo_name(var) := factor(!!var))
  var_levels <- unique(df_tmp[, quo_name(var)]) %>% pull(!!var) %>% levels()
  newvars <- c()
  for (vv in var_levels) {
    newvar <- paste0(quo_name(var), "_", vv)
    newvars <- c(newvars, newvar)
    df_tmp <-
      df_tmp %>%
      mutate(!!newvar := case_when(
        !!var %in% vv ~ 1,
        !(!!var %in% vv) ~ 0
      ))
  }
  attr(df_tmp, "binary_variables") <- newvars
  df_tmp
}

# Construct the full model frame
mf <- 
  df_panel %>% 
  mutate(year = as.factor(year)) %>% 
  add_binary_indicators(!!outcome) 


# factor_levs provides a K-length character vector of the levels of the K-category outcome factor variable (e.g., 1, 2, 3, ..., K)
factor_levs <- gsub(paste0(quo_name(outcome), "_"), "", attr(mf, "binary_variables"))

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
mult_multivar_fmla <-
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

