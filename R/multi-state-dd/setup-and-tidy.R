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
