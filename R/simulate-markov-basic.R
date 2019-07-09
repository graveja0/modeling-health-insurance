source(here::here("R/manifest.R"))
library(heemod)

# Ex Ante Distribution
ex_ante <- read_rds("output/dd-estimates/sipp-marginal-and-dd-estimates.rds")
c <- ex_ante$M_Cx_ExA * 4000000
R <- ex_ante$T_Cx

mat_covg <- define_transition(
  state_names = c(
    "ESI",
    "NG",
    "PUB",
    "UNIN"
  ),
  
  C,R[1,2],R[1,3],R[1,4],
  R[2,1],C,R[2,3],R[2,4],
  R[3,1],R[3,2],C,R[3,4],
  R[4,1],R[4,2],R[4,3],C
)

DD <- read_rds("output/dd-estimates/sipp-dd-estimates-with-inference.rds")

df_DD_params <- 
  DD %>% 
  pluck("fit") %>% 
  mutate(transition = gsub("insurance_","",term)) %>% 
  mutate(transition = gsub("01_|02_|03_|04_","",transition)) 

DD_params <- df_DD_params$obs_estimate %>% set_names(df_DD_params$transition)

df_DD_inference <- 
  DD$ri %>% 
  ungroup() %>% 
  group_by(term) %>% 
  summarize(se_est = sd(estimate)) %>% 
  mutate(transition = gsub("insurance_","",term)) %>% 
  mutate(transition = gsub("01_|02_|03_|04_","",transition))  

DD_se <- df_DD_inference$se_est %>% set_names(df_DD_inference$transition)

mat_reformed <- define_transition(
  state_names = c(
    "ESI",
    "NG",
    "PUB",
    "UNIN"
  ),
  
  C,max(0,min(1,R[1,2] + esi_TO_priv_oth))  , max(0,min(1,R[1,3] + esi_TO_public)) ,max(0,min(1,R[1,4]+ esi_TO_uninsured)),
  max(0,min(1,R[2,1] + priv_oth_TO_esi )),C,max(0,min(1,R[2,3] + priv_oth_TO_public)) ,max(0,min(1,R[2,4] + priv_oth_TO_uninsured)),
  max(0,min(1,R[3,1] + public_TO_esi)) ,max(0,min(1,R[3,2] + public_TO_priv_oth)),C,max(0,min(1,R[3,4] + public_TO_uninsured)),
  max(0,min(1,R[4,1] + uninsured_TO_esi)) ,max(0,min(1,R[4,2] + uninsured_TO_priv_oth)),max(0,min(1,R[4,3] + uninsured_TO_public)),C
)

st_ESI <- define_state(utility = welfare_esi, 
                       cost = cost_esi)
st_NG <- define_state(utility = welfare_ng,
                      cost = cost_ng)
st_PUB <- define_state(utility = welfare_pub, 
                       cost = cost_pub)
st_UNIN = define_state(utility = welfare_unin,
                       cost = cost_unin)

baseline <- define_strategy(
  transition = mat_covg,
  ESI = st_ESI,
  NG = st_NG,
  PUB = st_PUB,
  UNIN = st_UNIN,
  starting_values = define_starting_values(
    cost = 0
  )
)

reformed <- define_strategy(
  transition = mat_reformed,
  ESI = st_ESI,
  NG = st_NG,
  PUB = st_PUB,
  UNIN = st_UNIN,
  starting_values = define_starting_values(
    cost = 0
  )
)

param <- define_parameters( 
  welfare_ng = 0.67,
  welfare_esi = 1,
  welfare_pub = 0.4,
  welfare_unin = 0.0,
  
  cost_ng = 3000,
  cost_esi = 1200,
  cost_pub = 5000,
  cost_unin = 1000,
  
  esi_TO_esi = DD_params["esi_TO_esi"],
  esi_TO_priv_oth = DD_params["esi_TO_priv_oth"], 
  esi_TO_public = DD_params["esi_TO_public"], 
  esi_TO_uninsured = DD_params["esi_TO_uninsured"], 
  
  priv_oth_TO_esi = DD_params["priv_oth_TO_esi"],
  priv_oth_TO_priv_oth = DD_params["priv_oth_TO_priv_oth"], 
  priv_oth_TO_public = DD_params["priv_oth_TO_public"], 
  priv_oth_TO_uninsured = DD_params["priv_oth_TO_uninsured"], 
  
  public_TO_esi = DD_params["public_TO_esi"],
  public_TO_priv_oth = DD_params["public_TO_priv_oth"], 
  public_TO_public = DD_params["public_TO_public"], 
  public_TO_uninsured = DD_params["public_TO_uninsured"], 
  
  uninsured_TO_esi = DD_params["uninsured_TO_esi"],
  uninsured_TO_priv_oth = DD_params["uninsured_TO_priv_oth"], 
  uninsured_TO_public = DD_params["uninsured_TO_public"], 
  uninsured_TO_uninsured = DD_params["uninsured_TO_uninsured"]
)

res_mod <- run_model(
  reformed = reformed,
  baseline = baseline,
  parameters = param,
  cycles = 2,
  cost = cost,
  effect = utility,
  init = c,
  central_strategy = "baseline",
  method = "life-table"
)

p <- 
  plot(res_mod, type = "counts", panel = "by_state", free_y = TRUE) +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  ) + ggthemes::theme_tufte()
p


rsp <- define_psa(
  
  esi_TO_esi ~ normal(DD_params["esi_TO_esi"],DD_se["esi_TO_esi"]),
  esi_TO_priv_oth ~ normal(DD_params["esi_TO_priv_oth"],DD_se["esi_TO_priv_oth"]), 
  esi_TO_public ~ normal(DD_params["esi_TO_public"],DD_se["esi_TO_public"]), 
  esi_TO_uninsured ~ normal(DD_params["esi_TO_uninsured"],DD_se["esi_TO_uninsured"]), 
  
  priv_oth_TO_esi ~ normal(DD_params["priv_oth_TO_esi"],DD_se["priv_oth_TO_esi"]),
  priv_oth_TO_priv_oth ~ normal(DD_params["priv_oth_TO_priv_oth"],DD_se["priv_oth_TO_priv_oth"]), 
  priv_oth_TO_public ~ normal(DD_params["priv_oth_TO_public"],DD_se["priv_oth_TO_public"]), 
  priv_oth_TO_uninsured ~ normal(DD_params["priv_oth_TO_uninsured"],DD_se["priv_oth_TO_uninsured"]), 
  
  public_TO_esi ~ normal(DD_params["public_TO_esi"],DD_se["public_TO_esi"]),
  public_TO_priv_oth ~ normal(DD_params["public_TO_priv_oth"],DD_se["public_TO_priv_oth"]), 
  public_TO_public ~ normal(DD_params["public_TO_public"],DD_se["public_TO_public"]), 
  public_TO_uninsured ~ normal(DD_params["public_TO_uninsured"],DD_se["public_TO_uninsured"]), 
  
  uninsured_TO_esi ~ normal(DD_params["uninsured_TO_esi"],DD_se["uninsured_TO_esi"]),
  uninsured_TO_priv_oth ~ normal(DD_params["uninsured_TO_priv_oth"],DD_se["uninsured_TO_priv_oth"]), 
  uninsured_TO_public ~ normal(DD_params["uninsured_TO_public"],DD_se["uninsured_TO_public"]), 
  uninsured_TO_uninsured ~ normal(DD_params["uninsured_TO_uninsured"],DD_se["uninsured_TO_uninsured"]),
  
  correlation = diag(1,nrow = 16,ncol=16)
)

psa_run <- run_psa(res_mod, psa = rsp, N = 100)

psa_run$psa %>% select(-contains("TO")) %>% 
  group_by(.index) %>% 
  mutate_at(vars(.cost,.effect),function(x) (x-lead(x))/4e6) %>% 
  filter(!is.na(.cost)) %>% 
  mutate(.cost = .cost / 10000) %>% 
  ggplot(aes(x = .effect, y = .cost)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1/.3,lty=2) + 
  geom_abline(intercept = 0, slope = 1/.8, lty=3) +
 # xlim(c(-.5,.5)) + 
  #ylim(c(-1,1)) + 
  geom_hline(aes(yintercept = 0)) + 
  geom_vline(aes(xintercept = 0)) +
  ggthemes::theme_few() 
  
  
  
