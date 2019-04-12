library(heemod)

R_exa = trans_probs %>% filter(time==13) %>% pluck("data") %>% pluck(1) %>% 
  select(2:5) %>% as.matrix()

mat_covg <- define_transition(
  state_names = c(
    "ESI",
    "NG",
    "PUB",
    "UNIN"
  ),
  
  C,R_exa[1,2],R_exa[1,3],R_exa[1,4],
  R_exa[2,1],C,R_exa[2,3],R_exa[2,4],
  R_exa[3,1],R_exa[3,2],C,R_exa[3,4],
  R_exa[4,1],R_exa[4,2],R_exa[4,3],C
)

mat_reformed <- define_transition(
  state_names = c(
    "ESI",
    "NG",
    "PUB",
    "UNIN"
  ),
  
  C,R_exa[1,2],R_exa[1,3],R_exa[1,4],
  R_exa[2,1],C,R_exa[2,3],R_exa[2,4],
  R_exa[3,1],R_exa[3,2],C,R_exa[3,4],
  R_exa[4,1],R_exa[4,2],R_exa[4,3] + chg_pub,C
)

baseline <- define_strategy(
  transition = mat_covg,
  ESI = define_state(
    utility = 1,
    cost = 0
  ),
  NG = define_state(
    utility = 1,
    cost = 0
  ),
  PUB = define_state(
    utility = 1,
    cost = 4000
  ),
  UNIN = define_state(
    utility = 1,
    cost = 0
  ),
  starting_values = define_starting_values(
    cost = 0
  )
)

reformed <- define_strategy(
  transition = mat_reformed,
  ESI = define_state(
    utility = 1,
    cost = 0
  ),
  NG = define_state(
    utility = 1,
    cost = 0
  ),
  PUB = define_state(
    utility = 1,
    cost = 4000
  ),
  UNIN = define_state(
    utility = 1,
    cost = 0
  ),
  starting_values = define_starting_values(
    cost = 0
  )
)

param <- define_parameters( 
  chg_pub = 0.2
)

res_mod <- run_model(
  baseline = baseline,
  reformed = reformed,
  parameters = param,
  cycles = 10,
  cost = cost,
  effect = utility,
  init = ex_ante$n
)

plot(res_mod, type = "counts", panel = "by_state", free_y = TRUE) +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  ) + ggthemes::theme_tufte()