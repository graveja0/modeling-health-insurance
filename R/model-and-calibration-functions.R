
run_insurance_markov <- function(v_params) {
  with(as.list(v_params), {
    n_t <- 4
    v_n  <- c("ESI", "NG", "PUB", "UNIN") # the 4 states of the model: ESI, non-group, public, uninsured
    n_s <- length(v_n)              # number of insurance categories
    
    ####### INITIALIZATION ##########################################
    # create the cohort trace
    m_M <- matrix(NA, nrow = n_t + 1 , 
                  ncol = n_s,
                  dimnames = list(0:n_t, v_n))     # create Markov trace (n_t + 1 because R doesn't understand  Cycle 0)
    
    m_M[1, ] <- p                      # initialize Markov trace
    
    # create transition probability matrix for NO treatment
    m_P <- matrix(0,
                  nrow = n_s, 
                  ncol = n_s,
                  dimnames = list(v_n, v_n))
    # fill in the transition probability array
    ### From ESI
    m_P["ESI", "ESI"]  <- 1 - (p_ESI_NG + p_ESI_PUB + p_ESI_UNIN)
    m_P["ESI", "NG"] <- p_ESI_NG
    m_P["ESI", "PUB"] <- p_ESI_PUB
    m_P["ESI", "UNIN"] <- p_ESI_UNIN
    
    ### From Non-Group
    m_P["NG", "NG"]  <- 1 - (p_NG_ESI + p_NG_PUB + p_NG_UNIN)
    m_P["NG", "ESI"] <- p_NG_ESI
    m_P["NG", "PUB"] <- p_NG_PUB
    m_P["NG", "UNIN"] <- p_NG_UNIN
    
    ### From Non-Group
    m_P["PUB", "NG"]  <- p_PUB_NG
    m_P["PUB", "ESI"] <- p_PUB_ESI
    m_P["PUB", "PUB"] <- 1 - (p_PUB_ESI + p_PUB_NG + p_PUB_UNIN)
    m_P["PUB", "UNIN"] <- p_PUB_UNIN
    
    
    ### From UNINSURED
    m_P["UNIN", "NG"]  <- p_UNIN_NG
    m_P["UNIN", "ESI"] <- p_UNIN_ESI
    m_P["UNIN", "PUB"] <- p_UNIN_PUB
    m_P["UNIN", "UNIN"] <- 1 - (p_UNIN_ESI + p_UNIN_NG + p_UNIN_PUB)
    
    
    # check rows add up to 1
    if (!isTRUE(all.equal(as.numeric(rowSums(m_P)), as.numeric(rep(1, n_s))))) {
      stop("This is not a valid transition Matrix")
    }
    
    ############# PROCESS ###########################################
    
    for (t in 1:n_t){                              # throughout the number of cycles
      m_M[t + 1, ] <- m_M[t, ] %*% m_P           # estimate the Markov trace for cycle the next cycle (t + 1)
    }
    
    ####### EPIDEMIOLOGICAL OUTPUT  ###########################################
    #### Overall Survival (OS) ####
    #v_os <- 1 - m_M[, "UNIN"]                # calculate the overall survival (OS) probability for no treatment
    v_ESI <-  m_M[,"ESI"]
    v_NG <- m_M[,"NG"]
    v_PUB <- m_M[,"PUB"]
    v_UNIN <- m_M[,c("UNIN")]
    
    
    ####### RETURN OUTPUT  ###########################################
    #out <- list(coverage = m_M[2,])
    out <- list(ESI = m_M[-1,1],
                NG = m_M[-1,2],
                PUB = m_M[-1,3],
                UNIN = m_M[-1,4])
    
    return(out)
  }
  )
}

####################################################################
######  Specify calibration parameters  ######
####################################################################

p_ESI_NG <- R[1,2]
p_ESI_PUB <- R[1,3]
p_ESI_UNIN <- R[1,4]

p_NG_ESI <- R[2,1]
p_NG_PUB <- R[2,3]
p_NG_UNIN <- R[2,4]

p_PUB_ESI <- R[3,1]
p_PUB_NG <- R[3,2]
p_PUB_UNIN <- R[3,4]

p_UNIN_ESI <- R[4,1]
p_UNIN_NG <- R[4,2]
p_UNIN_PUB <- R[4,3]

ev <- c(p_ESI_NG = p_ESI_NG, p_ESI_PUB = p_ESI_PUB, p_ESI_UNIN = p_ESI_UNIN,
        p_NG_ESI = p_NG_ESI, p_NG_PUB = p_NG_PUB, p_NG_UNIN = p_NG_UNIN,
        p_PUB_ESI = p_PUB_ESI, p_PUB_NG = p_PUB_NG, p_PUB_UNIN = p_PUB_UNIN,
        p_UNIN_ESI = p_UNIN_ESI, p_UNIN_NG = p_UNIN_NG, p_UNIN_PUB = p_UNIN_PUB)



# Specify seed (for reproducible sequence of random numbers)
set.seed(23)

prior_type = "uniform"

# number of random samples
n_resamp <- 5000

# names and number of input parameters to be calibrated
v_param_names <- c("p_ESI_NG","p_ESI_PUB","p_ESI_UNIN","p_NG_ESI","p_NG_PUB","p_NG_UNIN",
                   "p_PUB_ESI","p_PUB_NG","p_PUB_UNIN","p_UNIN_ESI", "p_UNIN_NG","p_UNIN_PUB")
n_param <- length(v_param_names)

# range on input search space
ev <- c(p_ESI_NG = R[1,2], p_ESI_PUB = R[1,3], p_ESI_UNIN =R[1,4], 
        p_NG_ESI = R[2,1], p_NG_PUB = R[2,3], p_NG_UNIN = R[2,4],
        p_PUB_ESI = R[3,1], p_PUB_NG = R[3,2], p_PUB_UNIN = R[3,4], 
        p_UNIN_ESI = R[4,1], p_UNIN_NG =R[4,2], p_UNIN_PUB = R[4,3]) # lower bound
lb <- c(p_ESI_NG = 0.001, p_ESI_PUB = 0.001, p_ESI_UNIN = 0.03, 
        p_NG_ESI = 0.05, p_NG_PUB = 0.001, p_NG_UNIN = 0.01,
        p_PUB_ESI = 0.01, p_PUB_NG = 0.01, p_PUB_UNIN = 0.02, 
        p_UNIN_ESI = 0.01, p_UNIN_NG = 0.01, p_UNIN_PUB = 0.05) # lower bound
ub <- c(p_ESI_NG = 0.15, p_ESI_PUB = 0.15, p_ESI_UNIN = 0.15, 
        p_NG_ESI = 0.30, p_NG_PUB = 0.20, p_NG_UNIN = 0.10,
        p_PUB_ESI = 0.15, p_PUB_NG = 0.1, p_PUB_UNIN = 0.15, 
        p_UNIN_ESI = 0.2, p_UNIN_NG = .1, p_UNIN_PUB = 0.15) # upper bound

# number of calibration targets
v_target_names <- c("ESI","NG","PUB","UNIN")
n_target     <- length(v_target_names)

### Calibration functions

#  Write function to sample from prior
sample_prior <- function(n_samp){
  m_lhs_unit   <- randomLHS(n = n_samp, k = n_param)
  m_param_samp <- matrix(nrow = n_samp, ncol = n_param)
  colnames(m_param_samp) <- v_param_names
  for (i in 1:n_param){
    
    if (prior_type=="beta") {
      m_param_samp[,i] <- qbeta(m_lhs_unit[,i],shape1 = ev[i]*100,shape2=100-ev[i]*100)
    } else {
      m_param_samp[, i] <- qunif(m_lhs_unit[,i],
                                 min = lb[i],
                                 max = ub[i])
    }
  }
  return(m_param_samp)
}


###  PRIOR  ### 
# Write functions to evaluate log-prior and prior

# function that calculates the log-prior
calc_log_prior <- function(v_params){
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  n_samp <- nrow(v_params)
  colnames(v_params) <- v_param_names
  lprior <- rep(0, n_samp)
  for (i in 1:n_param){
    if (prior_type=="beta") {
      lprior <- lprior + dbeta(v_params[,i], shape1 = ev[i]*100, shape2 = 100-ev[i]*100,log = T)
    } else {
      lprior <- lprior + dunif(v_params[, i],
                               min = lb[i],
                               max = ub[i],
                               log = T)
    }
    
  }
  return(lprior)
}
calc_log_prior(v_params = ev)
calc_log_prior(v_params = sample_prior(10))

# function that calculates the (non-log) prior
calc_prior <- function(v_params) { 
  exp(calc_log_prior(v_params)) 
}
calc_prior(v_params = ev)
calc_prior(v_params = sample_prior(10))

###  LIKELIHOOD  ###
# Write functions to evaluate log-likelihood and likelihood

# function to calculate the log-likelihood
calc_log_lik <- function(v_params){
  # par_vector: a vector (or matrix) of model parameters 
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  n_samp <- nrow(v_params)
  v_llik <- matrix(0, nrow = n_samp, ncol = n_target) 
  llik_overall <- numeric(n_samp)
  for(j in 1:n_samp) { # j=1
    jj <- tryCatch( { 
      ###   Run model for parametr set "v_params" ###
      model_res <- run_insurance_markov(v_params[j, ])
      
      ###  Calculate log-likelihood of model outputs to targets  ###
      # TARGET 1: Survival ("Surv")
      # log likelihood  
      v_llik[j, 1] <- sum(dnorm(x = lst_targets$ESI,
                                mean = model_res$ESI,
                                sd = 1*sqrt(lst_targets$ESI * (1-lst_targets$ESI))/1e2,
                                log = T))
      
      v_llik[j, 2] <- sum(dnorm(x = lst_targets$NG,
                                mean = model_res$NG,
                                sd = 1*sqrt(lst_targets$NG * (1-lst_targets$NG))/1e2,
                                log = T))
      v_llik[j, 3] <- sum(dnorm(x = lst_targets$PUB,
                                mean = model_res$PUB,
                                sd = 1*sqrt(lst_targets$PUB * (1-lst_targets$PUB))/1e2,
                                log = T))
      v_llik[j, 4] <- sum(dnorm(x = lst_targets$UNIN,
                                mean = model_res$UNIN,
                                sd = 1*sqrt(lst_targets$UNIN * (1-lst_targets$UNIN))/1e2,
                                log = T))      
      
      # OVERALL 
      llik_overall[j] <- sum(v_llik[j, ])
    }, error = function(e) NA) 
    if(is.na(jj)) { llik_overall <- -Inf }
  } # End loop over sampled parameter sets
  # return LLIK
  return(llik_overall)
}
calc_log_lik(v_params = ev)
calc_log_lik(v_params = sample_prior(10))


# function to calculate the (non-log) likelihood
calc_likelihood <- function(v_params){ 
  exp(calc_log_lik(v_params)) 
}
calc_likelihood(v_params = ev)
calc_likelihood(v_params = sample_prior(1000))

###  POSTERIOR  ###
# Write functions to evaluate log-posterior and posterior

# function that calculates the log-posterior
calc_log_post <- function(v_params) { 
  lpost <- calc_log_prior(v_params) + calc_log_lik(v_params)
  return(lpost) 
}
calc_log_post(v_params = ev)
calc_log_post(v_params = sample_prior(10))


# function that calculates the (non-log) posterior
calc_post <- function(v_params) { 
  exp(calc_log_post(v_params)) 
}
calc_post(v_params = ev)
calc_post(v_params = sample_prior(10))

####################################################################
######  Calibrate!  ######
####################################################################
# record start time of calibration
t_init <- Sys.time()

###  Bayesian calibration using IMIS  ###
# define three functions needed by IMIS: prior(x), likelihood(x), sample.prior(n)

# based on ACS tables for 19-64 year olds in ACS data as constructed at http://statehealthcompare.shadac.org/                    


prior <- calc_prior
likelihood <- calc_likelihood
sample.prior <- sample_prior

# run IMIS
fit_imis <- IMIS(B = 5000, # the incremental sample size at each iteration of IMIS
                 B.re = n_resamp, # the desired posterior sample size
                 number_k = 10, # the maximum number of iterations in IMIS
                 D = 0) 

# obtain draws from posterior
m_calib_res <- fit_imis$resample



# Calculate log-likelihood (overall fit) and posterior probability of each sample
m_calib_res <- cbind(m_calib_res, 
                     "Overall_fit" = calc_log_lik(m_calib_res[,v_param_names]),
                     "Posterior_prob" = calc_post(m_calib_res[,v_param_names]))

# normalize posterior probability
m_calib_res[,"Posterior_prob"] <- m_calib_res[,"Posterior_prob"]/sum(m_calib_res[,"Posterior_prob"])

# Calculate computation time
comp_time <- Sys.time() - t_init


# Compute posterior mean
v_calib_post_mean <- colMeans(m_calib_res[,v_param_names])
v_calib_post_mean

# Compute posterior median and 95% credible interval
m_calib_res_95cr <- colQuantiles(m_calib_res[,v_param_names], probs = c(0.025, 0.5, 0.975))
m_calib_res_95cr

# Compute maximum-a-posteriori (MAP) parameter set
v_calib_map <- m_calib_res[which.max(m_calib_res[,"Posterior_prob"]),]

### Plot model-predicted output at mode vs targets ###
v_out_best <- run_insurance_markov(v_calib_map[v_param_names])
v_out_mean <- run_insurance_markov(v_calib_post_mean)
v_out_orig <- run_insurance_markov(ev)

p_calib <- bind_rows(
  lst_targets %>% bind_cols() %>% 
    mutate(year = 2015:2018) %>% 
    mutate(label = "target"),
  v_out_best %>% bind_cols() %>% 
    mutate(year = 2015:2018) %>% 
    mutate(label = "best_calibrated"),
  v_out_mean %>% bind_cols() %>% 
    mutate(year = 2015:2018) %>% 
    mutate(label = "mean_calibrated")#,
  # v_out_orig %>% bind_cols() %>% 
  #   mutate(year = 2015:2018) %>% 
  #   mutate(label = "orig")
) %>% 
  gather(type,value,-year,-label) %>% 
  ggplot(aes(x = year, y = value, colour = label)) + 
  geom_point() + 
  facet_wrap(~type, scales = "free") + 
  theme_minimal() + 
  geom_line()


final_probs <- v_calib_map[v_param_names]

R_c <- matrix(nrow = nrow(R), ncol = ncol(R))
R_c[1,1] <- 1 - final_probs["p_ESI_NG"] - final_probs["p_ESI_PUB"]  - final_probs["p_ESI_UNIN"] 
R_c[1,2] <- final_probs["p_ESI_NG"] 
R_c[1,3] <- final_probs["p_ESI_PUB"] 
R_c[1,4] <- final_probs["p_ESI_UNIN"] 

R_c[2,2] <- 1 - final_probs["p_NG_ESI"] - final_probs["p_NG_PUB"]  - final_probs["p_NG_UNIN"] 
R_c[2,1] <- final_probs["p_NG_ESI"] 
R_c[2,3] <- final_probs["p_NG_PUB"] 
R_c[2,4] <- final_probs["p_NG_UNIN"] 

R_c[3,3] <- 1 - final_probs["p_PUB_ESI"] - final_probs["p_PUB_NG"]  - final_probs["p_PUB_UNIN"] 
R_c[3,1] <- final_probs["p_PUB_ESI"] 
R_c[3,2] <- final_probs["p_PUB_NG"] 
R_c[3,4] <- final_probs["p_PUB_UNIN"] 

R_c[4,4] <- 1 - final_probs["p_UNIN_ESI"] - final_probs["p_UNIN_NG"]  - final_probs["p_UNIN_PUB"] 
R_c[4,1] <- final_probs["p_UNIN_ESI"] 
R_c[4,2] <- final_probs["p_UNIN_NG"] 
R_c[4,3] <- final_probs["p_UNIN_PUB"] 

df_out <- 
  bind_rows(
    run_insurance_markov(ev)  %>%
      map(~(.x %>% tbl_df())) %>%
      bind_cols() %>%
      set_names(c("ESI","NG","PUB","UNIN")) %>%
      mutate(type = "uncalibrated") %>%
      mutate(year = c("2015","2016","2017","2018")),
    
    run_insurance_markov(final_probs) %>%
      map(~(.x %>% tbl_df())) %>%
      bind_cols() %>%
      set_names(c("ESI","NG","PUB","UNIN")) %>%
      mutate(type = "calibrated") %>%
      mutate(year = c("2015","2016","2017","2018")),
    
    lst_targets %>%
      map(~(.x %>% tbl_df())) %>%
      bind_cols() %>%
      set_names(c("ESI","NG","PUB","UNIN")) %>%
      mutate(type = "target") %>%
      mutate(year = c("2015","2016","2017","2018"))
  )

calibration_results <- 
  list(fit = m_calib_res,
       R_c = R_c,
       df = df_out,
       target = lst_targets,
       test = run_insurance_markov(v_calib_map[v_param_names]))

