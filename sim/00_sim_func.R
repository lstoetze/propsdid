# Random Assignment based on factors =====
randomize_treatment_based_on_L <- function(L, N, N1, lambda = 1) {
  
  # Calculte aim pi and use log-odds for constant
  aim_p <- N1/N
  const <- log(aim_p/(1-aim_p))
  
  # Transform the measure into probabilities using a logistic function with strength parameter lambda
  pi <- 1 / (1 + exp(- const -lambda * L))
  
  # Simulate treatment assignment based on computed probabilities
  assignment_sim <- rbinom(N, 1, pi)
  
  # Adjusting the number of treated units to meet the cap
  index_as <- which(assignment_sim == 1)
  
  if (sum(assignment_sim) > N1){
    index_pert <- sample(index_as,N1)
    assignment_sim <- rep(0,N)
    assignment_sim[index_pert] <- 1
  } else if (sum(assignment_sim) < N1) {
    index_cont <- 1:N
    index_as <- index_cont[-index_as]
    index_pert <- sample(index_as,(N-N1))
    assignment_sim <- rep(1,N)
    assignment_sim[index_pert] <- 0
  }  else if (sum(assignment_sim) == 0){
    index_pert <- sample(1:N,N1)
    assignment_sim <- rep(0,N)
    assignment_sim[index_pert] <- 1
  }
  
  
  return(assignment_sim)
}

# DGP simulation state space model === 

simulateDGP_multi <- function(N = 50, T = 10, K = 3, 
                              tau = c(1.5, -1.5, 0, 0), 
                              treated_n = 25, treat_t = 5, 
                              lambda_set = 2,  treatmet_sel = 1,
                              set_delta = 0.5,
                              gamma_sd = rep(1,K), gamma_mu = rep(0,K),
                              upsilon_sd = rep(1,K), upsilon_mu = rep(0,K),
                              epsilon_t_sd=0.5,epsilon_i_sd=0.5,
                              gamma_update_sd = 0.1, upsilon_update_sd = 0.1
) {
  
  require(reshape2)  
  require(MASS)
  
  # Generate Latent Unit Factors
  Gamma0 <-   mvrnorm(N, mu=gamma_mu, Sigma = diag(gamma_sd)) # Unit factors
  Upsilon0 <-   mvrnorm(N, mu=upsilon_mu, Sigma = diag(upsilon_sd))
  
  # Generate treatment assignment based on L and reorder L
  W_ind <- randomize_treatment_based_on_L(L = (set_delta * Gamma0[,treatmet_sel] + (1-set_delta) * Upsilon0[,treatmet_sel]), 
                                          N = N, N1 = treated_n, lambda = lambda_set) 
  
  # Sort Latent States
  Gamma0 <- rbind(Gamma0[W_ind == 1,], Gamma0[W_ind == 0,]) 
  Upsilon0 <- rbind(Upsilon0[W_ind == 1,], Upsilon0[W_ind == 0,]) 
  
  # Create treatment matrix
  W <- matrix(0, ncol = T, nrow = N)
  W[1:treated_n, (treat_t:T)] <- 1
  
  # Define outcome arrays
  Y <- array(NA, dim = c(N, T, K))
  Y0 <- array(NA, dim = c(N, T, K)) # Counterfactuals Y(0)
  Y1 <- array(NA, dim = c(N, T, K)) # Counterfactuals Y(1)
  
  # Loop over time periods and factors to generate outcomes
  for (t in 1:T) {
    
    
    # Loop over proportions
    for (k in 1:K) {
      
      # Update unit factors (Gamm0) and time factors (Upsilon) dynamically
      Gamma0[,k] <- Gamma0[,k] + Upsilon0[,k]  + rnorm(N, sd = gamma_update_sd) # Update Gamm0
      Upsilon0[,k] <- Upsilon0[,k] + rnorm(N, sd = upsilon_update_sd)
      
      # Error Terms
      E <- rnorm(N,sd=epsilon_i_sd) # Idiosyncratic errors
      e <- rnorm(1,sd=epsilon_t_sd)
      
      Lk <- (Gamma0[,k]) + e
      Y[, t, k] <- exp(Lk + tau[k] * W[, t] + E) # Outcome matrix
      Y0[, t, k] <- exp(Lk + E) # Counterfactual Y(0)
      Y1[, t, k] <- exp(Lk + tau[k] + E) # Counterfactual Y(1)
    }
    
  }
  
  # Calculate row sums for normalization
  Ysum <- rowSums(Y, dims = 2)
  Y0sum <- rowSums(Y0, dims = 2)
  Y1sum <- rowSums(Y1, dims = 2)
  
  # Normalize outcome arrays
  Y <- sweep(Y, c(1, 2), Ysum, FUN = "/")
  Y0 <- sweep(Y0, c(1, 2), Y0sum, FUN = "/")
  Y1 <- sweep(Y1, c(1, 2), Y1sum, FUN = "/")
  
  # Convert arrays to long format
  dat <- melt(Y, value.name = "y")
  dat0 <- melt(Y0, value.name = "y0")
  dat1 <- melt(Y1, value.name = "y1")
  
  # Merge data frames
  dat <- merge(merge(dat, dat0), dat1)
  colnames(dat)[1:3] <- c("i", "t", "k")
  dat$d <- ifelse(dat$t >= treat_t & dat$i <= treated_n, 1, 0)
  dat$treated <- ifelse(dat$i <= treated_n, 1, 0)
  
  # Return the simulated data and parameters
  list(dat = dat, Y = Y, W = W)
}


# Estimation step, Two-way Fixed Effect, SynDiD Separate, Prop Syn Did ====
estmation_step  <- function(df, K=4){
  
  tau_did <- NULL
  tau_sc <- NULL
  tau_sdid <- NULL
  
  for(kth in 1:K){
    # Two-way fixed effects
    tau_did[kth]  <- coef(lm(y ~ d + as.factor(t) + as.factor(i), filter(df,k==kth)))["d"]
    
    # SynDiff-in-DIff
    setup <- filter(df,k==kth) %>%
      mutate(d = d==1 ) %>%
      panel.matrices(.,outcome = "y",
                     unit = "i",time = "t",treatment = "d")
    
    tau_sdid[kth]  <- sc_estimate(setup$Y,setup$N0, setup$T0,
                                  method="sdid")
    
    tau_sc[kth]  <- sc_estimate(setup$Y,setup$N0, setup$T0,
                                method="sc")
    
  }
  
  # Setup Array
  setup <- panel.array(df)
  est_prop <- sc_estimate(setup$Y,setup$N0, setup$T0,
                          porp_dat=T,method="sdid")
  
  return(list("did"=tau_did, 
              "sc"=tau_sc,"sdid"=tau_sdid, 
              "sdid_prop"= c(est_prop)))
  
}