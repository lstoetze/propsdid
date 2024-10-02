
  # Setup
  source("00_setup.R")
  source("00_sim_func.R")

# Simulation Setup ======
  
  # Cases
  sim_cases <- expand.grid("time_periods"=c(5,10), # 5, 10
                           "treatment_period"=c(1,2), # 1, 2
                           "units" = c(50,200,1000), # 50, 500, 2000 
                           "treatment_proportion" =  c(0.1,0.5), # 0.1, 0.5
                           "treatment_selection" = c(0,1)) %>%
    mutate(treat_period_start = time_periods - (treatment_period - 1))


  # Number of repetitions
  Nrep <- 1000
  

# Run MC =====  

  # MC list results
  res_mcmc <- list()

  # Loop
  for (i in 1:nrow(sim_cases)){
    
    # Set-up Cluster
    num_cores <- 50  # Use one less core than available
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)   
    
    # Setup values
    time_periods <- sim_cases$time_periods[i]
    treat_period_start <- sim_cases$treat_period_start[i] 
    N <- sim_cases$units[i]
    treated_n <- round(sim_cases$units[i]*sim_cases$treatment_proportion[i])
    set_delta =  sim_cases$treatment_selection[i]
    
    
    res_est <- foreach(sample = 1:Nrep, .combine = 'c', .packages = c('dplyr', 'foreach',
                                                                 'propsdid','reshape2')) %dopar% {
                                                                                                             
      # Generate Data
      df <- simulateDGP_multi(K=4,tau = c(0.5,0,0,0),
                              T = time_periods, 
                              treat_t = treat_period_start, 
                              
                              N=N, treated_n = treated_n,
                              
                              set_delta =  set_delta, 
                              lambda_set = 2,
                              
                              gamma_sd = c(1,1,1,1), gamma_mu = c(0,0,0,0),
                              upsilon_sd = c(0.5,0,0,0), upsilon_mu = c(0,0,0,0),
                              
                              gamma_update_sd = 0.1, upsilon_update_sd= 0.2,
                              
                              epsilon_i_sd=0.5, epsilon_t_sd=0.5)$dat
      
      # Calculate in-sample ATT and ATE based on counterfactuals
      att <- df %>% 
        filter(treated == 1, t >= treat_period_start) %>%
        group_by(k) %>%
        summarise("att" = mean(y1 - y0)) %>%
        pull("att")
      
      ate <- df %>% 
        group_by(k) %>%
        summarise("ate" = mean(y1 - y0)) %>%
        pull("ate")
      
      # Estimate Models
      df_sub <- df %>% dplyr::select(i, t, k, y, d, treated)
      res <- estmation_step(df_sub, K = 4)
      
      res_temp <- data.frame(t(do.call("rbind", res)))
      res_temp$ate <- ate
      res_temp$att <- att
      res_temp$k <- 1:4
      res_temp$sample <- sample
      rownames(res_temp) <- NULL
      
      list(res_temp)
      }
    
      # Put to data frame
      res_df <- do.call(rbind,res_est) 
      
      # Add info
      res_df <- cbind(res_df, c(sim_cases[i,]))
      
      # Add to list
      res_mcmc[[i]] <- res_df
      
      # Stop the cluster
      stopCluster(cl)
  }
  
  # Save results
  saveRDS(do.call(rbind,res_mcmc),file = "Sim_res.RDS")
