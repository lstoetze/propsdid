#' ---
#' title: "Synthetic Difference-in-Difference for Proportions"
#' description: "Setup Functions for Simulations"
#' author: "Lukas F. Stoetzer"
#' ---


library(propsdid)
library(tidyverse)



# Makes some plots
df <- simulateDGP_multi(K=4,tau = c(0.25,-0.5,0,0),additive = F,ratiosd_GU = 0.1,
                        T = 5, treated_n = 3)$dat
df %>%
  group_by(treated,t,k) %>%
  summarise("mean_y" = mean(y)) %>%
  ggplot() +
  geom_line(aes(y=mean_y,x=t,group=as.factor(treated),col=as.factor(treated))) +
  facet_wrap(~ k, scales = "free") +
  theme_minimal()


setup <- panel.array(df)
est <- sc_estimate(setup$Y,setup$N0, setup$T0,
                   porp_dat=T,method="sdid")

summary(est)

# Estimation step, Two-way Fixed Effect, SynDiD Separate, Prop Syn Did
estmation_step  <- function(df, K=4){

  ate <- NULL
  tau.hat <- NULL
  
  for(kth in 1:K){
    # Two-way fixed effects
    ate[kth]  <- coef(lm(y ~ d + as.factor(t) + as.factor(i), filter(df,k==kth)))["d"]

    # SynDiff-in-DIff
    setup <- filter(df,k==kth) %>%
      mutate(d = d==1 ) %>%
      panel.matrices(.,outcome = "y",
                     unit = "i",time = "t",treatment = "d")

    tau.hat[kth]  <- sc_estimate(setup$Y,setup$N0, setup$T0,
                                 method="sdid")

  }

  # Setup Array
  setup <- panel.array(df)
  est_prop <- sc_estimate(setup$Y,setup$N0, setup$T0,
                                   porp_dat=T,method="sdid")
  
  return(list("did"=ate, "sdid"=tau.hat, "sdid_prop"= c(est_prop)))

}

# Simulation
Nrep <- 1000
tau <- c(0.5,-0.5,0,0)
res_est <- list()


for(i in 1:Nrep){

  # Generate Data
  df <-  simulateDGP_multi(K=4,tau = tau, additive = F)$dat
 
  # Calculate in-sample ATT
  true_ATT <- data.frame("k"=1:length(tau),"tau"= tau) %>% 
    right_join(.,df) %>%
    # filter(treated == 1,t >9 ) %>%  # weiß ich nicht genau
    group_by(k) %>%
    summarise("ATT_insample" =  mean(y*(1-y) * tau)  )
 
  # Estimate Models
  res <- estmation_step(df, K = 4)
  df_res <- rbind(do.call(rbind, res), "att"= true_ATT$ATT_insample)

  res_est[[i]] <-  data.frame(df_res)
  res_est[[i]]$est_mean <- apply(res_est[[i]],1,mean)
  res_est[[i]]$est <- rownames(res_est[[i]])
  rownames(res_est[[i]]) <- NULL

}



# Estimate
df_plot <- do.call(rbind,res_est) %>%
  mutate(est = factor(est,levels = c("did","sdid","sdid_prop","att"),
                      labels = c("Two-way-fixed Effects",
                                  "Syn Diff-in-Diff",
                                  "Syn Diff-in-Diff Prop",
                                  "ATT")))


# Statistics
df_plot %>%
  group_by(est) %>%
  summarise_all(mean)


# Mean
df_plot %>%
  dplyr::select(est_mean,est) %>%
  group_by(est) %>%
  summarise("mean"=mean(est_mean), "sd" = sd(est_mean)) %>%
  ggplot()  +
  geom_pointrange(aes(x=est,
                      y=mean, ymin=mean+sd, ymax=mean-sd)) +
  coord_flip() +
  theme_minimal() +
  ylab("Mean of Estimates") +
  xlab("")


# Estimates (virtually makes no difference)
df_plot %>%
  dplyr::select(-est_mean) %>%
  reshape2::melt(.) %>%
  ggplot()  +
  geom_density(aes(value, color=est, fill=est), alpha=0.2) +
  geom_vline(xintercept=0, col="red", alpha=0.6) +
  facet_wrap(variable ~., scales="free") +
  theme_minimal() +
  xlab("Estimate of ATE") +
  ylab("")
