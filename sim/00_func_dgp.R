#' ---
#' title: "Synthetic Difference-in-Difference for Proportions"
#' description: "Setup Functions for Simulations"
#' author: "Lukas F. Stoetzer"
#' ---


library(propsdid)



simulateDGP_multi(K=4,tau_val = c(1,-2,0,0),additive = F)

# Makes some plots
df <- dpg(K=4,tau_val = c(1,-2,0,0),additive = F)
library(tidyverse)
df %>%
  group_by(treated,t,k) %>%
  summarise("mean_y" = mean(y)) %>%
  ggplot() +
  geom_line(aes(y=mean_y,x=t,group=as.factor(treated),col=as.factor(treated))) +
  facet_wrap(~ k, scales = "free") +
  theme_minimal()


# library
library(synthdidprop) # need to install from tar.tz


setup <- panel.array(df)
est <- synthdidprop::sc_estimate(setup$Y,setup$N0, setup$T0,
                   porp_dat=T,method="sdid")

# Estimation -fixed effects


estmation_step  <- function(df){

  ate <- NULL
  tau.hat <- NULL
  for(kth in 1:3){
    # Two-way fixed effects
    ate[kth]  <- coef(lm(y ~ d + as.factor(t) + as.factor(i), filter(df,k==kth)))["d"]

    # SynDiff-in-DIff
    setup <- filter(df,k==kth) %>%
      mutate(d = d==1 ) %>%
      panel.matrices(.,outcome = "y",
                     unit = "i",time = "t",treatment = "d")

    tau.hat[kth]  <- synthdidprop::synthdid_estimate(setup$Y, setup$N0, setup$T0)

  }

  # Setup Array
  setup <- panel.array(df)
  est_prop <- synthdidprop::sc_estimate(setup$Y,setup$N0, setup$T0,
                                   porp_dat=T,method="sdid")

  return(list("did"=ate, "sdid"=tau.hat, "sdid_prop"= c(est_prop)))

}



# Simulation

Nrep <- 100
res_est <- list()

for(i in 1:Nrep){

  # Generate Data
  df <- dpg(additive = F)

  # Estimate Models
  res <- estmation_step(df)

  res_est[[i]] <-  data.frame(do.call(rbind, res))
  res_est[[i]]$est_mean <- apply(res_est[[i]],1,mean)
  res_est[[i]]$est <- rownames(res_est[[i]])
  rownames(res_est[[i]]) <- NULL

}


# Constraint
reshape2::melt(res_mean)  %>%
  ggplot() +
  geom_histogram(aes(value)) +
  facet_grid(~Var2) +
  theme_minimal() +
  xlab("Mean of Treatment Effects over Proportions") +
  ylab("")



# Estimate
df_plot <- do.call(rbind,res_est) %>%
  mutate(est = factor(est,levels = c("did","sdid","sdid_prop"),
                      labels = c("Two-way-fixed Effects",
                                                         "Syn Diff-in-Diff",
                                                         "Syn Diff-in-Diff Prop")))


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
  facet_wrap(variable ~.) +
  theme_minimal() +
  xlab("Estimate of ATE") +
  ylab("")
