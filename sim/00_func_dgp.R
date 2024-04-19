#' ---
#' title: "Synthetic Difference-in-Difference for Proportions"
#' description: "Setup Functions for Simulations"
#' author: "Lukas F. Stoetzer"
#' ---


library(propsdid)
library(tidyverse)



# Makes illustartive plot
df <- simulateDGP_multi(K=4,tau = c(0.5,-0.5,0,0),additive = T,ratiosd_GU = 0.1,
                        T = 5, treat_t = 4, N=100, treated_n = 80 )$dat
df %>%
  group_by(treated,t,k) %>%
  summarise("Y" = mean(y)) %>%
  ungroup() %>% 
  mutate(k = factor(k, levels = 1:4,labels = paste("Proportion",1:4)),
         treated = factor(treated, levels = 0:1, labels = c("Control","Treated"))) %>%
  ggplot() +
  geom_line(aes(y=Y,x=t,group=(treated),col=(treated))) +
  facet_wrap(~ k) +
  annotate('rect', xmin = 3.5, xmax = 5.5, ymin = -Inf, ymax = Inf, fill = 'grey', alpha = 0.2) +
  theme_minimal() + xlab("Time") + ylab("y") +
  theme(legend.title = element_blank()) +
  scale_color_grey()

ggsave("fig_sim0.pdf",width = 9,height = 6)


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
  df <-  simulateDGP_multi(K=4,tau = c(0.5,-0.5,0,0),additive = T,ratiosd_GU = 0.1,
                           T = 5, treat_t = 4, N=100, treated_n = 80)$dat
 
  # Calculate in-sample ATT and ATE based on counterfactuals
  att <- df %>% filter(treated == 1, t>3 ) %>%
    group_by(k) %>%
    summarise("att"=mean(y1 - y0)) %>%
    pull("att")
 
  ate <- df %>% 
    group_by(k) %>%
    summarise("ate"=mean(y1 - y0)) %>%
    pull("ate")
  
  # Estimate Models
  df_sub <- df %>% select(i,t,k,y,d,treated)
  res <- estmation_step(df_sub , K = 4)

  res_est[[i]] <-  data.frame(t(do.call("rbind", res)))
  res_est[[i]]$ate <- ate
  res_est[[i]]$att <- att
  res_est[[i]]$k <- 1:4
  res_est[[i]]$sample <-  i
  rownames(res_est[[i]]) <- NULL

}


# SUm Constriant 
df_sum <- do.call(rbind,res_est)  %>%
  select(did   ,     sdid   ,  sdid_prop,k, sample) %>%
  pivot_longer(c("did","sdid", "sdid_prop"), names_to = "method") %>%
  mutate(method = ifelse(method == "sdid_prop","sdidp",method ))%>%
  group_by(method,sample) %>%
  summarise("value" = sum(value)) %>%
  group_by(method) %>%
  summarise("value"=sd(value)) %>%
  pivot_wider(names_from = "method")  %>%
  select(-did) %>% mutate(type="sum_cons")

do.call(rbind,res_est)  %>%
  select(did   ,     sdid   ,  sdid_prop,k, sample) %>%
  pivot_longer(c("did","sdid", "sdid_prop"), names_to = "method") %>%
  mutate(method = ifelse(method == "sdid_prop","sdidp",method ))%>%
  group_by(method,sample) %>%
  summarise("value" = abs(sum(value)) > 0.03) %>%
  group_by(method) %>%
  summarise("value"=mean(value))

# Get eValuation Metrics
df_bias <- do.call(rbind,res_est) %>%
  mutate(eval_did_bias_ate = ate - did,
         eval_did_bias_att = att - did,
         eval_sdid_bias_ate = ate - sdid,
         eval_sdid_bias_att = att - sdid,
         eval_sdidp_bias_ate = ate - sdid_prop,
         eval_sdidp_bias_att = att - sdid_prop,
         
         eval_did_rmse_ate = sqrt((ate - did)^2),
         eval_did_rmse_att = sqrt((att - did)^2),
         eval_sdid_rmse_ate = sqrt((ate - sdid)^2),
         eval_sdid_rmse_att = sqrt((att - sdid)^2),
         eval_sdidp_rmse_ate = sqrt((ate - sdid_prop)^2),
         eval_sdidp_rmse_att  = sqrt((att - sdid_prop)^2)
         ) %>%
  select(starts_with("eval_"),k, sample) %>%
  pivot_longer(starts_with("eval_")) %>%
  separate(name, into = c(NA,"method","type","comparision"))


# Table with Stats Bias and 
df_bias <- df_bias %>%
    group_by(type,method) %>%
    summarise("value"=mean(value)) %>%
    pivot_wider(names_from = "method")  %>%
    select(-did)
  

  
  bind_rows(df_bias,df_sum) %>%
    xtable::xtable(digits = 4)



