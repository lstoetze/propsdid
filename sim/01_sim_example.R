
  # Source Functions
  source("00_sim_func.R")

# Example === 

  # Generate Data
  set-seed(2893659135)
  df <- simulateDGP_multi(K=4,
                          tau = c(15,0,0,0),
                          T = 5, treat_t = 5, N=200, treated_n = 100,
                          set_delta =  0, lambda_set = 2,
                          gamma_sd = c(1,1,1,1), gamma_mu = c(0,0,0,0),
                          upsilon_sd = c(0.5,0,0,0), upsilon_mu = c(0,0,0,0),
                          gamma_update_sd = 0.1, upsilon_update_sd= 0.1,
                          epsilon_i_sd=0.2, epsilon_t_sd=0.5)$dat
  
  # Treatment Control
  df %>%
    group_by(treated,t,k) %>%
    summarise("Y" = mean(y)) %>%
    ungroup() %>% 
    mutate(k = factor(k, levels = 1:4,labels = paste("Proportion",1:4)),
           treated = factor(treated, levels = 1:0, labels = c("Treated","Control"))) %>%
    ggplot() +
    geom_line(aes(y=Y,x=as.factor(t),group=relevel(treated, "Treated"),col=(treated))) +
    facet_wrap(~ k) +
    annotate('rect', xmin = 4.5, xmax = 5, ymin = -Inf, ymax = Inf, fill = 'grey', alpha = 0.2) +
    theme_minimal() + xlab("Time") + ylab("Proportion") +
    theme(legend.title = element_blank()) +
    scale_color_grey()


  # Save Plot
  ggsave("fig_sim0.pdf", width=9, height=6)

# Individual time trends ====

  # Plot 10 single cases
  df %>%
    mutate(k = factor(k, levels = 1:4,labels = paste("Proportion",1:4)),
           treated = factor(treated, levels = 0:1, labels = c("Control","Treated"))) %>%
    filter(i %in% sample(1:200,10)) %>%
    ungroup() %>%
    ggplot() +
    geom_line(aes(y=y,x=t,group=(i),col=(treated))) +
    facet_wrap(~ k) +
    annotate('rect', xmin = 3.5, xmax = 5.5, ymin = -Inf, ymax = Inf, fill = 'grey', alpha = 0.2) +
    theme_minimal() + xlab("Time") + ylab("y") +
    theme(legend.title = element_blank()) +
    scale_color_grey()


# Estimation =====

  # Prepare Data
  df_est <- df %>% dplyr::select(i,t,k,y,d,treated)
  
  # TWFE
  m <- lm(y ~ d + as.factor(t) + as.factor(i), filter(df_est,k==1))
  est_twfeff <- coef(m)[2]
  
  # SDID
  setup <- panel.array(df_est)
  est_prop <- sc_estimate(setup$Y,setup$N0, setup$T0,
                          porp_dat=T,method="sdid")
  est_sdid <- est_prop[[1]]
  
  # TRUE ATT
  filter(df, k==1, t>=4) %>%
    summarise(mean(y1 - y0)) %>% pull() -> true_att


  # Print
  cat("True ATE:", round(true_att, 3),  
      "\nEst ATE sDID:", round(est_sdid,3),
      "\nEst ATE TWFE:", round(est_twfeff,3))

