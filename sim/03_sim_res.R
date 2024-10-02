
  # Packages
  source("00_setup.R")
  
  # Load results
  res_est <- readRDS("Sim_res.RDS") %>% 
    mutate(treatment_selection = case_when(treatment_selection == 1 ~ "level",
                                           treatment_selection == 0 ~ "trend"))


# Sum Constraint  ============

  # Add synthetic control

  # Calculate
  res_sumcons <- res_est  %>%
    dplyr::select(did,sdid,sdid_prop,k, sample, time_periods,
                  treatment_period,units, treatment_proportion, treatment_selection) %>%
    pivot_longer(c("did","sdid", "sdid_prop"), names_to = "method") %>%
    mutate(method = ifelse(method == "sdid_prop","sdidp",method ))%>%
    group_by(method,sample,time_periods,
             treatment_period,units, treatment_proportion, treatment_selection) %>%
    summarise("value" = abs(sum(value))) %>%
    group_by(method,treatment_selection) %>%
    summarise("sum_constraint"=round(mean(value),3)) 
  
  # Table sumcontraint
  res_sumcons


# Bias and RMSE  ============  
  
  # Calculate Evaluation Metrics
  df_bias <- res_est %>%
    mutate(
           # Standard Deviation
            eval_did_sd_ate = sd(did),
            eval_did_sd_att = sd(did),
            eval_sdid_sd_ate = sd(sdid),
            eval_sdid_sd_att = sd(sdid),
            eval_sdidp_sd_ate = sd(sdid_prop),
            eval_sdidp_sd_att = sd(sdid_prop),
      
           # In sample bias
           eval_did_bias_ate = (ate - did),
           eval_did_bias_att = (att - did),
           eval_sdid_bias_ate = (ate - sdid),
           eval_sdid_bias_att = (att - sdid),
           eval_sdidp_bias_ate = (ate - sdid_prop),
           eval_sdidp_bias_att = (att - sdid_prop),
           
           # In sample RMSE
           eval_did_rmse_ate = ((ate - did)^2),
           eval_did_rmse_att = ((att - did)^2),
           eval_sdid_rmse_ate = ((ate - sdid)^2),
           eval_sdid_rmse_att = ((att - sdid)^2),
           eval_sdidp_rmse_ate = ((ate - sdid_prop)^2),
           eval_sdidp_rmse_att  = ((att - sdid_prop)^2)
    ) %>%
    dplyr::select(starts_with("eval_"),k, sample,time_periods,
                  treatment_period,units, treatment_proportion, treatment_selection) %>%
    pivot_longer(starts_with("eval_")) %>%
    separate(name, into = c(NA,"method","type","comparision")) %>% 
    filter(comparision == "att")

  # Aggregate for treatment_selection and k 
  df_bias_agg <- df_bias %>%
    pivot_wider(names_from = type, values_from = value) %>%
    group_by(method,comparision,treatment_selection) %>%
    summarise("sd"=(mean(sd)),
              "rmse"= sqrt(mean(rmse)),
              "bias" = mean(abs(bias))) 

  # Table for first category
  df_bias_agg_tab <- df_bias_agg  %>% 
    arrange(treatment_selection) %>% 
    left_join(.,res_sumcons) %>%
    ungroup() %>%
    dplyr::select(-comparision)

  df_bias_agg_tab
  print(xtable(df_bias_agg_tab, digits = 3),include.rownames = F)
  
  
  # Aggregate for different scenarios
  df_bias_agg_all <- df_bias %>%
    group_by(type,method,comparision,k,
             time_periods,
             treatment_period,units, treatment_proportion, treatment_selection) %>%
    summarise("mean_value"=(mean(value)),
              "n" = n(),
              "se" = sd(value)/sqrt(n)) %>% 
    mutate_at(c("time_periods", "treatment_period","units","treatment_proportion","treatment_selection"), as.factor )
  
  # Figure
  ggplot(filter(df_bias_agg_all, comparision == "att", type=="bias")) +
    geom_point(aes(y=mean_value,x=method, col= paste(time_periods, units))) +
    facet_grid(treatment_selection~ k , scales="free") +
    theme_bw()
  
  df_bias_agg_all <- df_bias_agg_all %>%
    mutate(obs = as.numeric(as.character(time_periods)) * as.numeric(as.character(units)))

  ggplot(filter(df_bias_agg_all, comparision == "att", type=="rmse")) +
    geom_point(aes(y=mean_value,x=log(obs), col=method)) +
    geom_smooth(aes(y=mean_value,x=log(obs), group=method), method="lm") +
    facet_wrap(~ method) +
    theme_bw()
  
  summary(lm(mean_value ~ log(obs)*method, 
             filter(df_bias_agg_all, method %in% c("sdid","sdidp"))))
  
  
# Sampling Distribution  ============ 

  # Shape to long format
  df_dist <- res_est %>%
    pivot_longer(did:sdid_prop) %>%
    mutate(deviation = value-att)

  # Plot for k==1
  ggplot(filter(df_dist, k==1)) +
    geom_vline(xintercept = 0, col="red", alpha=1) +
    geom_histogram(aes(x=(deviation), fill=name), alpha=0.7) +
    facet_grid(treatment_proportion + units + time_periods ~treatment_selection, scales="free") +
    theme_minimal()
