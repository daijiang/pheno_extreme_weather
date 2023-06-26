source("code/00_pkg_functions.R")

if(!file.exists("data_output/d_for_stat.rds")){
  d = read_rds("data_output/df_25km_pheno_all.rds")
  
  names(d)
  n_distinct(d$sp) # 997
  n_distinct(d$id_cells) # 1,374
  
  # do all species have at least 3 estimates?
  spSum <- d %>% 
    group_by(sp) %>% 
    summarise(nEstimates = n()) %>% 
    filter(nEstimates >= 3)
  
  # remove species with less than 3 estimates
  d <- filter(d, sp %in% spSum$sp)
  
  n_distinct(d$sp) # 808
  n_distinct(d$id_cells) # 1,357
  
  table(d$yr)
  hist(d$onset)
  hist(d$offset)
  hist(d$duration)
  summary(d$duration)
  hist(log(d$duration))
  summary(d$n_obs)
  hist(d$n_obs)
  summary(d$n_days)
  hist(d$n_days)
  hist(log(d$n_days))
  hist(d$pop_25km_log10)
  hist(d$n_unusual_warm_days_before_onset)
  hist(log(d$n_unusual_warm_days_before_onset + 1))
  summary(d$n_unusual_warm_days_before_onset)
  hist(log(d$n_unusual_cold_days_before_onset + 1))
  summary(d$n_unusual_cold_days_before_onset)
  summary(d$n_unusual_dry_days_before_onset) # all 0
  summary(d$n_unusual_wet_days_before_onset)
  hist(d$n_unusual_wet_days_before_onset)
  hist(log(d$n_unusual_wet_days_before_onset + 1))
  summary(d$n_unusual_cold_days_within_duration)
  hist(log(d$n_unusual_cold_days_within_duration + 1))
  hist(log(d$n_unusual_warm_days_within_duration + 1))
  # lots of 0 
  
  hist(d$elev)
  summary(d$elev)
  
  hist(d$precip_current_yr)
  hist(log(d$precip_current_yr + 1))
  hist(sqrt(d$precip_current_yr))
  
  hist(d$precip_5_yr_ave)
  
  hist(d$tmean_5_yr_ave)
  hist(d$tmean_current_yr)
  
  plot(d$n_unusual_cold_days_before_onset, d$n_unusual_warm_days_before_onset)
  
  cor(d$n_unusual_cold_days_before_onset, d$n_unusual_warm_days_before_onset)
  
  # log transform things
  d = d |> 
    mutate(n_unusual_warm_days_total = n_unusual_warm_days_before_onset + n_unusual_warm_days_within_duration,
           n_unusual_cold_days_total = n_unusual_cold_days_before_onset + n_unusual_cold_days_within_duration,
           n_unusual_wet_days_total = n_unusual_wet_days_before_onset + n_unusual_wet_days_within_duration) |> 
    mutate(n_unusual_warm_days_before_onset_log = log(n_unusual_warm_days_before_onset + 1),
           n_unusual_cold_days_before_onset_log = log(n_unusual_cold_days_before_onset + 1),
           n_unusual_wet_days_before_onset_log = log(n_unusual_wet_days_before_onset + 1),
           n_unusual_warm_days_within_duration_log = log(n_unusual_warm_days_within_duration + 1),
           n_unusual_cold_days_within_duration_log = log(n_unusual_cold_days_within_duration + 1),
           n_unusual_wet_days_within_duration_log = log(n_unusual_wet_days_within_duration + 1),
           n_unusual_warm_days_total_log = log(n_unusual_warm_days_total + 1),
           n_unusual_cold_days_total_log = log(n_unusual_cold_days_total + 1),
           n_unusual_wet_days_total_log = log(n_unusual_wet_days_total + 1),
           n_days_log = log(n_days),
           duration_log = log(duration)
    )
  
  # scale 
  d2 = d |> mutate_at(c("n_unusual_warm_days_before_onset_log", 
                        "n_unusual_cold_days_before_onset_log",
                        "n_unusual_wet_days_before_onset_log",
                        "n_unusual_warm_days_within_duration_log",
                        "n_unusual_cold_days_within_duration_log",
                        "n_unusual_wet_days_within_duration_log",
                        "n_unusual_warm_days_total_log",
                        "n_unusual_cold_days_total_log",
                        "n_unusual_wet_days_total_log",
                        "precip_current_yr",
                        "tmean_current_yr",
                        "precip_5_yr_ave",
                        "tmean_5_yr_ave", 
                        "elev", "n_days_log",
                        "pop_25km_log10"), 
                      ~(scale(.) %>% as.vector))
  
  d2 = dplyr::select(d2, yr, id_cells, sp, n_days_log, onset, offset, duration,
                     pop_25km_log10, elev, tmean_5_yr_ave, precip_5_yr_ave,
                     ends_with("_log"))
  
  d2 = drop_na(d2, pop_25km_log10)
  saveRDS(d2, "data_output/d_for_stat.rds")
  
  sp_dom = d2 |> 
    group_by(sp) |> 
    tally() |> 
    arrange(desc(n)) |> 
    filter(n >= 30) |> 
    pull(sp)
  
  d_dom = filter(d2, sp %in% sp_dom) 
  
  
  # yearly different?
  ggplot(d_dom, aes(x = as.factor(yr), y = onset)) +
    geom_boxplot() +
    facet_wrap(~sp, scales = "free")
  
  ggplot(d_dom, aes(x = as.factor(yr), y = offset)) +
    geom_boxplot() +
    facet_wrap(~sp, scales = "free")
  
  ggplot(d_dom, aes(x = as.factor(yr), y = duration)) +
    geom_boxplot() +
    facet_wrap(~sp, scales = "free")
  
  # correlations among predictors?
  cor(dplyr::select(d2, ends_with("_log"), tmean_5_yr_ave, precip_5_yr_ave, pop_25km_log10)) |> 
    corrplot::corrplot(method = 'number') 
  
  # collection effort?
  ggplot(d_dom, aes(x = n_days_log, y = onset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position = "none")
  
  # relationships between variables and onset
  ggplot(d_dom, aes(x = tmean_5_yr_ave, y = onset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position = "none") # earlier in warm area
  
  ggplot(d_dom, aes(x = precip_5_yr_ave, y = onset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position = "none") # later in wet area
  
  ggplot(d_dom, aes(x = pop_25km_log10, y = onset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position = "none") # earlier in urban area
  
  ggplot(d_dom, aes(x = n_unusual_warm_days_before_onset_log, y = onset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", alpha = 0.5) +
    theme(legend.position = "none") # earlier

  ggplot(d_dom, aes(x = n_unusual_cold_days_before_onset_log, y = onset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", alpha = 0.5) +
    theme(legend.position = "none") # later, not clear
  
  ggplot(d_dom, aes(x = n_unusual_wet_days_before_onset_log, y = onset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", alpha = 0.5) +
    theme(legend.position = "none") # later, not clear
  
  # relationships between variables and offset
  ggplot(d_dom, aes(x = tmean_5_yr_ave, y = offset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position = "none") # earlier in warm area
  
  ggplot(d_dom, aes(x = precip_5_yr_ave, y = offset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position = "none") # mostly delay, later in wetter area
  
  ggplot(d_dom, aes(x = pop_25km_log10, y = offset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position = "none") # delay?
  
  ggplot(d_dom, aes(x = n_unusual_warm_days_within_duration_log, y = offset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", alpha = 0.5) +
    theme(legend.position = "none") # delay
  
  ggplot(d_dom, aes(x = n_unusual_cold_days_within_duration_log, y = offset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", alpha = 0.5) +
    theme(legend.position = "none") # delay???
  
  ggplot(d_dom, aes(x = n_unusual_wet_days_within_duration_log, y = offset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", alpha = 0.5) +
    theme(legend.position = "none") # delay??
  
  # relationships between variables and duration
  ggplot(d_dom, aes(x = tmean_5_yr_ave, y = duration, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    scale_y_log10() +
    theme(legend.position = "none") # mostly longer in warm area
  
  ggplot(d_dom, aes(x = precip_5_yr_ave, y = duration, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    scale_y_log10() +
    theme(legend.position = "none") # mostly longer in wetter area
  
  ggplot(d_dom, aes(x = pop_25km_log10, y = duration, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    scale_y_log10() +
    theme(legend.position = "none") # mostly longer in urban area
  
  ggplot(d_dom, aes(x = n_unusual_warm_days_total_log, y = duration, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    scale_y_log10() +
    theme(legend.position = "none") # longer
  
  ggplot(d_dom, aes(x = n_unusual_cold_days_total_log, y = duration, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    scale_y_log10() +
    theme(legend.position = "none") # longer????
  
  ggplot(d_dom, aes(x = n_unusual_wet_days_total_log, y = duration, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    scale_y_log10() +
    theme(legend.position = "none") # longer??
} else {
  d2 = readRDS("data_output/d_for_stat.rds")
}



