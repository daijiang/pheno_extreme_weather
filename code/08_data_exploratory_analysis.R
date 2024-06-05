source("code/00_pkg_functions.R")

if(!file.exists("data_output/d_for_stat.rds")){
  d = read_rds("data_output/df_25km_pheno_all.rds")

  
  for(i in 1:ncol(d)){
    cat(i, "\t")
    print(any(is.na(d[,i])))
  } # pop
  
  d = dplyr::select(d, -pop_25km, -pop_25km_log10)
  
  d = drop_na(d)
  names(d)
  
  # remove migrotory insects?
  insect_mig = read_csv("data/insect_sp_mig.csv")
  insect_mig_rm = filter(insect_mig, migratory == "y" | (!is.na(notes)))
  any(insect_mig_rm$sp %in% d$sp) # FALSE, confirmed removed
  
  # remove flying year around insects?
  insect_traits = read_csv("data_output/insect_traits.csv")
  insect_yeararound = filter(insect_traits, grepl("Flies year round", notes))$sp
  d = filter(d, !sp %in% insect_yeararound)
  
  yr_around = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1kGEc2E2qlJe08Q56bbsAmWC1vEZMdA_ViwaxARwE12I/edit#gid=2080482793")
  yr_around_sp = filter(yr_around, flies.year.round == "y")$sp
  d = filter(d, !sp %in% yr_around_sp)
  
  # do all species have at least 3 estimates?
  spSum <- d %>% 
    group_by(sp) %>% 
    summarise(nEstimates = n()) %>% 
    filter(nEstimates >= 3)
  
  # remove species with less than 3 estimates
  d <- filter(d, sp %in% spSum$sp)
  
  dplyr::select(d, taxon, id_cells) |> 
    distinct() |> 
    group_by(id_cells) |> 
    tally() |> 
    filter(n > 1) |> 
    pull(id_cells) -> cells_w_both
  
  # to only look at insects data whose id_cells with plant data
  d = filter(d, id_cells %in% cells_w_both)
  
  d |> 
    group_by(taxon) |> 
    summarise(n_sp = n_distinct(sp),
              n_cells = n_distinct(id_cells))
  
  # taxon   n_sp n_cells
  # <chr>  <int>   <int>
  # 1 insect   172     766
  # 2 plant    581     766
  
  # taxon   n_sp n_cells
  # <chr>  <int>   <int>
  # 1 insect   199     882
  # 2 plant    550     882
  
  # taxon   n_sp n_cells
  # <chr>  <int>   <int>
  #   1 insect   201     885
  # 2 plant    565     885
  
  table(d$yr)
  ggplot(d, aes(x = onset, fill = taxon)) +
    geom_histogram(alpha = 0.8)
  
  ggplot(d, aes(x = onset, xend = offset, y = latitude_center, yend = latitude_center, color = taxon)) +
    geom_segment(alpha = 0.5) +
    labs(x = "Day of Year")
  
  ggplot(d, aes(x = offset, fill = taxon)) +
    geom_histogram(alpha = 0.8)
  
  ggplot(d, aes(x = duration, fill = taxon)) +
    geom_histogram(alpha = 0.8)
  
  ggplot(mutate(d, duration = log(duration)), aes(x = duration, fill = taxon)) +
    geom_histogram(alpha = 0.8)
  
  summary(d$duration)
  hist(d$duration)
  hist(log(d$duration))
  summary(d$n_obs)
  hist(d$n_obs)
  hist(log(d$n_obs))
  summary(d$n_days)
  hist(d$n_days)
  hist(log(d$n_days))
  # hist(d$pop_25km_log10)
  hist(d$n_extreme_warm_days_before_onset)
  summary(d$n_extreme_warm_days_before_onset)
  hist(log(d$n_extreme_warm_days_before_onset + 1))
  mean(d$n_extreme_warm_days_before_onset == 0)
  
  hist(d$n_extreme_warm_days_within_duration)
  summary(d$n_extreme_warm_days_within_duration)
  hist(log(d$n_extreme_warm_days_within_duration + 1))
  mean(d$n_extreme_warm_days_within_duration == 0)
  
  hist(d$n_extreme_warm_days_total)
  summary(d$n_extreme_warm_days_total)
  hist(log(d$n_extreme_warm_days_total + 1))
  mean(d$n_extreme_warm_days_total == 0)
  
  hist(d$n_extreme_cold_days_before_onset)
  summary(d$n_extreme_cold_days_before_onset)
  hist(log(d$n_extreme_cold_days_before_onset + 1))
  mean(d$n_extreme_cold_days_before_onset == 0)
  
  hist(d$n_extreme_cold_days_within_duration)
  summary(d$n_extreme_cold_days_within_duration)
  hist(log(d$n_extreme_cold_days_within_duration + 1))
  mean(d$n_extreme_cold_days_within_duration == 0)
  
  hist(d$n_extreme_cold_days_total)
  summary(d$n_extreme_cold_days_total)
  hist(log(d$n_extreme_cold_days_total + 1))
  mean(d$n_extreme_cold_days_total == 0)
  
  hist(d$n_extreme_dry_days_before_onset)
  summary(d$n_extreme_dry_days_before_onset)
  hist(log(d$n_extreme_dry_days_before_onset + 1))
  mean(d$n_extreme_dry_days_before_onset == 0)
  
  hist(d$n_extreme_dry_days_within_duration)
  summary(d$n_extreme_dry_days_within_duration)
  hist(log(d$n_extreme_dry_days_within_duration + 1))
  mean(d$n_extreme_dry_days_within_duration == 0)
  
  hist(d$n_extreme_dry_days_total)
  summary(d$n_extreme_dry_days_total)
  hist(log(d$n_extreme_dry_days_total + 1))
  mean(d$n_extreme_dry_days_total == 0)
  
  
  hist(d$n_extreme_wet_days_before_onset)
  summary(d$n_extreme_wet_days_before_onset)
  hist(log(d$n_extreme_wet_days_before_onset + 1))
  mean(d$n_extreme_wet_days_before_onset == 0)
  
  hist(d$n_extreme_wet_days_within_duration)
  summary(d$n_extreme_wet_days_within_duration)
  hist(log(d$n_extreme_wet_days_within_duration + 1))
  mean(d$n_extreme_wet_days_within_duration == 0)
  
  hist(d$n_extreme_wet_days_total)
  summary(d$n_extreme_wet_days_total)
  hist(log(d$n_extreme_wet_days_total + 1))
  mean(d$n_extreme_wet_days_total == 0)
  

  hist(d$sample_entropy_extreme_warm_before_onset)
  hist(d$sample_entropy_extreme_cold_before_onset)
  hist(d$sample_entropy_temp_before_onset)
  
  hist(d$sample_entropy_extreme_wet_before_onset)
  hist(d$sample_entropy_extreme_dry_before_onset)
  hist(d$sample_entropy_precip_before_onset)
  
  hist(d$sample_entropy_extreme_warm_within_duration)
  hist(d$sample_entropy_extreme_cold_within_duration)
  hist(d$sample_entropy_temp_within_duration)
  
  hist(d$sample_entropy_extreme_wet_within_duration)
  hist(d$sample_entropy_extreme_dry_within_duration)
  hist(d$sample_entropy_precip_within_duration)
  
  
  hist(d$sample_entropy_extreme_warm_total)
  hist(d$sample_entropy_extreme_cold_total)
  hist(d$sample_entropy_temp_total)
  
  hist(d$sample_entropy_extreme_wet_total)
  hist(d$sample_entropy_extreme_dry_total)
  hist(d$sample_entropy_precip_total)
  
  plot(d$n_extreme_warm_days_before_onset, d$sample_entropy_extreme_warm_before_onset)
  cor(d$n_extreme_warm_days_before_onset, d$sample_entropy_extreme_warm_before_onset) # 0.87
  plot(d$n_extreme_warm_days_before_onset, d$sample_entropy_temp_before_onset) 
  cor(d$n_extreme_warm_days_before_onset, d$sample_entropy_temp_before_onset) # 0.68
  
  plot(d$n_extreme_cold_days_before_onset, d$sample_entropy_extreme_cold_before_onset)
  cor(d$n_extreme_cold_days_before_onset, d$sample_entropy_extreme_cold_before_onset) # 0.87
  
  plot(d$n_extreme_dry_days_before_onset, d$sample_entropy_extreme_dry_before_onset)
  cor(d$n_extreme_dry_days_before_onset, d$sample_entropy_extreme_dry_before_onset) # 0.49 hump shape
  plot(d$n_extreme_dry_days_before_onset, d$sample_entropy_precip_before_onset)
  cor(d$n_extreme_dry_days_before_onset, d$sample_entropy_precip_before_onset) # 0.25 hump shape
  cor(d$n_extreme_wet_days_before_onset, d$sample_entropy_precip_before_onset) # 0.39 hump shape
  plot(d$n_extreme_wet_days_before_onset, d$sample_entropy_precip_before_onset) # 0.39 hump shape
  
  plot(d$n_extreme_wet_days_before_onset, d$sample_entropy_extreme_wet_before_onset)
  cor(d$n_extreme_wet_days_before_onset, d$sample_entropy_extreme_wet_before_onset) # 0.83
  
  plot(d$n_extreme_warm_days_total, d$sample_entropy_extreme_warm_total)
  cor(d$n_extreme_warm_days_total, d$sample_entropy_extreme_warm_total) # 0.74
  plot(d$n_extreme_warm_days_total, d$sample_entropy_temp_total) 
  cor(d$n_extreme_warm_days_total, d$sample_entropy_temp_total) # 0.61
  
  
  hist(d$precip_current_yr)
  hist(log(d$precip_current_yr + 1))
  hist(sqrt(d$precip_current_yr))
  
  hist(d$precip_5_yr_ave)
  
  hist(d$tmean_5_yr_ave)
  hist(d$tmean_current_yr)
  
  plot(d$n_extreme_cold_days_before_onset, d$n_extreme_warm_days_before_onset)
  
  cor(d$n_extreme_cold_days_before_onset, d$n_extreme_warm_days_before_onset)
  
  d = d |> 
    mutate(prop_extreme_warm_days_before_onset_logit = logit_trans(n_extreme_warm_days_before_onset / 61),
           prop_extreme_cold_days_before_onset_logit = logit_trans(n_extreme_cold_days_before_onset / 61),
           prop_extreme_wet_days_before_onset_logit = logit_trans(n_extreme_wet_days_before_onset / 61),
           prop_extreme_dry_days_before_onset_logit = logit_trans(n_extreme_dry_days_before_onset / 61),
           prop_extreme_warm_days_within_duration_logit = logit_trans(n_extreme_warm_days_within_duration / duration),
           prop_extreme_cold_days_within_duration_logit = logit_trans(n_extreme_cold_days_within_duration / duration),
           prop_extreme_wet_days_within_duration_logit = logit_trans(n_extreme_wet_days_within_duration / duration),
           prop_extreme_dry_days_within_duration_logit = logit_trans(n_extreme_dry_days_within_duration / duration),
           prop_extreme_warm_days_total_logit = logit_trans(n_extreme_warm_days_total / (duration + 61)),
           prop_extreme_cold_days_total_logit = logit_trans(n_extreme_cold_days_total / (duration + 61)),
           prop_extreme_wet_days_total_logit = logit_trans(n_extreme_wet_days_total / (duration + 61)),
           prop_extreme_dry_days_total_logit = logit_trans(n_extreme_dry_days_total / (duration + 61))
           )
  
  # log transform things
  d = d |> 
    mutate(n_days_log = log(n_days),
           duration_log = log(duration)
    )
  
  names(d)
  saveRDS(d, "data_output/d_raw_for_stat.rds")
  
  d_ave_std = d[, c("prop_extreme_warm_days_before_onset_logit", 
        "prop_extreme_cold_days_before_onset_logit",
        "prop_extreme_wet_days_before_onset_logit",
        "prop_extreme_dry_days_before_onset_logit",
        "prop_extreme_warm_days_within_duration_logit",
        "prop_extreme_cold_days_within_duration_logit",
        "prop_extreme_wet_days_within_duration_logit",
        "prop_extreme_dry_days_within_duration_logit",
        "prop_extreme_warm_days_total_logit",
        "prop_extreme_cold_days_total_logit",
        "prop_extreme_wet_days_total_logit",
        "prop_extreme_dry_days_total_logit",
        "temp_seasonality_5_yr_ave",
        "precip_seasonality_5_yr_ave",
        "precip_5_yr_ave",
        "tmean_5_yr_ave")] |> 
    pivot_longer(cols = 1:16, names_to = "var", values_to = "value") |> 
    group_by(var) |> 
    summarise(ave = mean(value), std = sd(value), .groups = "drop") |> 
    mutate(up_1 = ave + std, low_1 = ave - std,
           up_1.5 = ave + 1.5 * std, low_1.5 = ave - 1.5 * std)
  
  saveRDS(d_ave_std, "data_output/d_ave_std.rds")
  
  
  d_ave_std2 = filter(d_ave_std, grepl("prop_", var), grepl("onset", var)) |> 
    mutate(low_2 = ave - 2 * std, up_2 = ave + 2 * std, 
           up_3 = ave + 3 * std, up_4 = ave + 4 * std) |> 
    mutate_if(is.numeric, logit_back_to_prop) |> 
    mutate_if(is.numeric, function(x) 61 * x)
  
  d_medium = d |> group_by(taxon, sp) |> 
    summarise(onset_medium = median(onset, na.rm = T),
              offset_medium = median(offset, na.rm = T), .groups = "drop")
  
  d_medium = mutate(d_medium, pheno_season = ifelse(onset_medium < 120, "early", # before May
                                                      ifelse(offset_medium > 240, "late", "middle")))
  
  count(d_medium, taxon, pheno_season)
  
  d_early = filter(d, sp %in% filter(d_medium, pheno_season == "early")$sp)
  d_late = filter(d, sp %in% filter(d_medium, pheno_season == "late")$sp)
  d_middle = filter(d, sp %in% filter(d_medium, pheno_season == "middle")$sp)
  
  d_early_ave_std = d_early[, c("prop_extreme_warm_days_before_onset_logit", 
                    "prop_extreme_cold_days_before_onset_logit",
                    "prop_extreme_wet_days_before_onset_logit",
                    "prop_extreme_dry_days_before_onset_logit",
                    "prop_extreme_warm_days_within_duration_logit",
                    "prop_extreme_cold_days_within_duration_logit",
                    "prop_extreme_wet_days_within_duration_logit",
                    "prop_extreme_dry_days_within_duration_logit",
                    "prop_extreme_warm_days_total_logit",
                    "prop_extreme_cold_days_total_logit",
                    "prop_extreme_wet_days_total_logit",
                    "prop_extreme_dry_days_total_logit",
                    "temp_seasonality_5_yr_ave",
                    "precip_seasonality_5_yr_ave",
                    "precip_5_yr_ave",
                    "tmean_5_yr_ave")] |> 
    pivot_longer(cols = 1:16, names_to = "var", values_to = "value") |> 
    group_by(var) |> 
    summarise(ave = mean(value), std = sd(value), .groups = "drop") |> 
    mutate(up_1 = ave + std, low_1 = ave - std,
           up_1.5 = ave + 1.5 * std, low_1.5 = ave - 1.5 * std)
  
  d_middle_ave_std = d_middle[, c("prop_extreme_warm_days_before_onset_logit", 
                                "prop_extreme_cold_days_before_onset_logit",
                                "prop_extreme_wet_days_before_onset_logit",
                                "prop_extreme_dry_days_before_onset_logit",
                                "prop_extreme_warm_days_within_duration_logit",
                                "prop_extreme_cold_days_within_duration_logit",
                                "prop_extreme_wet_days_within_duration_logit",
                                "prop_extreme_dry_days_within_duration_logit",
                                "prop_extreme_warm_days_total_logit",
                                "prop_extreme_cold_days_total_logit",
                                "prop_extreme_wet_days_total_logit",
                                "prop_extreme_dry_days_total_logit",
                                "temp_seasonality_5_yr_ave",
                                "precip_seasonality_5_yr_ave",
                                "precip_5_yr_ave",
                                "tmean_5_yr_ave")] |> 
    pivot_longer(cols = 1:16, names_to = "var", values_to = "value") |> 
    group_by(var) |> 
    summarise(ave = mean(value), std = sd(value), .groups = "drop") |> 
    mutate(up_1 = ave + std, low_1 = ave - std,
           up_1.5 = ave + 1.5 * std, low_1.5 = ave - 1.5 * std)
  
  d_late_ave_std = d_late[, c("prop_extreme_warm_days_before_onset_logit", 
                                "prop_extreme_cold_days_before_onset_logit",
                                "prop_extreme_wet_days_before_onset_logit",
                                "prop_extreme_dry_days_before_onset_logit",
                                "prop_extreme_warm_days_within_duration_logit",
                                "prop_extreme_cold_days_within_duration_logit",
                                "prop_extreme_wet_days_within_duration_logit",
                                "prop_extreme_dry_days_within_duration_logit",
                                "prop_extreme_warm_days_total_logit",
                                "prop_extreme_cold_days_total_logit",
                                "prop_extreme_wet_days_total_logit",
                                "prop_extreme_dry_days_total_logit",
                                "temp_seasonality_5_yr_ave",
                                "precip_seasonality_5_yr_ave",
                                "precip_5_yr_ave",
                                "tmean_5_yr_ave")] |> 
    pivot_longer(cols = 1:16, names_to = "var", values_to = "value") |> 
    group_by(var) |> 
    summarise(ave = mean(value), std = sd(value), .groups = "drop") |> 
    mutate(up_1 = ave + std, low_1 = ave - std,
           up_1.5 = ave + 1.5 * std, low_1.5 = ave - 1.5 * std)
  
  saveRDS(d_early_ave_std, "data_output/d_early_ave_std.rds")
  saveRDS(d_middle_ave_std, "data_output/d_middle_ave_std.rds")
  saveRDS(d_late_ave_std, "data_output/d_late_ave_std.rds")
  
  
  # scale 
  d2 = d |> mutate_at(c("prop_extreme_warm_days_before_onset_logit", 
                        "prop_extreme_cold_days_before_onset_logit",
                        "prop_extreme_wet_days_before_onset_logit",
                        "prop_extreme_dry_days_before_onset_logit",
                        "prop_extreme_warm_days_within_duration_logit",
                        "prop_extreme_cold_days_within_duration_logit",
                        "prop_extreme_wet_days_within_duration_logit",
                        "prop_extreme_dry_days_within_duration_logit",
                        "prop_extreme_warm_days_total_logit",
                        "prop_extreme_cold_days_total_logit",
                        "prop_extreme_wet_days_total_logit",
                        "prop_extreme_dry_days_total_logit",
                        "precip_current_yr",
                        "tmean_current_yr",
                        "temp_seasonality_5_yr_ave",
                        "precip_seasonality_5_yr_ave",
                        "precip_5_yr_ave",
                        "tmean_5_yr_ave", # "pop_25km_log10",
                        "elev", "n_days_log"), 
                      ~(scale(.) %>% as.vector))
  
  d2 = dplyr::select(d2, taxon, yr, id_cells, sp, n_days_log, onset, offset, duration_log,
                     elev, tmean_5_yr_ave, precip_5_yr_ave, precip_current_yr, tmean_current_yr,
                     temp_seasonality_5_yr_ave, precip_seasonality_5_yr_ave, # pop_25km_log10,
                     starts_with("prop"))
  
  saveRDS(d2, "data_output/d_for_stat.rds")
  
  d2_early = d_early |> mutate_at(c("prop_extreme_warm_days_before_onset_logit", 
                        "prop_extreme_cold_days_before_onset_logit",
                        "prop_extreme_wet_days_before_onset_logit",
                        "prop_extreme_dry_days_before_onset_logit",
                        "prop_extreme_warm_days_within_duration_logit",
                        "prop_extreme_cold_days_within_duration_logit",
                        "prop_extreme_wet_days_within_duration_logit",
                        "prop_extreme_dry_days_within_duration_logit",
                        "prop_extreme_warm_days_total_logit",
                        "prop_extreme_cold_days_total_logit",
                        "prop_extreme_wet_days_total_logit",
                        "prop_extreme_dry_days_total_logit",
                        "precip_current_yr",
                        "tmean_current_yr",
                        "temp_seasonality_5_yr_ave",
                        "precip_seasonality_5_yr_ave",
                        "precip_5_yr_ave",
                        "tmean_5_yr_ave", # "pop_25km_log10",
                        "elev", "n_days_log"), 
                      ~(scale(.) %>% as.vector))
  
  d2_early = dplyr::select(d2_early, taxon, yr, id_cells, sp, n_days_log, onset, offset, duration_log,
                     elev, tmean_5_yr_ave, precip_5_yr_ave, precip_current_yr, tmean_current_yr,
                     temp_seasonality_5_yr_ave, precip_seasonality_5_yr_ave, # pop_25km_log10,
                     starts_with("prop"))
  
  saveRDS(d2_early, "data_output/d_early_for_stat.rds")
  
  d2_middle = d_middle |> mutate_at(c("prop_extreme_warm_days_before_onset_logit", 
                                    "prop_extreme_cold_days_before_onset_logit",
                                    "prop_extreme_wet_days_before_onset_logit",
                                    "prop_extreme_dry_days_before_onset_logit",
                                    "prop_extreme_warm_days_within_duration_logit",
                                    "prop_extreme_cold_days_within_duration_logit",
                                    "prop_extreme_wet_days_within_duration_logit",
                                    "prop_extreme_dry_days_within_duration_logit",
                                    "prop_extreme_warm_days_total_logit",
                                    "prop_extreme_cold_days_total_logit",
                                    "prop_extreme_wet_days_total_logit",
                                    "prop_extreme_dry_days_total_logit",
                                    "precip_current_yr",
                                    "tmean_current_yr",
                                    "temp_seasonality_5_yr_ave",
                                    "precip_seasonality_5_yr_ave",
                                    "precip_5_yr_ave",
                                    "tmean_5_yr_ave", # "pop_25km_log10",
                                    "elev", "n_days_log"), 
                                  ~(scale(.) %>% as.vector))
  
  d2_middle = dplyr::select(d2_middle, taxon, yr, id_cells, sp, n_days_log, onset, offset, duration_log,
                           elev, tmean_5_yr_ave, precip_5_yr_ave, precip_current_yr, tmean_current_yr,
                           temp_seasonality_5_yr_ave, precip_seasonality_5_yr_ave, # pop_25km_log10,
                           starts_with("prop"))
  
  saveRDS(d2_middle, "data_output/d_middle_for_stat.rds")
  
  d2_late = d_late |> mutate_at(c("prop_extreme_warm_days_before_onset_logit", 
                                    "prop_extreme_cold_days_before_onset_logit",
                                    "prop_extreme_wet_days_before_onset_logit",
                                    "prop_extreme_dry_days_before_onset_logit",
                                    "prop_extreme_warm_days_within_duration_logit",
                                    "prop_extreme_cold_days_within_duration_logit",
                                    "prop_extreme_wet_days_within_duration_logit",
                                    "prop_extreme_dry_days_within_duration_logit",
                                    "prop_extreme_warm_days_total_logit",
                                    "prop_extreme_cold_days_total_logit",
                                    "prop_extreme_wet_days_total_logit",
                                    "prop_extreme_dry_days_total_logit",
                                    "precip_current_yr",
                                    "tmean_current_yr",
                                    "temp_seasonality_5_yr_ave",
                                    "precip_seasonality_5_yr_ave",
                                    "precip_5_yr_ave",
                                    "tmean_5_yr_ave", # "pop_25km_log10",
                                    "elev", "n_days_log"), 
                                  ~(scale(.) %>% as.vector))
  
  d2_late = dplyr::select(d2_late, taxon, yr, id_cells, sp, n_days_log, onset, offset, duration_log,
                           elev, tmean_5_yr_ave, precip_5_yr_ave, precip_current_yr, tmean_current_yr,
                           temp_seasonality_5_yr_ave, precip_seasonality_5_yr_ave, # pop_25km_log10,
                           starts_with("prop"))
  
  saveRDS(d2_late, "data_output/d_late_for_stat.rds")
  
  
  # species genus know to be pollinated by moths and butterflies
  g = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SoTGsd1KdqpJdKP1PuGezq0aPSdwh4IkCtZKQZ2wceI/edit#gid=254374704", sheet = 3)
  g = sort(unique(g$Genus))
  
  g2 = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SoTGsd1KdqpJdKP1PuGezq0aPSdwh4IkCtZKQZ2wceI/edit#gid=254374704", sheet = 4)
  g2 = sort(unique(g2$genus))
  
  intersect(g, g2)
  
  g2 = c(g, g2, filter(rtrees::classifications, taxon == "plant", family %in% c("Apiaceae", "Echinacea", "Asteraceae"))$genus) |> unique()
  
  plts = rtrees::sp_list_df(unique(filter(d, taxon == "plant")$sp), "plant")
  filter(plts, genus %in% g2) # 317 plants
  intersect(plts$genus, filter(rtrees::classifications, taxon == "plant", family %in% c("Apiaceae", "Echinacea"))$genus)
  
  sub_plant = filter(plts, genus %in% g2) |> 
    mutate(sp = str_replace_all(species, "_", " ")) |> 
    pull(sp)
  
  setdiff(unique(filter(d, taxon == "insect")$sp), filter(yr_around, Family != "Saturniidae")$sp)
  
  insect_rm = c(filter(yr_around, Family == "Saturniidae")$sp, 
                "Syssphinx heiligbrodti", "Actias luna", 
                "Anisota pellucida", "Anisota stigma", "Citheronia regalis",
                "Coloradia pandora", "Eupackardia calleta", "Hemileuca eglanterina",
                "Hyalophora cecropia", "Hyalophora euryalus", "Syssphinx hubbardi")
  
  
  d_subset = filter(d, !sp %in% insect_rm) |> filter(taxon == "insect" | sp %in% sub_plant)
  
  d_subset_ave_std = d_subset[, c("prop_extreme_warm_days_before_onset_logit", 
                              "prop_extreme_cold_days_before_onset_logit",
                              "prop_extreme_wet_days_before_onset_logit",
                              "prop_extreme_dry_days_before_onset_logit",
                              "prop_extreme_warm_days_within_duration_logit",
                              "prop_extreme_cold_days_within_duration_logit",
                              "prop_extreme_wet_days_within_duration_logit",
                              "prop_extreme_dry_days_within_duration_logit",
                              "prop_extreme_warm_days_total_logit",
                              "prop_extreme_cold_days_total_logit",
                              "prop_extreme_wet_days_total_logit",
                              "prop_extreme_dry_days_total_logit",
                              "temp_seasonality_5_yr_ave",
                              "precip_seasonality_5_yr_ave",
                              "precip_5_yr_ave",
                              "tmean_5_yr_ave")] |> 
    pivot_longer(cols = 1:16, names_to = "var", values_to = "value") |> 
    group_by(var) |> 
    summarise(ave = mean(value), std = sd(value), .groups = "drop") |> 
    mutate(up_1 = ave + std, low_1 = ave - std,
           up_1.5 = ave + 1.5 * std, low_1.5 = ave - 1.5 * std)
  
  saveRDS(d_subset_ave_std, "data_output/d_subset_ave_std.rds")
  
  d2_subset = d_subset |> mutate_at(c("prop_extreme_warm_days_before_onset_logit", 
                                  "prop_extreme_cold_days_before_onset_logit",
                                  "prop_extreme_wet_days_before_onset_logit",
                                  "prop_extreme_dry_days_before_onset_logit",
                                  "prop_extreme_warm_days_within_duration_logit",
                                  "prop_extreme_cold_days_within_duration_logit",
                                  "prop_extreme_wet_days_within_duration_logit",
                                  "prop_extreme_dry_days_within_duration_logit",
                                  "prop_extreme_warm_days_total_logit",
                                  "prop_extreme_cold_days_total_logit",
                                  "prop_extreme_wet_days_total_logit",
                                  "prop_extreme_dry_days_total_logit",
                                  "precip_current_yr",
                                  "tmean_current_yr",
                                  "temp_seasonality_5_yr_ave",
                                  "precip_seasonality_5_yr_ave",
                                  "precip_5_yr_ave",
                                  "tmean_5_yr_ave", # "pop_25km_log10",
                                  "elev", "n_days_log"), 
                                ~(scale(.) %>% as.vector))
  
  d2_subset = dplyr::select(d2_subset, taxon, yr, id_cells, sp, n_days_log, onset, offset, duration_log,
                          elev, tmean_5_yr_ave, precip_5_yr_ave, precip_current_yr, tmean_current_yr,
                          temp_seasonality_5_yr_ave, precip_seasonality_5_yr_ave, # pop_25km_log10,
                          starts_with("prop"))
  
  saveRDS(d2_subset, "data_output/d_subset_for_stat.rds")
  
  
  hist(d$prop_extreme_warm_days_before_onset_logit)
  hist(d$prop_extreme_cold_days_before_onset_logit)
  hist(d$prop_extreme_wet_days_before_onset_logit)
  hist(d$prop_extreme_dry_days_before_onset_logit)
  
  sp_dom = d2 |> 
    group_by(sp) |> 
    tally() |> 
    arrange(desc(n)) |> 
    filter(n >= 30) |> 
    pull(sp)
  
  d_dom = filter(d2, sp %in% sp_dom) 
  
  
  # yearly different?
  ggplot(filter(d_dom, taxon == "plant"), aes(x = as.factor(yr), y = onset)) +
    geom_boxplot() +
    facet_wrap(~sp, scales = "free")
  
  ggplot(filter(d_dom, taxon == "insect"), aes(x = as.factor(yr), y = onset)) +
    geom_boxplot() +
    facet_wrap(~sp, scales = "free")
  
  ggplot(filter(d_dom, taxon == "plant"), aes(x = as.factor(yr), y = duration)) +
    geom_boxplot() +
    facet_wrap(~sp, scales = "free")
  
  ggplot(filter(d_dom, taxon == "insect"), aes(x = as.factor(yr), y = duration)) +
    geom_boxplot() +
    facet_wrap(~sp, scales = "free")
  
  # correlations among predictors?
  cor(distinct(dplyr::select(d2, ends_with("_logit"), tmean_5_yr_ave, precip_5_yr_ave)) |> 
        drop_na()) |> 
    corrplot::corrplot(method = 'number') 
  
  # collection effort?
  ggplot(d_dom, aes(x = n_days_log, y = onset, color = sp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~taxon) +
    theme(legend.position = "none") 
  # more for insects
  
  
  # relationships between variables and onset
  ggplot(d_dom, aes(x = tmean_5_yr_ave, y = onset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # earlier in warm area
  
  ggplot(d_dom, aes(x = precip_5_yr_ave, y = onset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # later in wet area
  
  
  ggplot(d_dom, aes(x = prop_extreme_warm_days_before_onset_logit, y = onset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # earlier

  ggplot(d_dom, aes(x = prop_extreme_cold_days_before_onset_logit, y = onset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # later, not clear
  
  ggplot(d_dom, aes(x = prop_extreme_wet_days_before_onset_logit, y = onset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # later, not clear
  
  ggplot(d_dom, aes(x = prop_extreme_dry_days_before_onset_logit, y = onset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # earlier?, not clear
  
  # relationships between variables and offset
  ggplot(d_dom, aes(x = tmean_5_yr_ave, y = offset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # earlier in warm area
  
  ggplot(d_dom, aes(x = precip_5_yr_ave, y = offset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # mostly delay, later in wetter area
  
  ggplot(d_dom, aes(x = prop_extreme_warm_days_within_duration_log, y = offset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # delay
  
  ggplot(d_dom, aes(x = prop_extreme_cold_days_within_duration_log, y = offset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # delay???
  
  ggplot(d_dom, aes(x = prop_extreme_wet_days_within_duration_log, y = offset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # delay??
  
  ggplot(d_dom, aes(x = prop_extreme_dry_days_within_duration_log, y = offset, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # delay??
  
  # relationships between variables and duration
  ggplot(d_dom, aes(x = tmean_5_yr_ave, y = duration_log, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # mostly longer in warm area
  
  ggplot(d_dom, aes(x = precip_5_yr_ave, y = duration_log, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # mostly longer in wetter area
  
  ggplot(d_dom, aes(x = prop_extreme_warm_days_total_logit, y = duration_log, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # longer
  
  ggplot(d_dom, aes(x = prop_extreme_cold_days_total_logit, y = duration_log, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # longer????
  
  ggplot(d_dom, aes(x = prop_extreme_wet_days_total_logit, y = duration_log, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # longer??
  
  ggplot(d_dom, aes(x = prop_extreme_dry_days_total_logit, y = duration_log, color = sp)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~taxon) +
    theme(legend.position = "none") # longer??
  
} else {
  d2 = readRDS("data_output/d_for_stat.rds")
}



d2 = readRDS("data_output/d_for_stat.rds")
group_by(d2, taxon) |> summarise(nsp = n_distinct(sp), ncell = n_distinct(id_cells), n = n())
