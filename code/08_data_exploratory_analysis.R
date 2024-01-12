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
