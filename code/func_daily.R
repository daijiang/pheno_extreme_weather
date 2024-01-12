# this file is for 05_get_annomaly_days.R specifically
# because it used `job::jobs()` and I don't want to copy all data into different jobs
library(tidyverse)

Rcpp::sourceCpp("code/sample_entropy.cpp")

# for each day of a year, find out is there any need for this date's climate data
# if so, read raster and extract for lat/long locations.
get_date = function(dy = "2020-01-27", ndays_ahead = 120){
  # print(dy)
  dy = ymd(dy)
  if(is.null(ndays_ahead)){
    start_day = ymd(paste0(year(dy) - 1, "1001"))
  } else {
    start_day = dy + lubridate::ddays(-ndays_ahead)
  }
  tibble(days_needed = seq(start_day, dy, by = "days"))
}



#' @param df Data frame with two columns: days_needed, id_cells
detect_unsual_days = function(df, unusual_days = TRUE){
  
  # Modified Rainfall Anomaly Index (mRAI)?
  df2 = dplyr::select(df, days_needed) |> mutate(yi = 0)
  miny = year(min(df2$days_needed))
  maxy = year(max(df2$days_needed))
  
  # work before
  yr = miny 
  yi = 0
  while (yr > 1980) {
    yi = yi - 1
    df2 = bind_rows(df2, 
                    tibble(days_needed = df$days_needed + years(yi),
                           yi = yi)
    )
    yr = yr - 1
  }
  # work after
  yr = maxy 
  yi = 0
  while (yr < 2022) {
    yi = yi + 1
    df2 = bind_rows(df2, 
                    tibble(days_needed = df$days_needed + years(yi),
                           yi = yi)
    )
    yr = yr + 1
  }
  
  df2 = filter(df2, days_needed > as.Date("1979-12-31"), days_needed < as.Date("2023-01-01"))
  
  df2$id_cells = df$id_cells[1]
  
  df2 = left_join(df2, daymet_daily_date, by = c("id_cells", "days_needed" = "dates"))
  df2 = drop_na(df2)
  df2_precip = group_by(df2, yi) |> summarise(total_precip = sum(precip, na.rm = T))
  df2_precip_focus = filter(df2_precip, yi == 0)$total_precip
  df2_precip_others = df2_precip$total_precip
  df2_precip_others_q = quantile(df2_precip_others, probs = c(0.1, 0.5, 0.9))
  e_dry = mean(df2_precip_others[df2_precip_others <= df2_precip_others_q[1]])
  e_wet = mean(df2_precip_others[df2_precip_others >= df2_precip_others_q[3]])
  m = unname(df2_precip_others_q[2])
  
  # mRAI
  scaling_factor = 1.7
  if(df2_precip_focus <= m){ # less than the median
    scaling_factor = -1.7
    mrai = scaling_factor * (df2_precip_focus - m) / (e_dry - m)
  } else { # more than median
    scaling_factor = 1.7
    mrai = scaling_factor * (df2_precip_focus - m) / (e_wet - m)
  }
  
  if(unusual_days){
    days_needed = mutate(df, doy = yday(days_needed)) |> 
      rename(dates = days_needed)
    # for each day, extract its daily precip, tmax, and tmin, and compare with average
    # to determine whether it is unusual or not
    days_needed_climate = left_join(days_needed, daymet_daily_date, 
                                    by = c("doy", "id_cells", "dates"))
    # get average daily date between 1980-2022
    days_needed_climate = left_join(days_needed_climate, daymet_daily_date_summary, 
                                    by = c("doy", "id_cells"))
    # warm? cold? dry? wet?
    days_needed_climate = mutate(days_needed_climate,
                                 warm = tmean > mean_daily_tmean + 2 * sd_daily_tmean,
                                 cold = tmean < mean_daily_tmean - 2 * sd_daily_tmean,
                                 dry = precip < mean_daily_precip - 2 * sd_daily_precip,
                                 wet = precip > mean_daily_precip + 2 * sd_daily_precip
    )
    ts_warm = as.integer(days_needed_climate$warm)
    ts_cold = as.integer(days_needed_climate$cold)
    ts_wet = as.integer(days_needed_climate$wet)
    
    mo = tibble(n_unusual_warm_days = sum(days_needed_climate$warm, na.rm = T),
                n_unusual_cold_days = sum(days_needed_climate$cold, na.rm = T),
                n_unusual_wet_days = sum(days_needed_climate$wet, na.rm = T),
                n_unusual_dry_days = sum(days_needed_climate$dry, na.rm = T),
                tmean = mean(days_needed_climate$tmean, na.rm = TRUE),
                precip = sum(days_needed_climate$precip, na.rm = TRUE),
                sample_entropy_warm = SampleEntropy(ts_warm, m = 2L, r = 0.2, sd = sd(ts_warm), N = as.integer(length(ts_warm))),
                sample_entropy_cold = SampleEntropy(ts_cold, m = 2L, r = 0.2, sd = sd(ts_cold), N = as.integer(length(ts_cold))),
                sample_entropy_wet = SampleEntropy(ts_wet, m = 2L, r = 0.2, sd = sd(ts_wet), N = as.integer(length(ts_wet))),
                mRAI = mrai)
  } else {
    mo = tibble(mRAI = mrai)
  }
  
  mo
}

#' @param df Data frame with two columns: days_needed, id_cells
detect_unsual_days_window = function(df, days_buff_at_one_side = 3){
  days_10days_window = vector("list", length = nrow(df))
  for(i in 1:nrow(df)){
    # the 3 days before and after each day
    days_buff = c(df$days_needed[i] - c(1:days_buff_at_one_side), 
                  df$days_needed[i], 
                  df$days_needed[i] + c(1:days_buff_at_one_side))
    df1 = tibble(days_needed = days_buff, yi = 0)
    
    # get the same days for each year within 1980 - 2015 as the baselines
    yr = year(min(df1$days_needed)) 
    yi = 0
    while (yr > 1980) {
      yi = yi - 1
      df1 = bind_rows(df1, 
                      tibble(days_needed = days_buff + years(yi), yi = yi)
      )
      yr = yr - 1
    }
    df1 = filter(df1, days_needed > as.Date("1979-12-31"), days_needed < as.Date("2016-01-01"))
    df1$target_date = df$days_needed[i]
    days_10days_window[[i]] = df1
  }
  days_10days_window = bind_rows(days_10days_window)
  
  # merge with daily weather data
  df2 = distinct(dplyr::select(days_10days_window, days_needed)) |> 
    mutate(id_cells = df$id_cells[1]) |> 
    # add current year's data
    bind_rows(tibble(days_needed = c(min(df$days_needed) - c(1:days_buff_at_one_side),
                                     df$days_needed,
                                     max(df$days_needed) + c(1:days_buff_at_one_side)), 
                     id_cells = df$id_cells[1])) |> 
    distinct() |> 
    left_join(daymet_daily_date, by = c("id_cells", "days_needed" = "dates"))
  
  # d = filter(days_10days_window, target_date == "2019-01-17")
  calc_mRAI = function(d){
    # get the observed value
    df2_target = filter(df2, days_needed %in% c(d$target_date[1] - c(1:days_buff_at_one_side), 
                                                d$target_date[1], 
                                                d$target_date[1] + c(1:days_buff_at_one_side)))
    precip_target = sum(df2_target$precip, na.rm = T)
    tmean_target = mean(df2_target$tmean, na.rm = T)
    tmax_target = mean(df2_target$tmax, na.rm = T)
    tmin_target = mean(df2_target$tmin, na.rm = T)
    
    # get the same value for historical dates
    get_total_precip = function(x){
      tibble(total_precip = sum(filter(df2, days_needed %in% x$days_needed)$precip, na.rm = T),
             ave_tmean = mean(filter(df2, days_needed %in% x$days_needed)$tmean, na.rm = T),
             ave_tmax = mean(filter(df2, days_needed %in% x$days_needed)$tmax, na.rm = T),
             ave_tmin = mean(filter(df2, days_needed %in% x$days_needed)$tmin, na.rm = T)
      )
    }
    
    clim_base = d |> 
      group_by(yi) |> 
      do(get_total_precip(.)) |> ungroup()
    
    # mRAI
    precip_base = clim_base$total_precip
    precip_base_q = quantile(precip_base, probs = c(0.1, 0.5, 0.9))
    e_dry = mean(precip_base[precip_base <= precip_base_q[1]])
    e_wet = mean(precip_base[precip_base >= precip_base_q[3]])
    m = unname(precip_base_q[2])
    # if the e_dry is equal m, then the denominator will be 0, mRAI will be NaN
    if(abs(e_dry - m) < 0.001 | abs(e_wet - m) < 0.001) m = mean(precip_base, na.rm = T)
    
    if(precip_target <= m){ # less than the median
      scaling_factor = -2
      mrai = scaling_factor * (precip_target - m) / (e_dry - m)
    } else { # more than median
      scaling_factor = 2
      mrai = scaling_factor * (precip_target - m) / (e_wet - m)
    } 
    
    tibble(mRAI = mrai,
           extreme_warm = tmax_target >= mean(clim_base$ave_tmax, na.rm = T) + 2 * sd(clim_base$ave_tmax, na.rm = T),
           extreme_cold = tmin_target <= mean(clim_base$ave_tmin, na.rm = T) - 2 * sd(clim_base$ave_tmin, na.rm = T))
  }
  
  mrai_values = days_10days_window |> 
    group_by(target_date) |> 
    do(calc_mRAI(.)) |> ungroup()
  
  mrai_values = mutate(mrai_values, very_dry = mRAI <= -1.5, extreme_dry = mRAI <= -2,
                       very_wet = mRAI >= 1.5, extreme_wet = mRAI >= 2) |> 
    mutate(extreme_temp = ifelse(extreme_warm & extreme_cold, 2, ifelse(extreme_warm, 1, ifelse(extreme_cold, -1, 0))),
           extreme_precip = ifelse(extreme_wet, 1, ifelse(extreme_dry, -1, 0)))
  
  ts_warm = as.integer(mrai_values$extreme_warm)
  ts_cold = as.integer(mrai_values$extreme_cold)
  ts_wet = as.integer(mrai_values$extreme_wet)
  ts_dry = as.integer(mrai_values$extreme_dry)
  ts_wet_0 = as.integer(mrai_values$very_wet)
  ts_dry_0 = as.integer(mrai_values$very_dry)
  ts_temp = as.integer(mrai_values$extreme_temp)
  ts_precip = as.integer(mrai_values$extreme_precip)
  
  mo = tibble(n_extreme_warm_days = sum(ts_warm, na.rm = T),
              n_extreme_cold_days = sum(ts_cold, na.rm = T),
              n_extreme_wet_days = sum(ts_wet, na.rm = T),
              n_extreme_dry_days = sum(ts_dry, na.rm = T),
              n_very_wet_days = sum(ts_wet_0, na.rm = T),
              n_very_dry_days = sum(ts_dry_0, na.rm = T),
              sample_entropy_extreme_warm = SampleEntropy(ts_warm, m = 2L, r = 0.2, sd = sd(ts_warm), N = as.integer(length(ts_warm))),
              sample_entropy_extreme_cold = SampleEntropy(ts_cold, m = 2L, r = 0.2, sd = sd(ts_cold), N = as.integer(length(ts_cold))),
              sample_entropy_extreme_wet = SampleEntropy(ts_wet, m = 2L, r = 0.2, sd = sd(ts_wet), N = as.integer(length(ts_wet))),
              sample_entropy_extreme_dry = SampleEntropy(ts_dry, m = 2L, r = 0.2, sd = sd(ts_dry), N = as.integer(length(ts_dry))),
              sample_entropy_very_wet = SampleEntropy(ts_wet_0, m = 2L, r = 0.2, sd = sd(ts_wet_0), N = as.integer(length(ts_wet_0))),
              sample_entropy_very_dry = SampleEntropy(ts_dry_0, m = 2L, r = 0.2, sd = sd(ts_dry_0), N = as.integer(length(ts_dry_0))),
              sample_entropy_temp = SampleEntropy(ts_temp, m = 2L, r = 0.2, sd = sd(ts_temp), N = as.integer(length(ts_temp))),
              sample_entropy_precip = SampleEntropy(ts_precip, m = 2L, r = 0.2, sd = sd(ts_precip), N = as.integer(length(ts_precip)))
  )
  
  list(days_determine = mrai_values, days_summary = mo)
}

# to work with both before onset, and between onset and offset
detect_unsual_days2 = function(i, n_day_ahead = 60){
  # before onset
  df1 = get_date(df_25k_pheno$onset_date[i], ndays_ahead = n_day_ahead) |> 
    mutate(id_cells = df_25k_pheno$id_cells[i])
  anom1 = detect_unsual_days(df1)
  names(anom1) = paste0(names(anom1), "_before_onset")
  
  # within duration
  df2 = tibble(days_needed = seq(df_25k_pheno$onset_date[i], df_25k_pheno$offset_date[i], by = "day")[-1],
               id_cells = df_25k_pheno$id_cells[i])
  # onset date already inclueded in the above section
  anom2 = detect_unsual_days(df2)
  names(anom2) = paste0(names(anom2), "_within_duration")
  
  df3 = bind_rows(df1, df2) |> unique()
  anom3 = detect_unsual_days(df3)
  names(anom3) = paste0(names(anom3), "_total")
  
  bind_cols(anom1, anom2, anom3) |> 
    mutate(id_cells = df_25k_pheno$id_cells[i],
           yr = df_25k_pheno$yr[i],
           sp = df_25k_pheno$sp[i]
    )
}

# 7 days window instead of just 1 day
detect_unsual_days2_window = function(i, n_day_ahead = 60){
  # before onset
  df1 = get_date(df_25k_pheno$onset_date[i], ndays_ahead = n_day_ahead) |> 
    mutate(id_cells = df_25k_pheno$id_cells[i])
  anom1 = detect_unsual_days_window(df = df1)
  names(anom1$days_summary) = paste0(names(anom1$days_summary), "_before_onset")
  
  # within duration
  df2 = tibble(days_needed = seq(df_25k_pheno$onset_date[i], df_25k_pheno$offset_date[i], by = "day")[-1],
               id_cells = df_25k_pheno$id_cells[i])
  # onset date already inclueded in the above section
  anom2 = detect_unsual_days_window(df2)
  names(anom2$days_summary) = paste0(names(anom2$days_summary), "_within_duration")
  
  mrai_values = bind_rows(
    anom1$days_determine,
    anom2$days_determine
  ) |> unique()
  
  ts_warm = as.integer(mrai_values$extreme_warm)
  ts_cold = as.integer(mrai_values$extreme_cold)
  ts_wet = as.integer(mrai_values$extreme_wet)
  ts_dry = as.integer(mrai_values$extreme_dry)
  ts_wet_0 = as.integer(mrai_values$very_wet)
  ts_dry_0 = as.integer(mrai_values$very_dry)
  ts_temp = as.integer(mrai_values$extreme_temp)
  ts_precip = as.integer(mrai_values$extreme_precip)
  
  anom3 = tibble(n_extreme_warm_days = sum(ts_warm, na.rm = T),
                 n_extreme_cold_days = sum(ts_cold, na.rm = T),
                 n_extreme_wet_days = sum(ts_wet, na.rm = T),
                 n_extreme_dry_days = sum(ts_dry, na.rm = T),
                 n_very_wet_days = sum(ts_wet_0, na.rm = T),
                 n_very_dry_days = sum(ts_dry_0, na.rm = T),
                 sample_entropy_extreme_warm = SampleEntropy(ts_warm, m = 2L, r = 0.2, sd = sd(ts_warm), N = as.integer(length(ts_warm))),
                 sample_entropy_extreme_cold = SampleEntropy(ts_cold, m = 2L, r = 0.2, sd = sd(ts_cold), N = as.integer(length(ts_cold))),
                 sample_entropy_extreme_wet = SampleEntropy(ts_wet, m = 2L, r = 0.2, sd = sd(ts_wet), N = as.integer(length(ts_wet))),
                 sample_entropy_extreme_dry = SampleEntropy(ts_dry, m = 2L, r = 0.2, sd = sd(ts_dry), N = as.integer(length(ts_dry))),
                 sample_entropy_very_wet = SampleEntropy(ts_wet_0, m = 2L, r = 0.2, sd = sd(ts_wet_0), N = as.integer(length(ts_wet_0))),
                 sample_entropy_very_dry = SampleEntropy(ts_dry_0, m = 2L, r = 0.2, sd = sd(ts_dry_0), N = as.integer(length(ts_dry_0))),
                 sample_entropy_temp = SampleEntropy(ts_temp, m = 2L, r = 0.2, sd = sd(ts_temp), N = as.integer(length(ts_temp))),
                 sample_entropy_precip = SampleEntropy(ts_precip, m = 2L, r = 0.2, sd = sd(ts_precip), N = as.integer(length(ts_precip)))
  )
  
  
  names(anom3) = paste0(names(anom3), "_total")
  
  bind_cols(anom1$days_summary, anom2$days_summary, anom3) |> 
    mutate(id_cells = df_25k_pheno$id_cells[i],
           yr = df_25k_pheno$yr[i],
           sp = df_25k_pheno$sp[i]
    )
}



df_25k_pheno = bind_rows(
  mutate(readRDS("data_output/all_25km_pheno2.rds"), taxon = "plant"),
  mutate(readRDS("data_output/all_25km_pheno2_insects.rds"), taxon = "insect")
)

df_25k_pheno = mutate(df_25k_pheno, onset = floor(onset), offset = ceiling(offset), 
                      duration = offset - onset, onset_date = as.Date(onset - 1, origin = paste0(yr, "-01-01")),
                      offset_date = as.Date(offset - 1, origin = paste0(yr, "-01-01")))

daymet_daily_date = read_rds("data/daymet_daily_date.rds")
daymet_daily_date_summary = read_rds("data/daymet_daily_date_summary.rds")



