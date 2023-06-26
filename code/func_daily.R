# this file is for 05_get_annomaly_days.R specifically
# because it used `job::jobs()` and I don't want to copy all data into different jobs
library(tidyverse)


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
detect_unsual_days = function(df){
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
  
  tibble(n_unusual_warm_days = sum(days_needed_climate$warm, na.rm = T),
         n_unusual_cold_days = sum(days_needed_climate$cold, na.rm = T),
         n_unusual_wet_days = sum(days_needed_climate$wet, na.rm = T),
         n_unusual_dry_days = sum(days_needed_climate$dry, na.rm = T))
}

# to work with both before onset, and between onset and offset
detect_unsual_days2 = function(i, n_day_ahead = 90){
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
  
  bind_cols(anom1, anom2) |> 
    mutate(id_cells = df_25k_pheno$id_cells[i],
           yr = df_25k_pheno$yr[i],
           sp = df_25k_pheno$sp[i]
    )
}


df_25k_pheno = readRDS("data_output/all_25km_pheno2.rds")

df_25k_pheno = mutate(df_25k_pheno, onset = floor(onset), offset = ceiling(offset), 
                      duration = offset - onset, onset_date = as.Date(onset - 1, origin = paste0(yr, "-01-01")),
                      offset_date = as.Date(offset - 1, origin = paste0(yr, "-01-01")))

daymet_daily_date = read_rds("data/daymet_daily_date.rds")
daymet_daily_date_summary = read_rds("data/daymet_daily_date_summary.rds")



