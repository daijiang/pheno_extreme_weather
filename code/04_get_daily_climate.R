source("code/00_pkg_functions.R")

## get daily climate data ----
getwd()
x = terra::rast("../../common_data/climate/Daymet_V4_daily/daymet_v4_daily_na_prcp_1980.nc")

y = readRDS("data/grids_usa_used.rds")
# change crs to match the climate data
y2 = st_transform(y, crs(x))


# extract daily data from 1980-2022 for each grid cell ----
for(i in list.files("../../common_data/climate/Daymet_V4_daily")){
  cat(i, "\n")
  f_save = paste0("data/daymet/", i, ".rds")
  if(file.exists(f_save)) next()
  
  x = terra::rast(paste0("../../common_data/climate/Daymet_V4_daily/", i))
  
  y2_climate <- pbmclapply(1:nlyr(x), function(j){
    terra::extract(x[[j]], y2, fun = mean, na.rm = TRUE)
  }, mc.cores = 50)
  
  y2_climate2 = purrr::reduce(y2_climate, left_join, by = "ID")
  colnames(y2_climate2) = c("ID", as.character(time(x)))
  
  y2_climate3 = bind_cols(st_drop_geometry(y), y2_climate2) |> as_tibble()
  
  write_rds(y2_climate3, f_save)
}



# daymet daily climate data ----
if(file.exists("data/daymet_daily_date_summary.rds")){
  daymet_daily_date = read_rds("data/daymet_daily_date.rds")
  daymet_daily_date_summary = read_rds("data/daymet_daily_date_summary.rds")
} else {
  daymet_precip_files = list.files("data/daymet", pattern = "prcp", full.names = T)
  daymet_tmax_files = list.files("data/daymet", pattern = "tmax", full.names = T)
  daymet_tmin_files = list.files("data/daymet", pattern = "tmin", full.names = T)
  # read_rds(daymet_precip_files[3])
  
  daymet_daily_precip = map(daymet_precip_files, read_rds) |> 
    reduce(left_join)
  daymet_daily_precip_long = pivot_longer(daymet_daily_precip, cols = -c(1:2), 
                                          names_to = "dates", values_to = "precip")
  
  daymet_daily_tmax = map(daymet_tmax_files, read_rds) |> 
    reduce(left_join)
  daymet_daily_tmax_long = pivot_longer(daymet_daily_tmax, cols = -c(1:2), 
                                        names_to = "dates", values_to = "tmax")
  
  daymet_daily_tmin = map(daymet_tmin_files, read_rds) |> 
    reduce(left_join)
  daymet_daily_tmin_long = pivot_longer(daymet_daily_tmin, cols = -c(1:2), 
                                        names_to = "dates", values_to = "tmin")
  
  
  daymet_daily_date = left_join(daymet_daily_precip_long, daymet_daily_tmax_long) |> 
    left_join(daymet_daily_tmin_long)
  daymet_daily_date = mutate(daymet_daily_date, dates = ymd(dates))
  daymet_daily_date$ID = NULL
  daymet_daily_date = mutate(daymet_daily_date, yr = year(dates), doy = yday(dates))
  daymet_daily_date = mutate(daymet_daily_date, tmean = (tmax + tmin) / 2)
  
  write_rds(daymet_daily_date, "data/daymet_daily_date.rds")
  
  # get summary data
  daymet_daily_date_summary = daymet_daily_date |> 
    group_by(id_cells, doy) |> 
    summarise(mean_daily_precip = mean(precip, na.rm = T),
              sd_daily_precip = sd(precip, na.rm = T),
              mean_daily_tmean = mean(tmean, na.rm = T),
              sd_daily_tmean = sd(tmean, na.rm = T),
              mean_daily_tmax = mean(tmax, na.rm = T),
              sd_daily_tmax = sd(tmax, na.rm = T),
              mean_daily_tmin = mean(tmin, na.rm = T),
              sd_daily_tmin = sd(tmin, na.rm = T), .groups = "drop")
  write_rds(daymet_daily_date_summary, "data/daymet_daily_date_summary.rds")
}

