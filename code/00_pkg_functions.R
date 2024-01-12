# a modified version of ropensci/prism to download all variables at once
if(!require("xfun")) install.packages(xfun)
# if(!require("prism")) remotes::install_github("daijiang/prism")
# if(!require(phenesse)) remotes::install_github("mbelitz/phenesse")
xfun::pkg_attach2(c("tidyverse", "raster", "sf", "terra", "phenesse", 
                    # "prism", 
                    "lubridate", "parallel", "corrplot", "lmerTest", "sjPlot",
                    "broom.mixed", "pbmcapply", "phyr"))

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


#' Plot observation data and group by grid cells
#' 
#' @param us_map US map.
#' @param cell_size Size (meters) of the square grid cells.
#' @param dat Data frame of observed information. The first column must be longitude;
#' the second column must be latitude.
#' @param n_per_cell Minimum records per cell to be used.
#' @param days_per_cell Minimum days of data per cell to be used.
#' @param sd_cutoff The cutoff of standard deviation of the number of records of 
#' all days for each year and cell combination; if higher than this cutoff, it
#' suggests that some days have way more records than the other days; we will thin
#' records for these days.
#' @param show_fig Plot the figure at the same time?
#' @param add_lakes_map Plot lakes?
#' @return A list of maps, a data frame to summarise number of records per cell,
#' and a data frame of the records that fall within cells with enough records.
#' 
plt_summary = function(
    dat = tibble(long = runif(300, -110, -85),
                 lat = runif(300, 26, 45), z = 1),
    n_per_cell = 5, days_per_cell = 3, sd_cutoff = 3.5,
    show_fig = FALSE, plt_base, grids_usa, cell_size = 25000){
  # is there any cell with most obs from one day of a year? likely because of iNat city challenge
  # 2019: April 26 - 29
  # 2018: April 27 - 30
  # 2017: April 14 - 18
  # 2016: April 14 - 21
  city_challenge_days = as.Date(c(
    "2022-04-29", "2022-04-30", "2022-05-01", "2022-05-02", 
    "2021-04-30", "2021-05-01", "2021-05-02", "2021-05-03", 
    "2020-04-24", "2020-04-25", "2020-04-26", "2020-04-27", 
    "2019-04-26", "2019-04-27", "2019-04-28", "2019-04-29",
    "2018-04-27", "2018-04-28", "2018-04-29", "2018-04-30",
    "2017-04-14", "2017-04-15", "2017-04-16", "2017-04-17", "2017-04-18",
    "2016-04-14", "2016-04-15", "2016-04-16", "2016-04-17", 
    "2016-04-18", "2016-04-19", "2016-04-20", "2016-04-21"))
  
  # identify cells/year with enough records but may be peaked at specific days (based on SD)
  cell_yr_peaks = group_by(st_drop_geometry(dat), id_cells, yr, observed_on) %>% 
    tally() %>% # count per day
    group_by(id_cells, yr) %>% 
    summarise(n_days = n(), 
              n_records = sum(n, na.rm = T),
              ave_records_per_day = mean(n, na.rm = T), 
              sd_record_days = sd(n, na.rm = T)) %>% 
    filter(sd_record_days > sd_cutoff, n_records >= n_per_cell, n_days >= 5) %>% 
    ungroup()
  
  if(nrow(cell_yr_peaks)){
    # data that need to thin
    dat_cells2 = left_join(dplyr::select(cell_yr_peaks, id_cells, yr), 
                           dat, by = c("id_cells", "yr")) 
    # get n record per day and check whether they are from city challenge dates
    cell_yr_peak_days = group_by(dat_cells2, id_cells, yr, observed_on) %>% 
      tally() %>% 
      mutate(in_city_challenge = observed_on %in% city_challenge_days) %>% 
      arrange(id_cells, yr, desc(n)) %>% ungroup()
    # get the max number of records that are not from city challenge
    cell_yr_peak_no_city_challenge = cell_yr_peak_days %>% 
      filter(!in_city_challenge) %>% 
      group_by(id_cells, yr) %>% 
      summarise(max_n_per_day_not_city_challenge = max(n)) %>% 
      ungroup()
    
    cell_yr_peaks = left_join(cell_yr_peaks, cell_yr_peak_no_city_challenge, 
                              by = c("id_cells", "yr"))
    # decide how many records to keep for each day
    n_to_keep = bind_rows(
      # the top 1, no matter it is from city challenge or not
      group_by(cell_yr_peak_days, id_cells, yr) %>% slice(1L),
      # the first top 5 that are from city challenge
      group_by(cell_yr_peak_days, id_cells, yr) %>% 
        slice(1L:5L) %>% filter(in_city_challenge) 
    ) %>% unique() %>% 
      ungroup() %>% 
      left_join(cell_yr_peaks, by = c("id_cells", "yr")) %>% 
      filter(n >= max_n_per_day_not_city_challenge) %>% 
      mutate(ave_records_per_day = round(ave_records_per_day),
             # if the day with max record is the dates of city challgend
             # keep the number of records that is equal to the max number from non-city-challenge days
             # otherwise, use the average number of record per day
             n_to_keep = ifelse(in_city_challenge, 
                                max_n_per_day_not_city_challenge,
                                ave_records_per_day)) %>% 
      dplyr::select(id_cells, yr, observed_on, n_to_keep)
    
    # data that are good to go
    dat_cells_asis = anti_join(dat, 
                               dplyr::select(n_to_keep, id_cells, yr, observed_on), 
                               by = c("id_cells", "yr", "observed_on"))
    # random thin for these days
    dat_cells_thinned = left_join(n_to_keep, dat, 
                                  by = c("id_cells", "yr", "observed_on")) %>% 
      group_by(id_cells, yr, observed_on) %>% 
      sample_n(unique(n_to_keep)) %>% ungroup() %>% 
      dplyr::select(-n_to_keep) %>% st_sf()
    
    dat = rbind(dat_cells_asis, dat_cells_thinned) %>% unique()
  }
  
  # count number of records per cell and year combination
  dat_cells_count = group_by(st_drop_geometry(dat), id_cells, yr) %>% 
    summarise(n_records = n(), n_days = n_distinct(observed_on)) %>% 
    mutate(enough_data = n_records >= n_per_cell & n_days >= days_per_cell) %>% 
    ungroup()
  
  if(sum(dat_cells_count$enough_data) < 3){
    message("<3 cells with enough data")
    return(NULL)
  }
  
  # cells with data
  cells_with_data = dplyr::filter(grids_usa, id_cells %in% dat_cells_count$id_cells) %>% 
    left_join(dat_cells_count, by = "id_cells")
  # # add centroid coords
  # cells_with_data = bind_cols(cells_with_data, 
  #                             suppressWarnings(st_centroid(cells_with_data) %>% 
  #                                                st_transform(4326) %>% 
  #                                                st_coordinates() %>% 
  #                                                as.data.frame() %>% 
  #                                                rename(long_cell = X, lat_cell = Y)))
  
  # records fall within cells with >= n_per_cell records
  
  dat_to_use = right_join(dat, 
                          dplyr::select(st_drop_geometry(filter(cells_with_data, enough_data)), 
                                        id_cells, yr),
                          by = c("id_cells", "yr")
  )
  
  
  plt = plt_base +
    geom_sf(data = dat, size = 0.5, alpha = 0.6) + 
    geom_sf(data = filter(cells_with_data, enough_data), alpha = 0, 
            size = 0.15, color = "red") +
    labs(title = paste(dat$scientific_name[1],
                       nrow(filter(cells_with_data, enough_data)),
                       "highlighted cells with records more than",
                       n_per_cell, 
                       "(Cell resolution:", cell_size/1000, "km by",
                       cell_size/1000, "km)",
                       collapse = " ")) 
  
  if(show_fig){
    print(plt)
  }
  
  cat(nrow(filter(cells_with_data, enough_data)), 
      "cells with records more than", n_per_cell, "for",
      dat$scientific_name[1], "\n")
  
  
  list(cells_with_data = cells_with_data,
       dat_to_use = dat_to_use, fig = plt)
}

# remove potential outliers
outliers_det = function(s){ # s is a vector
  s1 = unique(s)
  so = boxplot.stats(s1, coef = 2)$out
  if(length(so)) {
    sw = which(!s %in% so)
  } else {
    sw = seq_along(s)
  }
  sw # these are the index to keep
}


run_phenesse2 <- function(df, minimum_obs = 10, minimum_days = 3,
                          earliest_year = 2017, last_year = 2019, 
                          flowering_cutoff = "01-01", n_item = 500,
                          onset_perct = 0.05, offset_perct = 0.95, num_cores = 6,
                          save_rds = FALSE,
                          rds_folder_path = "data_output/phenesse_outputs"){
  if(nrow(df) == 0) return(tibble())
  if(file.exists(paste0(rds_folder_path, "/phenesse_", gsub(" ", "_", unique(df$sp)), ".rds"))){
    return(NULL)
  }
  
  
  df = df |> 
    group_by(yr, id_cells) %>%
    do(.[outliers_det(.$doy),]) |> 
    ungroup()
  
  # recheck whether we still have enough data?
  df = df |> group_by(yr, id_cells) |> 
    summarise(n_ob = n(), n_day = n_distinct(doy), .groups = "drop") |> 
    filter(n_ob >= 5, n_day >= 3) |> 
    dplyr::select(yr, id_cells) |> 
    left_join(df)
  
  df_summary = df |> 
    group_by(yr, id_cells) |> 
    summarise(n_obs = n(), n_days = n_distinct(doy), .groups = "drop")
  
  df2 = df %>% 
    group_by(yr, id_cells, doy) %>% 
    summarise(n_obs = n(), .groups = "drop")
  
  # make list with all doy values in it for each cell x year combination
  species_cell_year <- split(df2, f = list(df2$yr, df2$id_cells), drop = TRUE)
  
  # lapply functions
  if(onset_perct == 0 | offset_perct == 1) {
    setestimator <- function(x, niter = n_item, perct = 0){
      tibble(est = weib_percentile(observations = rep(x$doy, x$n_obs), 
                                   iterations = niter, percentile = perct))
    } 
  } else {
    setestimator <- function(x, niter = n_item, perct = 0){
      tibble(est = quantile_ci(observations = rep(x$doy, x$n_obs), 
                               bootstraps = niter, percentile = perct))
    }
  }
  
  # Estimate onseet and offset
  if(num_cores > 1){
    onset <- pbmclapply(species_cell_year, setestimator, niter = n_item,
                        perct = onset_perct, mc.cores = num_cores)
    offset <- pbmclapply(species_cell_year, setestimator, niter = n_item,
                         perct = offset_perct, mc.cores = num_cores)
  } else{
    onset <- plyr::llply(species_cell_year, setestimator, niter = n_item, perct = onset_perct, .progress = "text")
    offset <- plyr::llply(species_cell_year, setestimator, niter = n_item, perct = offset_perct, .progress = "text")
  }
  
  # remove potential try-errors
  try_error_onset = map(onset, class) %>% map_lgl(~ "try-error" %in% .x)
  if(any(try_error_onset)) warning("onset estimations have try-error")
  onset = onset[!try_error_onset]
  
  try_error_offset = map(offset, class) %>% map_lgl(~ "try-error" %in% .x)
  if(any(try_error_offset)) warning("offset estimations have try-error")
  offset = offset[!try_error_offset]
  
  # split outputs back to df
  if(onset_perct == 0 | offset_perct == 1) {
    onset_df = map_df(onset, ~.x, .id = "yr_cell") %>% 
      separate("yr_cell", into = c("yr2", "id_cells"), sep = "[.]") %>% 
      mutate(id_cells = as.numeric(id_cells),
             yr2 = as.numeric(yr2)) %>% 
      rename(onset = est)
    
    offset_df = map_df(offset, ~.x, .id = "yr_cell") %>% 
      separate("yr_cell", into = c("yr2", "id_cells"), sep = "[.]") %>% 
      mutate(id_cells = as.numeric(id_cells),
             yr2 = as.numeric(yr2)) %>% 
      rename(offset = est)
  } else {
    onset_df = map_df(onset, ~.x$est,  .id = "yr_cell") %>% 
      separate("yr_cell", into = c("yr", "id_cells"), sep = "[.]") %>% 
      mutate(id_cells = as.character(id_cells),
             yr = as.numeric(yr)) %>% 
      rename(onset = estimate, on_low_ci = low_ci, on_high_ci = high_ci)
    offset_df = map_df(offset, ~.x$est, .id = "yr_cell") %>% 
      separate("yr_cell", into = c("yr", "id_cells"), sep = "[.]") %>% 
      mutate(id_cells = as.character(id_cells),
             yr = as.numeric(yr)) %>% 
      rename(offset = estimate, off_low_ci = low_ci, off_high_ci = high_ci)
  }
  
  # join estimates with original sf dataframe based on cell_ids and year
  cell_duration <- left_join(onset_df, offset_df, 
                             by = c("yr", "id_cells")) %>% 
    mutate(duration = offset - onset,
           sp = unique(df$sp))
  
  cell_duration = left_join(df_summary, cell_duration, by = c("yr", "id_cells"))
  
  if(save_rds) saveRDS(cell_duration, file = paste0(rds_folder_path, "/phenesse_", 
                                                    gsub(" ", "_", unique(df$sp)), ".rds"))
  
  return(cell_duration)
}

logit_trans = function(x){
  stopifnot(all(x >= 0) & all(x <= 1))
  x = ifelse(x == 0, x + 0.01, x)
  x = ifelse(x == 1, x - 0.01, x)
  log(x / (1 - x))
}
