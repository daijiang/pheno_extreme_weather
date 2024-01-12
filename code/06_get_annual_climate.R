source("code/00_pkg_functions.R")

if(!file.exists("data_output/t_clim_annual.rds")){
  
  grids_usa_used = readRDS("data_output/grids_usa_used.rds")
  
  
  yr = (2016 - 4): 2022
  
  fis = c(
    paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_prcp_annttl_na_", yr, ".nc"),
    paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_tmax_annavg_na_", yr, ".nc"),
    paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_tmin_annavg_na_", yr, ".nc")
  )
  
  tst = terra::rast(fis[1])
  
  grids_usa_used = st_transform(grids_usa_used, crs(tst))
  
  
  # tst1 = terra::extract(tst, grids_usa_used, fun = mean, method = "bilinear", na.rm = T) |> as_tibble()
  
  t_clim_all = vector("list", length = length(yr))
  for(i in seq_along(yr)){
    cat(yr[i], "\n")
    prcp = paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_prcp_annttl_na_", yr[i], ".nc")
    tmax = paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_tmax_annavg_na_", yr[i], ".nc")
    tmin = paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_tmin_annavg_na_", yr[i], ".nc")
    tmean = (terra::rast(tmax) + terra::rast(tmin))/2
    prcp = terra::rast(prcp)
    t_prcp = terra::extract(prcp, grids_usa_used, fun = mean, method = "bilinear", na.rm = T) |> as_tibble()
    t_tmean = terra::extract(tmean, grids_usa_used, fun = mean, method = "bilinear", na.rm = T) |> as_tibble()
    t_clim_all[[i]] = left_join(t_prcp, t_tmean) |> 
      mutate(yr = i, id_cells = grids_usa_used$id_cells) |> 
      dplyr::select(-ID)
  }
  
  t_clim_all = bind_rows(t_clim_all)
  t_clim_all = mutate(t_clim_all, yr = yr + 2011)
  t_clim_all = rename(t_clim_all, tmean = tmax)
  
  t_clim_ave = vector("list", length(2016:2022))
  for (i in 2016:2022) {
    xx = filter(t_clim_all, yr %in% c((i - 4):i))
    xxx = xx |> dplyr::select(-yr) |> 
      group_by(id_cells) |> 
      summarise_all(mean, na.rm = T) |> 
      rename(tmean_5_yr_ave = tmean,
             precip_5_yr_ave = prcp) |> 
      mutate(yr = i)
    t_clim_ave[[i - 2015]] = xxx
  }
  t_clim_ave = bind_rows(t_clim_ave)
  t_clim_annual = filter(t_clim_all, yr %in% 2016:2022) |> 
    rename(tmean_current_yr = tmean, precip_current_yr = prcp) |> 
    left_join(t_clim_ave)
  
  all(grids_usa_used$id_cells %in% t_clim_annual$id_cells)
  
  drop_na(t_clim_annual)
  
  ## seasonality ----
  fis = c(
    paste0("../../common_data/climate/Daymet_V4_Monthly_Climatology/data/daymet_v4_prcp_monttl_na_", yr, ".nc"),
    paste0("../../common_data/climate/Daymet_V4_Monthly_Climatology/data/daymet_v4_tmax_monavg_na_", yr, ".nc"),
    paste0("../../common_data/climate/Daymet_V4_Monthly_Climatology/data/daymet_v4_tmin_monavg_na_", yr, ".nc")
  )
  
  tst = terra::rast(fis[1])
  
  grids_usa_used = st_transform(grids_usa_used, crs(tst))
  
  
  # tst1 = terra::extract(tst, grids_usa_used, fun = mean, method = "bilinear", na.rm = T) |> as_tibble()
  get_cv = function(x) 100 * sd(x, na.rm = T) / mean(x, na.rm = T)
  
  t_clim_season_all = vector("list", length = length(yr))
  for(i in seq_along(yr)[-1]){
    cat(yr[i], "\n")
    prcp = paste0("../../common_data/climate/Daymet_V4_Monthly_Climatology/data/daymet_v4_prcp_monttl_na_", yr[i], ".nc")
    tmax = paste0("../../common_data/climate/Daymet_V4_Monthly_Climatology/data/daymet_v4_tmax_monavg_na_", yr[i], ".nc")
    tmin = paste0("../../common_data/climate/Daymet_V4_Monthly_Climatology/data/daymet_v4_tmin_monavg_na_", yr[i], ".nc")
    tmean = (terra::rast(tmax) + terra::rast(tmin))/2
    prcp = terra::rast(prcp)
    t_prcp = terra::extract(prcp, grids_usa_used, fun = mean, method = "bilinear", na.rm = T) |> 
      as_tibble() |> dplyr::select(-ID)
    
    t_tmean = terra::extract(tmean, grids_usa_used, fun = mean, method = "bilinear", na.rm = T) |> 
      as_tibble() |> dplyr::select(-ID)
    
    t_clim_season_all[[i]] = tibble(yr = i + 2011, 
                                    id_cells = grids_usa_used$id_cells,
                             precip_seasonality = apply(t_prcp, 1, get_cv),
                             temp_seasonality = apply(t_tmean, 1, sd, na.rm = T) * 100) 
  }
  
  saveRDS(t_clim_season_all, "data_output/t_clim_season_all.rds")
  
  t_clim_season_all = bind_rows(t_clim_season_all)
  
  t_clim_season_ave = vector("list", length(2016:2022))
  for (i in 2016:2022) {
    xx = filter(t_clim_season_all, yr %in% c((i - 4):i))
    xxx = xx |> dplyr::select(-yr) |> 
      group_by(id_cells) |> 
      summarise_all(mean, na.rm = T) |> 
      rename(temp_seasonality_5_yr_ave = temp_seasonality,
             precip_seasonality_5_yr_ave = precip_seasonality) |> 
      mutate(yr = i)
    t_clim_season_ave[[i - 2015]] = xxx
  }
  t_clim_season_ave = bind_rows(t_clim_season_ave)
  t_clim_season_annual = filter(t_clim_season_all, yr %in% 2016:2022) |> 
    rename(temp_seasonality_current_yr = temp_seasonality, 
           precip_seasonality_current_yr = precip_seasonality) |> 
    left_join(t_clim_season_ave)
  
  all(grids_usa_used$id_cells %in% t_clim_season_annual$id_cells)
  
  drop_na(t_clim_season_annual)
  
  t_clim_annual = left_join(t_clim_season_annual, t_clim_annual)
  
  write_rds(t_clim_annual, "data_output/t_clim_annual.rds")
  
} else {
  t_clim_annual = readRDS("data_output/t_clim_annual.rds")
}

