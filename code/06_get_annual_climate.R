source("code/00_pkg_functions.R")

if(!file.exists("data_output/t_clim_annual.rds")){
  grid_usa_used = read_rds("data/grids_usa_used.rds")
  
  yr = (2016 - 4): 2022
  
  fis = c(
    paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_prcp_annttl_na_", yr, ".nc"),
    paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_tmax_annavg_na_", yr, ".nc"),
    paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_tmin_annavg_na_", yr, ".nc")
  )
  
  tst = terra::rast(fis[1])
  
  grid_usa_used = st_transform(grid_usa_used, crs(tst))
  
  
  tst1 = terra::extract(tst, grid_usa_used, fun = mean, method = "bilinear", na.rm = T) |> as_tibble()
  
  t_clim_all = vector("list", length = length(yr))
  for(i in seq_along(yr)){
    cat(yr, "\n")
    prcp = paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_prcp_annttl_na_", yr[i], ".nc")
    tmax = paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_tmax_annavg_na_", yr[i], ".nc")
    tmin = paste0("../../common_data/climate/Daymet_V4_annual/daymet_v4_tmin_annavg_na_", yr[i], ".nc")
    tmean = (terra::rast(tmax) + terra::rast(tmin))/2
    prcp = terra::rast(prcp)
    t_prcp = terra::extract(prcp, grid_usa_used, fun = mean, method = "bilinear", na.rm = T) |> as_tibble()
    t_tmean = terra::extract(tmean, grid_usa_used, fun = mean, method = "bilinear", na.rm = T) |> as_tibble()
    t_clim_all[[i]] = left_join(t_prcp, t_tmean) |> 
      mutate(yr = i, id_cells = grid_usa_used$id_cells) |> 
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
  
  write_rds(t_clim_annual, "data_output/t_clim_annual.rds")
  
} else {
  t_clim_annual = readRDS("data_output/t_clim_annual.rds")
}

