source("code/00_pkg_functions.R")

if(!file.exists("data_output/df_25km_pheno_all.rds")){
  grids_usa = read_rds("data_output/grids_usa.rds")
  
  df_25k_pheno = readRDS("data_output/all_25km_pheno2.rds") 
  df_25k_pheno = mutate(df_25k_pheno, onset = floor(onset), offset = ceiling(offset), 
                        duration = offset - onset, onset_date = as.Date(onset - 1, origin = paste0(yr, "-01-01")),
                        offset_date = as.Date(offset - 1, origin = paste0(yr, "-01-01")))
  
  grids_usa_used = filter(grids_usa, id_cells %in% sort(unique(df_25k_pheno$id_cells)))
  # write_rds(grids_usa_used, "data_output/grids_usa_used.rds")
  
  
  # human population density ----
  popu_usa_m5_2010 = terra::rast(x = "/media/dli/Data/common_data/USA_pop_density/USA_HistoricalPopulationDataset/pop_m5_2010/w001001.adf")
  
  pop_25km = terra::extract(popu_usa_m5_2010, 
                            st_transform(grids_usa_used, crs(popu_usa_m5_2010)), 
                            fun = mean, na.rm = TRUE, method = "bilinear")
  pop_25km = as_tibble(pop_25km)
  pop_25km$id_cells = grids_usa_used$id_cells
  
  
  # elevation ----
  elev_usa = terra::rast("/media/dli/Data/common_data/evel_raster_all.tif")
  elev_25km = terra::extract(elev_usa, st_transform(grids_usa_used, crs(elev_usa)),
                             fun = mean, na.rm = TRUE, method = "bilinear")
  pop_25km$elev = elev_25km$evel_raster_all
  
  envi_dat = left_join(grids_usa_used, pop_25km)
  envi_dat = rename(envi_dat, pop_25km = w001001)
  envi_dat = mutate(envi_dat, pop_25km_log10 = log10(pop_25km + 1))
  
  plot(envi_dat["pop_25km_log10"])
  plot(envi_dat["elev"])
  
  
  df_25k_pheno_anom = read_rds("data_output/df_25km_anomaly.rds") 
  
  df_annual_clim = read_rds("data_output/t_clim_annual.rds")
  
  df_25k_pheno_all = dplyr::select(df_25k_pheno, yr, id_cells, sp, everything()) |> 
    left_join(df_25k_pheno_anom) |> 
    left_join(df_annual_clim) |> 
    left_join(st_drop_geometry(envi_dat)) |> 
    dplyr::select(-ID)
  
  write_rds(df_25k_pheno_all, "data_output/df_25km_pheno_all.rds")
} else {
  df_25k_pheno_all = read_rds("data_output/df_25km_pheno_all.rds")
}




