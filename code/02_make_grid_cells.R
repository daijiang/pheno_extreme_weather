source("code/01_get_data_iNat.R")

# make grid
us_map = readRDS("data/usa_map.rds")

if(!file.exists("data_output/grids_usa.rds")){
  cell_size = 25000
  grids = st_make_grid(us_map, cellsize = c(cell_size, cell_size))
  grids_us = st_intersects(grids, us_map, sparse = F)
  grids2 = grids[grids_us]
  
  plot(us_map[0])
  plot(grids2, add = T)
  # add grid cell id
  grids_usa = mutate(st_sf(geometry = grids2), id_cells = paste0("cell_", 1:n()))
  write_rds(grids_usa, "data_output/grids_usa.rds")
} else {
  grids_usa = readRDS("data_output/grids_usa.rds")
}

plt_base = ggplot() +
  geom_sf(data = us_map) +
  geom_sf(data = readRDS("data/lakes.rds"), color = "gray50", fill = "white") +
  geom_sf(data = grids_usa, alpha = 0, size = 0.1, color = "gray")

# plants ----
if(!file.exists("data_output/all_25km_df_raw.rds")){
  all_dat = readRDS("data_output/d2_plants.rds") |>  
    dplyr::select(longitude = decimalLongitude, latitude = decimalLatitude, 
                  # scientific_name = scientificName,
                  everything()) |>  
    rename(observed_on = eventDate)
  n_distinct(all_dat$sp) # 1867
  table(all_dat$sp)
  sort(unique(all_dat$sp))
  
  # covert the raw data frame to sf
  all_dat_sf = st_transform(st_as_sf(all_dat, coords = c("longitude", "latitude"), 
                                     crs = 4326, remove = FALSE), 
                            crs = st_crs(us_map))
  all_dat_sf_cells = st_join(all_dat_sf, grids_usa) 
  sum(is.na(all_dat_sf_cells$id_cells))
  all_dat_sf_cells = drop_na(all_dat_sf_cells, id_cells)
  st_geometry(all_dat_sf_cells) = all_dat_sf_cells$geometry
  
  # for each species, check which cell-yr has enough data and filter out the others
  splist = sort(unique(all_dat_sf_cells$sp))
  
  all_25km = vector("list", length = length(splist))
  names(all_25km) = splist
  for(i in seq_along(splist)){
    cat("i = ", i, " ", splist[i], "\n")
    x = filter(all_dat_sf_cells, sp == splist[i])
    all_25km[[i]] <- suppressMessages(plt_summary(dat = x, n_per_cell = 5, days_per_cell = 3, show_fig = FALSE, 
                                                  plt_base = plt_base, grids_usa = grids_usa))
  }
  
  map_chr(all_25km, class)
  is.null(all_25km)
  all_25km = all_25km[!map_lgl(all_25km, is.null)]
  all_25km_df = map_dfr(all_25km, "dat_to_use")
  
  # confirm that each cell-yr-sp has >= 5 observations
  all_25km_df |> 
    st_drop_geometry() |> 
    group_by(sp, yr, id_cells) |> 
    tally() |> arrange(n)
  
  # p_map = plt_base +
  #   geom_sf(data = all_25km_df, size = 0.5, alpha = 0.6) +
  #   geom_sf(data = filter(grids_usa, id_cells %in% unique(all_25km_df$id_cells)), alpha = 0,
  #           size = 0.15, color = "red")
  # saveRDS(p_map, file = "figures/all_cells_w_data.rds")
  # ggsave("figures/all_cells_w_data.png", plot = p_map, width = 9, height = 6)
  
  
  n_distinct(all_25km_df$sp) # 1401
  n_distinct(all_25km_df$id_cells) # 2227
  
  # how many species have flowering date across year in winter? ----
  
  x = all_25km_df |> 
    st_drop_geometry() |> 
    group_by(sp, id_cells) |> 
    summarise(over_winter = any(doy > 330),
              over_spring = any(doy < 60)) |> 
    mutate(overyr = over_winter & over_spring) |> 
    filter(overyr)
  n_distinct(x$sp) # 333 remove them?? decide later
  sort(unique(x$sp)) # or make those doy in winter as negative values?
  hist(filter(all_dat, sp %in% unique(x$sp))$mth)
  filter(all_25km_df, sp == "Acmispon rigidus") 
  
  all_25km_df = filter(all_25km_df, !sp %in% unique(x$sp))
  n_distinct(all_25km_df$sp) # 1068
  
  write_rds(all_25km_df, file = "data_output/all_25km_df_raw.rds")
} else {
  all_25km_df = read_rds("data_output/all_25km_df_raw.rds")
}


# insects ----
if(!file.exists("data_output/all_25km_df_raw_insects.rds")){
  all_dat_insects = readRDS("data_output/d2_insects.rds") |>  
    dplyr::select(longitude = decimalLongitude, latitude = decimalLatitude, 
                  everything()) |>  
    rename(observed_on = eventDate)
  
  # covert the raw data frame to sf
  all_dat_sf_insects = st_transform(st_as_sf(all_dat_insects, coords = c("longitude", "latitude"), 
                                             crs = 4326, remove = FALSE), 
                                    crs = st_crs(us_map))
  all_dat_sf_cells_insects = st_join(all_dat_sf_insects, grids_usa) 
  sum(is.na(all_dat_sf_cells_insects$id_cells))
  all_dat_sf_cells_insects = drop_na(all_dat_sf_cells_insects, id_cells)
  st_geometry(all_dat_sf_cells_insects) = all_dat_sf_cells_insects$geometry
  
  # for each species, check which cell-yr has enough data and filter out the others
  splist_insects = sort(unique(all_dat_sf_cells_insects$sp))
  
  # remove migrotory insects?
  insect_mig = read_csv("data/insect_sp_mig.csv")
  insect_mig_rm = filter(insect_mig, migratory == "y" | (!is.na(notes)))
  any(insect_mig_rm$sp %in% d$sp) # FALSE, confirmed removed
  
  # remove flying year around insects?
  insect_traits = read_csv("data_output/insect_traits.csv")
  insect_yeararound = filter(insect_traits, grepl("Flies year round", notes))$sp
  insect_yeararound = unique(c(insect_mig_rm$sp, insect_yeararound))
  
  insect_yeararound %in% splist_insects
  splist_insects = setdiff(splist_insects, insect_yeararound)
  
  all_25km_insects = vector("list", length = length(splist_insects))
  names(all_25km_insects) = splist_insects
  for(i in seq_along(splist_insects)){
    cat("i = ", i, " ", splist_insects[i], "\n")
    x = filter(all_dat_sf_cells_insects, sp == splist_insects[i])
    all_25km_insects[[i]] <- suppressMessages(plt_summary(x, n_per_cell = 5, days_per_cell = 3, show_fig = FALSE, 
                                                  plt_base = plt_base, grids_usa = grids_usa))
  }
  
  map_chr(all_25km_insects, class)
  is.null(all_25km_insects)
  all_25km_insects = all_25km_insects[!map_lgl(all_25km_insects, is.null)]
  all_25km_df_insects = map_dfr(all_25km_insects, "dat_to_use")
  
  # confirm that each cell-yr-sp has >= 5 observations
  all_25km_df_insects |> 
    st_drop_geometry() |> 
    group_by(sp, yr, id_cells) |> 
    tally() |> arrange(n)
  
  # p_map_insects = plt_base +
  #   geom_sf(data = all_25km_df_insects, size = 0.5, alpha = 0.6) +
  #   geom_sf(data = filter(grids_usa, id_cells %in% unique(all_25km_df_insects$id_cells)), alpha = 0, 
  #           size = 0.15, color = "red") 
  # # saveRDS(p_map_insects, file = "figures/all_cells_w_data_insects.rds")
  # ggsave("figures/all_cells_w_data_insects.png", plot = p_map_insects, width = 9, height = 6)
  
  
  n_distinct(all_25km_df_insects$sp)  # 465
  n_distinct(all_25km_df_insects$id_cells) # 3201
  
  # only keep grid cells with plants data
  cells_w_plants = read_rds("data_output/all_25km_df_raw.rds") |> 
    st_drop_geometry() |> 
    pull(id_cells) |> unique()
  
  all_25km_df_insects = filter(all_25km_df_insects, id_cells %in% cells_w_plants)
  n_distinct(all_25km_df_insects$sp)  # 462
  n_distinct(all_25km_df_insects$id_cells) # 1567
  
  # how many species have flowering date across year in winter? ----
  
  # x = all_25km_df_insects |>
  #   st_drop_geometry() |>
  #   group_by(sp, id_cells) |>
  #   summarise(over_winter = any(doy > 330),
  #             over_spring = any(doy < 10)) |>
  #   mutate(overyr = over_winter & over_spring) |>
  #   filter(overyr)
  # n_distinct(x$sp) # 140 remove them?? decide later
  # sort(unique(x$sp)) # or make those doy in winter as negative values?
  # hist(filter(all_dat_insects, sp %in% unique(x$sp))$mth)
  # 
  # all_25km_df_insects = filter(all_25km_df_insects, !sp %in% unique(x$sp))
  
  
  
  filter(all_25km_df_insects, id_cells == "cell_3759", sp == "Leptotes marina") |> 
    st_drop_geometry() 
  
  write_rds(all_25km_df_insects, file = "data_output/all_25km_df_raw_insects.rds")
} else {
  all_25km_df_insects = read_rds("data_output/all_25km_df_raw_insects.rds")
}

