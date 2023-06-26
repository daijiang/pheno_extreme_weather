source("code/01_get_data_iNat.R")

all_dat = readRDS("data_output/d2.rds") |>  
  dplyr::select(longitude = decimalLongitude, latitude = decimalLatitude, 
                scientific_name = scientificName,
                everything()) |>  
  rename(observed_on = eventDate)
table(all_dat$scientific_name)
n_distinct(all_dat$sp) # 2018
table(all_dat$sp)
sort(unique(all_dat$sp))

# make grid
cell_size = 25000
us_map = readRDS("data/usa_map.rds")
grids = st_make_grid(us_map, cellsize = c(cell_size, cell_size))
grids_us = st_intersects(grids, us_map, sparse = F)
grids2 = grids[grids_us]

plot(us_map[0])
plot(grids2, add = T)
# add grid cell id
grids_usa = mutate(st_sf(geometry = grids2), id_cells = paste0("cell_", 1:n()))
# write_rds(grids_usa, "data_output/grids_usa.rds")

plt_base = ggplot() +
  geom_sf(data = us_map) +
  geom_sf(data = readRDS("data/lakes.rds"), color = "gray50", fill = "white") +
  geom_sf(data = grids_usa, alpha = 0, size = 0.1, color = "gray")

# covert the raw data frame to sf
all_dat_sf = st_transform(st_as_sf(all_dat, coords = c("longitude", "latitude"), 
                                   crs = 4326, remove = FALSE), 
                          crs = st_crs(us_map))
all_dat_sf_cells = st_join(all_dat_sf, grids_usa) # why linux said their crs are different?? No issue on Mac
sum(is.na(all_dat_sf_cells$id_cells))
all_dat_sf_cells = drop_na(all_dat_sf_cells, id_cells)
st_geometry(all_dat_sf_cells) = all_dat_sf_cells$geometry

# for each species, check which cell-yr has enough data and filter out the others
splist = sort(unique(all_dat_sf_cells$sp))

if(!file.exists("data_output/all_25km_df_raw.rds")){
  all_25km = vector("list", length = length(splist))
  names(all_25km) = splist
  for(i in seq_along(splist)){
    cat("i = ", i, " ", splist[i], "\n")
    x = filter(all_dat_sf_cells, sp == splist[i])
    all_25km[[i]] <- suppressMessages(plt_summary(x, n_per_cell = 5, days_per_cell = 3, show_fig = FALSE, 
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
  
  p_map = plt_base +
    geom_sf(data = all_25km_df, size = 0.5, alpha = 0.6) +
    geom_sf(data = filter(grids_usa, id_cells %in% unique(all_25km_df$id_cells)), alpha = 0, 
            size = 0.15, color = "red") 
  saveRDS(p_map, file = "figures/all_cells_w_data.rds")
  ggsave("figures/all_cells_w_data.png", plot = p_map, width = 9, height = 6)
  
  
  n_distinct(all_25km_df$sp) # 1334
  n_distinct(all_25km_df$id_cells) # 2038
  
  # how many species have flowering date across year in winter? ----
  
  x = all_25km_df |> 
    st_drop_geometry() |> 
    group_by(sp, id_cells) |> 
    summarise(over_winter = any(doy > 330),
              over_spring = any(doy < 60)) |> 
    mutate(overyr = over_winter & over_spring) |> 
    filter(overyr)
  n_distinct(x$sp) # 275 remove them?? decide later
  sort(unique(x$sp)) # or make those doy in winter as negative values?
  hist(filter(all_dat, sp %in% unique(x$sp))$mth)
  filter(all_25km_df, sp == "Acmispon rigidus") 
  
  all_25km_df = filter(all_25km_df, !sp %in% unique(x$sp))
  
  write_rds(all_25km_df, file = "data_output/all_25km_df_raw.rds")
} else {
  all_25km_df = read_rds("data_output/all_25km_df_raw.rds")
}


