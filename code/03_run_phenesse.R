source("code/02_make_grid_cells.R")

if(!file.exists("data_output/all_25km_pheno2.rds")) {
  all_25km <- st_drop_geometry(all_25km_df)
  
  splist = sort(unique(all_25km$sp))
  
  if(!dir.exists("data_output/phenesse_outputs")) 
    dir.create("data_output/phenesse_outputs")
  
  j = 1
  for(i in splist){
    cat(j, "out of ", n_distinct(all_25km$sp), "sp = ", i, "\n")
    x = filter(all_25km, sp == i)
    try(run_phenesse2(df = x, n_item = 1000, onset_perct = 0.05, offset_perct = 0.95, num_cores = 50, save_rds = TRUE))
    j = j + 1
  }
  
  
  all_25km_pheno = list.files("data_output/phenesse_outputs", full.names = T) |> 
    map_dfr(read_rds)
  
  n_distinct(all_25km_pheno$sp) # 1064
  n_distinct(all_25km_pheno$id_cells) # 1748
  table(all_25km_pheno$yr)
  
  od = function(s){
    s1 = abs(s - median(s) ) / mad(s)
    which(s1 < 3.5)
  }
  
  detect_outliers_df = function(df){
    i_onset = od(df$onset)
    i_offset = od(df$offset)
    i_dur = od(df$duration)
    df[intersect(intersect(i_onset, i_offset), i_dur), ]
  }
  
  all_25km_pheno2 = all_25km_pheno |> 
    group_by(sp) |> 
    do(detect_outliers_df(.)) |> 
    filter(duration >= 5) |>  # less than 5 days are unlikely
    ungroup()
  
  all_25km_pheno2 = filter(all_25km_pheno2, duration < 300, onset > 15, offset < 350)
  
  n_distinct(all_25km_pheno2$sp) # 991
  n_distinct(all_25km_pheno2$id_cells) # 1403
  
  all_25km_pheno2 = st_centroid(grids_usa) |> 
    st_transform(crs = 4236) |> 
    st_coordinates() |> 
    as_tibble() |> 
    mutate(id_cells = grids_usa[["id_cells"]]) |> 
    set_names(c("longitude_center", "latitude_center", "id_cells")) |> 
    right_join(all_25km_pheno2)
  
  sp_abund = all_25km_pheno2 |> 
    group_by(sp) |> 
    tally() |> 
    arrange(desc(n))
  
  filter(all_25km_pheno2, sp %in% sp_abund$sp[101:200]) |> 
    ggplot(aes(x = onset, xend = offset, y = latitude_center, yend = latitude_center)) +
    geom_segment() +
    facet_wrap(~sp, scales = "free_y")
  
  
  # maybe drop the top and bottom 10% of the estimated durations??
  all_25km_pheno2_abund = filter(all_25km_pheno2, sp %in% filter(sp_abund, n >= 10)$sp)
  
  all_25km_pheno2_rare = filter(all_25km_pheno2, sp %in% filter(sp_abund, n < 10)$sp)
  
  # for species with at least 10 duration estimates, remove the top 10% (shortest) 
  # probably because of lack of data
  all_25km_pheno2_abund = anti_join(all_25km_pheno2_abund, 
                                    all_25km_pheno2_abund |> 
                                      group_by(sp) |> 
                                      arrange(duration) |> 
                                      slice_head(prop = 0.1) |> 
                                      ungroup()
  )
  
  n_distinct(all_25km_pheno2_abund$sp) # 336
  n_distinct(all_25km_pheno2_rare$sp) # 655
  
  rare_sdd = all_25km_pheno2_rare |> 
    group_by(sp) |> 
    summarise(sdd = sd(duration, na.rm = T)) |> 
    arrange(desc(sdd))
  
  dplyr::select(rare_sdd, sp)[1:100,] |> 
    left_join(all_25km_pheno2_rare) |> 
    mutate(sp = factor(sp, levels = rare_sdd$sp[1:100])) |> 
    ggplot(aes(x = onset, xend = offset, y = latitude_center, yend = latitude_center)) +
    geom_segment() +
    facet_wrap(~sp, scales = "free_y")
  
  # it seems sd of 30 can be a good threshold
  all_25km_pheno2_rare = anti_join(all_25km_pheno2_rare, 
                                   filter(all_25km_pheno2_rare, sp %in% filter(rare_sdd, sdd > 20)$sp) |> 
                                     group_by(sp) |> 
                                     slice_min(n = 1, order_by = duration) |> 
                                     ungroup()
  )
  
  all_25km_pheno2 = bind_rows(all_25km_pheno2_abund, all_25km_pheno2_rare)
  
  
  # remove species with < 5 estimations?
  sp_2_keep = all_25km_pheno2 |> 
    group_by(sp) |> 
    tally() |> 
    arrange(n) |> 
    filter(n >= 5) |> 
    pull(sp)
  
  
  all_25km_pheno2 = filter(all_25km_pheno2, sp %in% sp_2_keep)
  
  n_distinct(all_25km_pheno2$sp) # 584
  n_distinct(all_25km_pheno2$id_cells) # 1264
  
  filter(all_25km_pheno2, sp %in% (all_25km_pheno2 |> 
                                             group_by(sp) |> 
                                             tally() |> 
                                             arrange(desc(n)) |> pull(sp) %>% .[1:70])) |> 
    ggplot(aes(x = onset, xend = offset, y = latitude_center, yend = latitude_center)) +
    geom_segment() +
    facet_wrap(~sp, scales = "free_y")
  
  saveRDS(all_25km_pheno2, file = "data_output/all_25km_pheno2.rds")
} else {
  all_25km_pheno2 = read_rds("data_output/all_25km_pheno2.rds")
}

# Insects ----

if(!file.exists("data_output/all_25km_pheno2_insects.rds")) {
  all_25km_insects <- st_drop_geometry(all_25km_df_insects)
  
  splist_insects = sort(unique(all_25km_insects$sp))
  
  if(!dir.exists("data_output/phenesse_outputs_insects")) 
    dir.create("data_output/phenesse_outputs_insects")
  
  j = 1
  for(i in splist_insects){
    cat(j, "out of ", n_distinct(all_25km_insects$sp), "sp = ", i, "\n")
    x = filter(all_25km_insects, sp == i)
    try(run_phenesse2(df = x, n_item = 1000, onset_perct = 0.05, offset_perct = 0.95, 
                      num_cores = 50, save_rds = TRUE, 
                      rds_folder_path = "data_output/phenesse_outputs_insects"))
    j = j + 1
  }
  
  
  all_25km_pheno_insects = list.files("data_output/phenesse_outputs_insects", full.names = T) |> 
    map_dfr(read_rds)
  
  table(all_25km_pheno_insects$yr)
  all_25km_pheno_insects = filter(all_25km_pheno_insects, yr < 2023)
  n_distinct(all_25km_pheno_insects$sp) # 445
  n_distinct(all_25km_pheno_insects$id_cells) # 1451
  
  od = function(s){
    s1 = abs(s - median(s) ) / mad(s)
    which(s1 < 3.5)
  }
  
  detect_outliers_df = function(df){
    i_onset = od(df$onset)
    i_offset = od(df$offset)
    i_dur = od(df$duration)
    df[intersect(intersect(i_onset, i_offset), i_dur), ]
  }
  
  all_25km_pheno2_insects = all_25km_pheno_insects |> 
    group_by(sp) |> 
    do(detect_outliers_df(.)) |> 
    filter(duration >= 5) |>  # less than 5 days are unlikely
    ungroup()
  
  all_25km_pheno2_insects = filter(all_25km_pheno2_insects, duration < 300, onset > 15, offset < 350)
  
  n_distinct(all_25km_pheno2_insects$sp) # 335
  n_distinct(all_25km_pheno2_insects$id_cells) # 1162
  
  # maybe drop the top and bottom 10% of the estimated durations??
  all_25km_pheno2_insects_abund = filter(all_25km_pheno2_insects, sp %in% 
                                           (all_25km_pheno2_insects |> 
                                              group_by(sp) |> 
                                              tally() |> 
                                              filter(n >= 10) |> pull(sp)))
  
  all_25km_pheno2_insects_rare = filter(all_25km_pheno2_insects, sp %in% 
                                          (all_25km_pheno2_insects |> 
                                             group_by(sp) |> 
                                             tally() |> 
                                             filter(n < 10) |> pull(sp)))
  
  # for species with at least 10 duration estimates, remove the top 10% (shortest) 
  # probably because of lack of data
  all_25km_pheno2_insects_abund = anti_join(all_25km_pheno2_insects_abund, 
                                            all_25km_pheno2_insects_abund |> 
                                              group_by(sp) |> 
                                              arrange(duration) |> 
                                              slice_head(prop = 0.1) |> 
                                              ungroup()
  )
  
  n_distinct(all_25km_pheno2_insects_rare$sp) 
  
  rare_sdd = all_25km_pheno2_insects_rare |> 
    group_by(sp) |> 
    summarise(sdd = sd(duration, na.rm = T)) |> 
    arrange(sdd)
  
  # it seems sd of 30 can be a good threshold
  all_25km_pheno2_insects_rare = anti_join(all_25km_pheno2_insects_rare, 
                                           filter(all_25km_pheno2_insects_rare, sp %in% filter(rare_sdd, sdd > 30)$sp) |> 
                                             group_by(sp) |> 
                                             slice_min(n = 1, order_by = duration) |> 
                                             ungroup()
  )
  
  all_25km_pheno2_insects = bind_rows(all_25km_pheno2_insects_abund, all_25km_pheno2_insects_rare)
  
  n_distinct(all_25km_pheno2_insects$sp) # 335
  
  # remove species with < 5 estimations?
  sp_2_keep = all_25km_pheno2_insects |> 
    group_by(sp) |> 
    tally() |> 
    arrange(n) |> 
    filter(n >= 5) |> 
    pull(sp)
  
  
  all_25km_pheno2_insects = st_centroid(grids_usa) |> 
    st_transform(crs = 4236) |> 
    st_coordinates() |> 
    as_tibble() |> 
    mutate(id_cells = grids_usa[["id_cells"]]) |> 
    set_names(c("longitude_center", "latitude_center", "id_cells")) |> 
    right_join(all_25km_pheno2_insects) |> 
    filter(sp %in% sp_2_keep)
  
  # remove migratory insects?
  insect_mig = read_csv("data/insect_sp_mig.csv")
  insect_mig_rm = filter(insect_mig, migratory == "y" | (!is.na(notes)))
  
  all_25km_pheno2_insects = filter(all_25km_pheno2_insects, !sp %in% insect_mig_rm$sp)
  
  dplyr::filter(all_25km_pheno2_insects, sp %in% rare_sdd$sp) |> 
    ggplot(aes(x = onset, xend = offset, y = latitude_center, yend = latitude_center)) +
    geom_segment() +
    facet_wrap(~sp, scales = "free_y")

  filter(all_25km_pheno2_insects, sp %in% (all_25km_pheno2_insects |> 
           group_by(sp) |> 
           tally() |> 
           arrange(desc(n)) |> pull(sp) %>% .[1:70])) |> 
  ggplot(aes(x = onset, xend = offset, y = latitude_center, yend = latitude_center)) +
    geom_segment() +
    facet_wrap(~sp, scales = "free_y")
  
  saveRDS(all_25km_pheno2_insects, file = "data_output/all_25km_pheno2_insects.rds")
} else {
  all_25km_pheno2_insects = read_rds("data_output/all_25km_pheno2_insects.rds")
}


if(!file.exists("data_output/grids_usa_used.rds")){
  grids_usa = read_rds("data_output/grids_usa.rds")
  
  df_25k_pheno = bind_rows(
    mutate(readRDS("data_output/all_25km_pheno2.rds"), taxon = "plant"),
    mutate(readRDS("data_output/all_25km_pheno2_insects.rds"), taxon = "insect")
  )
  df_25k_pheno = mutate(df_25k_pheno, onset = floor(onset), offset = ceiling(offset), 
                        duration = offset - onset, onset_date = as.Date(onset - 1, origin = paste0(yr, "-01-01")),
                        offset_date = as.Date(offset - 1, origin = paste0(yr, "-01-01")))
  
  grids_usa_used = filter(grids_usa, id_cells %in% sort(unique(df_25k_pheno$id_cells)))
  
  write_rds(grids_usa_used, "data_output/grids_usa_used.rds")
}


