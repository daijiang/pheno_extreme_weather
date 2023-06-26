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
  
  n_distinct(all_25km_pheno$sp) # 1012
  n_distinct(all_25km_pheno$id_cells) # 1742
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
  
  saveRDS(all_25km_pheno2, file = "data_output/all_25km_pheno2.rds")
} else {
  all_25km_pheno2 = read_rds("data_output/all_25km_pheno2.rds")
}



