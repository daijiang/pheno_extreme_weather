source("code/08_data_exploratory_analysis.R")

# limit to plants vs insects that have at least one common grid cell, and some overlapping in their phenology ----
insect_sp_list = filter(d2, taxon == "insect") |> 
  group_by(sp) |> 
  summarise(onset_median = median(onset),
            offset_median = median(offset), 
            n_est = n(),
            .groups = "drop") |> 
  arrange(desc(n_est)) 
plant_sp_list = sort(unique(filter(d2, taxon == "plant")$sp))
d2_plant = filter(d2, taxon == "plant")
d2_plant_median = group_by(d2_plant, sp) |> 
  summarise(onset_median = median(onset),
            offset_median = median(offset), 
            n_est = n(),
            .groups = "drop")

# iNat occurence
library(arrow)
inat_obs = arrow::open_dataset("../../common_data/phenobase_iNat/data/inaturalist-open-data/metadata/observations/")
angio_taxa = arrow::open_dataset("../../common_data/phenobase_iNat/data/inaturalist-open-data/metadata/taxa/") |> 
  collect()

ewe_sp = sort(unique(d2$sp))
setdiff(ewe_sp, angio_taxa$name)
ewe_inat_sp = tibble(sp_ewe = intersect(ewe_sp, angio_taxa$name)) |> 
  mutate(name = sp_ewe) |> 
  bind_rows(tibble(sp_ewe = c("Argynnis aphrodite", "Argynnis callippe", 
                              "Argynnis coronis", "Argynnis diana",  
                              "Argynnis hesperis",  "Boloria myrina"),
                   name = c("Speyeria aphrodite", "Speyeria callippe", 
                              "Speyeria coronis", "Speyeria diana",  
                              "Speyeria hesperis",  "Boloria selene myrina")))
setdiff(ewe_inat_sp$name, angio_taxa$name)
ewe_inat_sp = left_join(ewe_inat_sp, unique(dplyr::select(angio_taxa, name, taxon_id, active, rank)))
ewe_inat_sp1 = filter(ewe_inat_sp, name == "Maurandella antirrhiniflora")
ewe_inat_sp = filter(ewe_inat_sp, active, rank != "complex")
ewe_inat_sp = bind_rows(ewe_inat_sp, ewe_inat_sp1)
setdiff(ewe_sp, ewe_inat_sp$sp_ewe)
any(duplicated(ewe_inat_sp$taxon_id))

usa_box = tibble(long_min = -127.86923, long_max = -65.15705,
                 lat_min = 22.94453, lat_max = 51.64284)

ewe_inat_obs = filter(inat_obs, taxon_id %in% ewe_inat_sp$taxon_id,
                      longitude > usa_box$long_min, longitude < usa_box$long_max,
                      latitude > usa_box$lat_min, latitude < usa_box$lat_max) |> 
  collect()
count(ewe_inat_obs, quality_grade)

ewe_inat_obs = filter(ewe_inat_obs, quality_grade == "research") |> 
  dplyr::select(observation_uuid, latitude, longitude, taxon_id, observed_on) |> 
  left_join(dplyr::select(ewe_inat_sp, taxon_id, sp_ewe))


xi = filter(ewe_inat_obs, sp_ewe == "Impatiens pallida") |> 
  st_as_sf(coords = c("longitude", "latitude"), remove = F)
plot(st_geometry(xi))
plot(st_convex_hull(st_combine(st_geometry(xi))), add = T)

count(angio_taxa, active)
n_distinct(angio_taxa$taxon_id) # 302,644

plant_convex_hull = vector("list", length = nrow(d2_plant_median))
names(plant_convex_hull) = d2_plant_median$sp
for(j in 1:nrow(d2_plant_median)){
  xj = filter(ewe_inat_obs, sp_ewe == d2_plant_median$sp[j]) |> 
    st_as_sf(coords = c("longitude", "latitude"), remove = F)
  xj_sf_convex_hull = st_convex_hull(st_combine(st_geometry(xj)))
  plant_convex_hull[[j]] = xj_sf_convex_hull
}


insect_plant_pairs = vector("list", length = nrow(insect_sp_list))
for(i in 1:nrow(insect_sp_list)){
  cat("i =", i, "\t")
  d2_insect = filter(d2, sp == insect_sp_list$sp[i])
  insect_onset_median = median(d2_insect$onset)
  insect_offset_median = median(d2_insect$offset)
  active_days = seq(floor(insect_onset_median), ceiling(insect_offset_median))
  
  # space distribution
  xi = filter(ewe_inat_obs, sp_ewe == insect_sp_list$sp[i]) |> 
    st_as_sf(coords = c("longitude", "latitude"), remove = F)
  xi_sf_convex_hull = st_convex_hull(st_combine(st_geometry(xi)))
  
  d2_plant_median_i = d2_plant_median
  d2_plant_median_i$overlap_time_i = FALSE
  d2_plant_median_i$overlap_space_i = FALSE
  
  for(j in 1:nrow(d2_plant_median_i)){
    active_j = seq(floor(d2_plant_median_i$onset_median[j]), ceiling(d2_plant_median_i$offset_median[j]))
    if(length(intersect(active_days, active_j))) d2_plant_median_i$overlap_time_i[j] = TRUE
    
  d2_plant_median_i$overlap_space_i[j] = any(as.vector(st_overlaps(xi_sf_convex_hull, plant_convex_hull[[j]], sparse = F)))
  }
  
  insect_plant_pairs[[i]] = d2_plant_median_i
}

saveRDS(insect_plant_pairs, file = "data_output/insect_plant_pairs.rds")

insect_plant_pairs[[1]]

insect_plant_pairs2 = map(insect_plant_pairs, function(x) arrange(x, desc(n_est)))
insect_plant_pairs2 = map(insect_plant_pairs2, function(x) filter(x, overlap_space_i))
names(insect_plant_pairs2) = insect_sp_list$sp

insect_plant_pairs2[1:10]

xx = insect_plant_pairs2[grep("Argynnis", names(insect_plant_pairs2))]

insect_plant_pairs2 = map(insect_plant_pairs2, function(x) filter(x, n_est >= 10))

saveRDS(insect_plant_pairs2, "data_output/insect_plant_pairs2.rds")

sort(map_dbl(insect_plant_pairs2, nrow))


# pairs identified ----
pairs_to_look = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SoTGsd1KdqpJdKP1PuGezq0aPSdwh4IkCtZKQZ2wceI/edit#gid=0")
library(broom)

dplyr::select(pairs_to_look, sp = lepidopteraSpecies) |> 
  left_join(insect_sp_list)
dplyr::select(pairs_to_look, sp = hostPlant) |> 
  left_join(d2_plant_median)


# load("data_output/models.RData")
# formula_all_onset = formula(m_onset_final)
# formula_all_offset = formula(m_offset_final)
# formula_all_duration = formula(m_duration_final)


pairs_models = vector("list", length = nrow(pairs_to_look))
for(i in 1:nrow(pairs_to_look)){
  cat("i = ", i, "\n")
  d2_p1 = readRDS("data_output/d_raw_for_stat.rds") |> 
    filter(sp %in% c(pairs_to_look$lepidopteraSpecies[i], pairs_to_look$hostPlant[i])) |> 
    mutate_at(c("prop_extreme_warm_days_before_onset_logit", 
                "prop_extreme_cold_days_before_onset_logit",
                "prop_extreme_wet_days_before_onset_logit",
                "prop_extreme_dry_days_before_onset_logit",
                "prop_extreme_warm_days_within_duration_logit",
                "prop_extreme_cold_days_within_duration_logit",
                "prop_extreme_wet_days_within_duration_logit",
                "prop_extreme_dry_days_within_duration_logit",
                "prop_extreme_warm_days_total_logit",
                "prop_extreme_cold_days_total_logit",
                "prop_extreme_wet_days_total_logit",
                "prop_extreme_dry_days_total_logit",
                "precip_current_yr",
                "tmean_current_yr",
                "temp_seasonality_5_yr_ave",
                "precip_seasonality_5_yr_ave",
                "precip_5_yr_ave",
                "tmean_5_yr_ave", # "pop_25km_log10",
                "elev", "n_days_log"), 
              ~(scale(.) %>% as.vector)) |> 
    dplyr::select(taxon, yr, id_cells, sp, n_days_log, onset, offset, duration_log,
                  elev, tmean_5_yr_ave, precip_5_yr_ave, precip_current_yr, tmean_current_yr,
                  temp_seasonality_5_yr_ave, precip_seasonality_5_yr_ave, # pop_25km_log10,
                  starts_with("prop"))
  
  m_p1_onset = lmerTest::lmer(onset ~ tmean_5_yr_ave + taxon + temp_seasonality_5_yr_ave + 
                                precip_5_yr_ave + precip_seasonality_5_yr_ave + prop_extreme_warm_days_before_onset_logit + 
                                prop_extreme_cold_days_before_onset_logit + prop_extreme_wet_days_before_onset_logit + 
                                prop_extreme_dry_days_before_onset_logit + n_days_log +
                                tmean_5_yr_ave:taxon + 
                                taxon:precip_5_yr_ave + taxon:prop_extreme_warm_days_before_onset_logit + 
                                taxon:prop_extreme_cold_days_before_onset_logit + taxon:prop_extreme_wet_days_before_onset_logit + 
                                taxon:prop_extreme_dry_days_before_onset_logit + 
                                tmean_5_yr_ave:prop_extreme_warm_days_before_onset_logit +
                                tmean_5_yr_ave:prop_extreme_cold_days_before_onset_logit +
                                precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit +
                                precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit +
                                prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit +
                                tmean_5_yr_ave:taxon:prop_extreme_cold_days_before_onset_logit +
                                taxon:precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit +
                                taxon:precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit +
                                taxon:prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit +
                                (1 | id_cells) + (1 | yr), data = d2_p1)
  # summary(m_p1_onset)
  
  m_p1_offset = lmerTest::lmer(offset ~ tmean_5_yr_ave + taxon + precip_5_yr_ave + precip_seasonality_5_yr_ave + 
                                 prop_extreme_warm_days_within_duration_logit + prop_extreme_cold_days_within_duration_logit + 
                                 prop_extreme_wet_days_within_duration_logit + prop_extreme_dry_days_within_duration_logit + 
                                 n_days_log + tmean_5_yr_ave:taxon + taxon:precip_5_yr_ave + taxon:prop_extreme_warm_days_within_duration_logit + 
                                 taxon:prop_extreme_cold_days_within_duration_logit + taxon:prop_extreme_dry_days_within_duration_logit + 
                                 tmean_5_yr_ave:prop_extreme_cold_days_within_duration_logit + 
                                 precip_5_yr_ave:prop_extreme_wet_days_within_duration_logit + 
                                 prop_extreme_warm_days_within_duration_logit:prop_extreme_dry_days_within_duration_logit + 
                                 prop_extreme_warm_days_within_duration_logit:prop_extreme_wet_days_within_duration_logit + 
                                 tmean_5_yr_ave:taxon:prop_extreme_cold_days_within_duration_logit + 
                                 taxon:prop_extreme_warm_days_within_duration_logit:prop_extreme_dry_days_within_duration_logit +
                                 (1 | id_cells) + (1 | yr), data = d2_p1)
  # summary(m_p1_offset)
  
  m_p1_duration = lmerTest::lmer(duration_log ~ tmean_5_yr_ave + taxon + temp_seasonality_5_yr_ave + 
                                   precip_5_yr_ave + prop_extreme_warm_days_total_logit + prop_extreme_cold_days_total_logit + 
                                   prop_extreme_wet_days_total_logit + prop_extreme_dry_days_total_logit + 
                                   n_days_log + tmean_5_yr_ave:taxon + 
                                   taxon:temp_seasonality_5_yr_ave + taxon:precip_5_yr_ave + 
                                   taxon:prop_extreme_cold_days_total_logit + taxon:prop_extreme_wet_days_total_logit + 
                                   taxon:prop_extreme_dry_days_total_logit + tmean_5_yr_ave:prop_extreme_warm_days_total_logit + 
                                   tmean_5_yr_ave:prop_extreme_cold_days_total_logit + precip_5_yr_ave:prop_extreme_wet_days_total_logit + 
                                   precip_5_yr_ave:prop_extreme_dry_days_total_logit + 
                                   tmean_5_yr_ave:taxon:prop_extreme_cold_days_total_logit + 
                                   taxon:precip_5_yr_ave:prop_extreme_wet_days_total_logit + 
                                   taxon:precip_5_yr_ave:prop_extreme_dry_days_total_logit +
                                   (1 | id_cells) + (1 | yr), data = d2_p1)
  # summary(m_p1_duration)
  
  pairs_models[[i]] = bind_rows(
    tidy(m_p1_onset) |> filter(effect == "fixed") |> 
      mutate(phenology = "onset"),
    tidy(m_p1_offset) |> filter(effect == "fixed") |> 
      mutate(phenology = "offset"),
    tidy(m_p1_duration) |> filter(effect == "fixed") |> 
      mutate(phenology = "duration")
  ) |> 
    mutate(sp_pairs = paste(pairs_to_look$lepidopteraSpecies[i], pairs_to_look$hostPlant[i], sep = "_"))
}

pairs_models = bind_rows(pairs_models)

filter(pairs_models, grepl(".+[:].+[:].+$", term)) |> 
  group_by(term) |> 
  mutate(p.adj = p.adjust(p.value, method = "BH"),
    p.value = round(p.value, 3),
    p.adj = round(p.adj, 3)) |> 
  View()
