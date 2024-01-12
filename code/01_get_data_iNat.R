source("code/00_pkg_functions.R")

if(!file.exists("data_output/d2_insects.rds")){
  # data downloaded in common_data/phenobase_iNat folder
  inat_gbif = data.table::fread(input = "../../common_data/phenobase_iNat/data/iNat_GBIF/observations.csv",
                                select = c("eventDate", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", 
                                           "countryCode", "scientificName", "taxonRank", "family", "kingdom", "sex",
                                           "lifeStage", "reproductiveCondition"))
  
  # where to focus? US?
  # so focus on US alone? seems to be reasonable
  usa_box = tibble(long_min = -127.86923, long_max = -65.15705,
                   lat_min = 22.94453, lat_max = 51.64284)
  
  inat_gbif <- inat_gbif |> 
    drop_na(decimalLongitude, decimalLatitude) |> 
    filter(decimalLongitude > usa_box$long_min, decimalLongitude < usa_box$long_max,
           decimalLatitude > usa_box$lat_min, decimalLatitude < usa_box$lat_max) |> 
    filter(countryCode %in% c("US"), # not sure how accurate this be
           is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters < 30000) 
  
  inat_gbif = mutate(inat_gbif, yr = year(eventDate), mth = month(eventDate), doy = yday(eventDate)) |> 
    filter(yr >= 2016)
  
  inat_gbif = filter(inat_gbif, taxonRank %in% c("species", "subspecies", "variety", "hybrid"))
  
  inat_gbif = mutate(inat_gbif, sp = str_extract(scientificName, "^[^ ]+ ×? ?[^ ]+"),
                     sp = str_trim(sp),
                     genus = str_extract(sp, "^[^ ]+"))
  
  inat_gbif = dplyr::select(inat_gbif, -coordinateUncertaintyInMeters, -countryCode, -scientificName)
  
  ## confirmed that all data are in the US
  # dplyr::select(inat_gbif, decimalLongitude, decimalLatitude) |>
  #   distinct() |>
  #   sample_frac(0.05) |>
  #   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4316) |>
  #   plot()
  
  ## plants ----
  
  inat_plant = filter(inat_gbif, kingdom == "Plantae",  
                      reproductiveCondition %in% c("flowering", "flower budding", "flowering|flower budding")) |> 
    dplyr::select(sp, decimalLatitude, decimalLongitude, eventDate, yr, mth, doy)
  n_distinct(inat_plant$sp) # 12,238
  table(inat_plant$yr)
  
  # add the GCB data?
  # add the GCB data?
  d_gcb = read_csv("data/all_dat_gcb.csv")
  table(d_gcb$flowers)
  d_gcb = rename(d_gcb, decimalLatitude = latitude, decimalLongitude = longitude, 
                 sp = scientific_name, eventDate = observed_on) |> 
    dplyr::select(-id_iNat, -flowers)
  d_gcb$sp[d_gcb$sp == "Viola sororia_1"] = "Viola sororia"
  d_gcb$sp[d_gcb$sp == "Viola sororia_2"] = "Viola sororia"
  d_gcb = mutate(d_gcb, yr = year(eventDate), doy = yday(eventDate))
  
  d_plant = bind_rows(inat_plant, d_gcb)
  # when? recent years.
  
  
  table(d_plant$yr)
  
  d_plant = filter(d_plant, yr %in% 2016:2022)
  
  n_distinct(d_plant$sp) # 11,787
  
  dn = d_plant |> group_by(sp) |> tally()
  # most species have < 100 observations
  sp_keep = filter(dn, n >= 100)$sp
  
  d2_plants = filter(
    d_plant, sp %in% sp_keep,
    decimalLongitude < 0
  )
  
  n_distinct(d2_plants$sp)
  
  saveRDS(d2_plants, "data_output/d2_plants.rds")
  
  # insects ----
  
  # d_insects = readRDS("data/lepidoptera_usa_16_22.rds")
  d_insects = filter(inat_gbif, 
                     family %in% c("Papilionidae", "Nymphalidae", "Pieridae", 
                                   "Lycaenidae", "Hesperiidae", "Riodinidae",
                                   "Saturniidae", "Sphingidae"))
  
  count(d_insects, lifeStage)
  
  
  # load("data_output/models.RData")
  # inat_insect2 = filter(d_insects, sp %in% unique(model.frame(m_onset_final)$sp))
  # inat_insect2_larva = inat_insect2 |>
  #   group_by(sp) |>
  #   summarise(n_larva = sum(lifeStage == "larva", na.rm = T),
  #             n_total = n()) |>
  #   mutate(lp = n_larva / n_total)
  # summary(inat_insect2_larva$lp)
  # # # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  # # # 0.000000 0.000000 0.001533 0.042218 0.028358 0.814536 
  
  
  d_insects = filter(d_insects, !lifeStage %in% c("egg", "larva", "pupa"))
  
  table(d_insects$yr)
  
  
  n_distinct(d_insects$sp) # 906
  sort(unique(d_insects$sp))
  
  
  dn = d_insects |> group_by(sp) |> tally()
  # most species have < 100 observations
  sp_keep = filter(dn, n >= 100)$sp
  
  d2_insects = filter(d_insects, sp %in% sp_keep) |> 
    dplyr::select(sp, decimalLatitude, decimalLongitude, eventDate, yr, mth, doy)
    n_distinct(d2_insects$sp)
    dim(d2_insects)
  
  write_rds(d2_insects, "data_output/d2_insects.rds")
} else {
  d2_plants = read_rds("data_output/d2_plants.rds") # plants
  d2_insects = read_rds("data_output/d2_insects.rds")
}

