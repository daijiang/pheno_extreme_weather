source("code/00_pkg_functions.R")

if(!file.exists("data_output/d2.rds")){
  annotated_inat <- arrow::read_csv_arrow("../../common_data/phenobase_iNat/data/phenobase_dwca/observations.csv")
  
  flowering_yes_inat = filter(annotated_inat, 
                              reproductiveCondition %in% c("flowering", "flower budding", 
                                                           "flowering|flower budding"))
  
  
  
  # where to focus? US?
  names(flowering_yes_inat)
  table(flowering_yes_inat$datasetName)
  table(flowering_yes_inat$basisOfRecord)
  sort(table(flowering_yes_inat$countryCode)) # 1.4 m from US, the next largest is CA with 181 k
  # so focus on US alone? seems to be reasonable
  table(flowering_yes_inat$stateProvince)
  
  usa_box = tibble(long_min = -127.86923, long_max = -65.15705,
                   lat_min = 22.94453, lat_max = 51.64284)
  
  d <- flowering_yes_inat |> 
    drop_na(decimalLongitude, decimalLatitude) |> 
    filter(datasetName == "iNaturalist research-grade observations",
           # countryCode %in% c("US"), # not sure how accurate this be
           decimalLongitude > usa_box$long_min, decimalLongitude < usa_box$long_max,
           decimalLatitude > usa_box$lat_min, decimalLatitude < usa_box$lat_max) |> 
    dplyr::select(scientificName, taxonRank, family, genus, eventDate, decimalLatitude,
                  decimalLongitude, stateProvince, coordinateUncertaintyInMeters, 
                  reproductiveCondition) |> 
    distinct()
  
  # us = readRDS("data/ecography/usa_map.rds") 
  
  sort(unique(d$stateProvince))
  
  d = filter(d, !stateProvince %in% c("Alaska", "Hawaii")) 
  
  summary(d$coordinateUncertaintyInMeters)
  hist(d$coordinateUncertaintyInMeters)
  sum(d$coordinateUncertaintyInMeters < 30000, na.rm = T)
  
  # uncertainty
  d = filter(d, is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters < 30000)
  
  d = mutate(d, eventDate = as.Date(eventDate))
  
  # add the GCB data?
  d_gcb = read_csv("data/all_dat_gcb.csv")
  table(d_gcb$flowers)
  d_gcb = rename(d_gcb, decimalLatitude = latitude, decimalLongitude = longitude, 
                 scientificName = scientific_name, eventDate = observed_on) |> 
    mutate(reproductiveCondition = "flowering_mannual_score",
           taxonRank = "species") |> 
    select(-id_iNat, -flowers)
  d_gcb$scientificName[d_gcb$scientificName == "Viola sororia_1"] = "Viola sororia"
  d_gcb$scientificName[d_gcb$scientificName == "Viola sororia_2"] = "Viola sororia"
  
  d = bind_rows(d, d_gcb)
  # when? recent years.
  
  
  d = mutate(d, yr = year(eventDate), mth = month(eventDate), doy = yday(eventDate)) |> 
    select(-coordinateUncertaintyInMeters)
  
  table(d$taxonRank)
  table(d$yr)
  
  d = filter(d, yr %in% 2016:2022,
             taxonRank %in% c("species", "subspecies", "variety"))
  
  n_distinct(d$scientificName) # 14,031
  sort(unique(d$scientificName))
  
  d = mutate(d, sp = str_extract(scientificName, "^[^ ]+ [^ ]+"))
  n_distinct(d$sp) # 11,593
  sort(unique(d$sp))
  
  dn = d |> group_by(sp) |> tally()
  # most species have < 100 observations
  sp_keep = filter(dn, n >= 100)$sp
  
  d2 = filter(d, sp %in% sp_keep, 
              decimalLongitude < 0)
  
  
  # d2 |> group_by(sp) |> tally() |> View()
  
  saveRDS(d2, "data_output/d2.rds")
} else {
  d2 = read_rds("data_output/d2.rds")
}





