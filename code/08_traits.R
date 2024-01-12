source("code/07_get_all_envi_data.R")

df_25k_pheno_all

plant_growth_form = read_csv("data_output/trait_growth_form.csv")

insect_traits = read_csv("data_output/insectSppListTraits.csv")
summary(insect_traits)
sum(is.na(insect_traits$overwintering.stage))
sum(is.na(insect_traits$voltinism))
count(insect_traits, notes)
table(insect_traits$overwintering.stage)
count(insect_traits, overwintering.stage)
count(insect_traits, voltinism)
insect_traits = mutate(insect_traits, 
                       voltinism = recode(voltinism, "Univoltine" = "uni"),
                       voltinism = tolower(voltinism),
                       overwintering.stage = recode(overwintering.stage, "Eggs" = "Egg"))
insect_traits$voltinism[insect_traits$sp == "Hermeuptychia sosybius"] = "multi"
write_csv(insect_traits, "data_output/insect_traits.csv")

# remove insects with NAs

if(FALSE){
  
  # plants ----
  plant_sp_list = sort(unique(filter(df_25k_pheno_all, taxon == "plant")$sp))
  
  if(!file.exists("data_output/trait_BIEN.csv")){
    target_traits = c(
      "flower color", "flower pollination syndrome", "leaf area",
      "leaf area per leaf dry mass", "leaf carbon content per leaf dry mass",
      "leaf life span", "leaf nitrogen content per leaf dry mass",
      "leaf phosphorus content per leaf dry mass", "longest whole plant longevity",
      "root dry mass", "seed mass", "whole plant dispersal syndrome",
      "whole plant growth form", "whole plant height",
      "whole plant sexual system", "whole plant vegetative phenology",
      "whole plant woodiness", "plant flowering begin",
      "plant flowering duration"
    )
    sp_traits = BIEN::BIEN_trait_traitbyspecies(species = plant_sp_list, trait = target_traits)
    sp_traits = as_tibble(sp_traits)
    sort(unique(sp_traits$trait_name))
    
    filter(sp_traits, trait_name == "plant flowering duration") # only 9 records
    filter(sp_traits, trait_name == "plant flowering duration") # 132 species
    # probably remove them?
    
    sp_traits = dplyr::filter(sp_traits, !trait_name %in% c("plant flowering begin",
                                                            "plant flowering duration"))
    
    sp_traits2 = left_join(sp_traits,
                           data_frame(trait_name = target_traits[-c(18, 19)],
                                      catg = c("char", "char", "num", "num", "num",
                                               "num", "num", "num", "num", "num", "num", "char",
                                               "char", "num", "char", "char", "char")))
    
    unique(filter(sp_traits2, trait_name == "whole plant growth form")$trait_value)
    trait_units = distinct(dplyr::select(sp_traits2, trait_name, unit))
    write_csv(trait_units, "data_output/trait_BIEN_units.csv")
    
    
    unique(filter(sp_traits2, trait_name == "whole plant growth form")$trait_value) %>% tolower() %>% unique() %>% sort()
    filter(sp_traits2, trait_name == "whole plant growth form", trait_value == "4")
    
    sp_traits2 = anti_join(sp_traits2, filter(sp_traits2, trait_name == "whole plant growth form", trait_value == "4"))
    
    summ2 = function(x){
      if(all(x$catg == "char")){
        xx = tolower(str_trim(x$trait_value))
        xx = gsub(pattern = "[*]$", "", xx)
        as.data.frame(table(xx))
      } else {
        data.frame(xx = NA, Freq = median(as.numeric(x$trait_value, na.rm = T)))
      }
    }
    sp_traits3 = dplyr::select(sp_traits2, sp = scrubbed_species_binomial, trait_name, trait_value, unit, catg) %>%
      group_by(sp, trait_name) %>%
      do(summ2(.))
    
    sp_trait_catg = dplyr::filter(sp_traits3, !is.na(xx))
    sp_trait_num = dplyr::filter(sp_traits3, is.na(xx))
    
    
    sp_trait_catg2 = sp_trait_catg %>% group_by(sp, trait_name) %>%
      arrange(desc(Freq)) %>% slice(1)
    group_by(sp_trait_catg2, sp, trait_name) %>% tally() %>% arrange(desc(n))
    
    dplyr::select(sp_trait_catg2, -Freq) %>% rename(value = xx) %>%
      spread(trait_name, value)
    
    trait_bien = left_join(
      dplyr::select(sp_trait_catg2, -Freq) %>% rename(value = xx)%>%
        spread(trait_name, value),
      rename(sp_trait_num, value = Freq) %>% dplyr::select(-xx)%>%
        spread(trait_name, value))
    write_csv(trait_bien, file = "data_output/trait_BIEN.csv")
  } else {
    trait_bien = read_csv("data_output/trait_BIEN.csv")
  }
  
  trait_bien
  for(i in 1:ncol(trait_bien)){
    cat(colnames(trait_bien)[i], "has", sum(is.na(trait_bien[, i])), "NAs \n")
  }
  
  
  names(trait_bien)
  # remove traits poorly covered
  trait_bien = dplyr::select(trait_bien, sp, 
                             flower_color = `flower color`, 
                             pollination = `flower pollination syndrome`,
                             growth_form = `whole plant growth form`,
                             ever_deciduous = `whole plant vegetative phenology`,
                             wood_herb = `whole plant woodiness`, 
                             leaf_area = `leaf area`, 
                             sla = `leaf area per leaf dry mass`,
                             leaf_carbon = `leaf carbon content per leaf dry mass`,
                             leaf_nitrogen = `leaf nitrogen content per leaf dry mass`,
                             root_mass = `root dry mass`,
                             seed_mass = `seed mass`, 
                             plant_height = `whole plant height`)
  
  
  
  
  
  # GIFT?? ----
  library(GIFT)
  trait_meta <- GIFT_traits_meta()
  
  if(!file.exists("data_output/trait_GIFT.rds")){
    trait_gift = GIFT_traits(trait_IDs = c("1.1.1", "1.2.1", "1.6.2", "2.1.1", "2.4.1", "3.6.1", "4.2.1", "4.1.3"))
    saveRDS(trait_gift, "data_output/trait_GIFT.rds")
  } else {
    trait_gift = readRDS("data_output/trait_GIFT.rds")
  }
  
  
  n_distinct(trait_gift$work_species)
  sp_missing = setdiff(plant_sp_list, unique(trait_gift$work_species))
  lirrr::tnrs_match_names_2(sp_missing)
  "Cicerbita muralis" %in% unique(trait_gift$work_species)
  "Sibbaldia retusa" %in% unique(trait_gift$work_species)
  
  trait_gift2 = filter(trait_gift, work_species %in% plant_sp_list) |> as_tibble()
  
  
  
  
  # TRY ----
  
  # fill holes with TRY data
  
  # 42	Plant growth form
  # 197	Plant functional type (PFT)
  # 3106	Plant height vegetative
  # 343	Plant life form (Raunkiaer life form)
  # 38	Plant woodiness
  # 29	Pollination syndrome
  # 3117	Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or exclu
  # 207	Flower color
  # 205	Flower pollinator and type of reward
  # 42, 197, 3106, 343, 38, 29, 3117, 207, 205
  
  try_sp = read_tsv("https://www.try-db.org/dnld/TryAccSpecies.txt")
  setdiff(plant_sp_list, try_sp$AccSpeciesName) # 27 not there
  filter(try_sp, AccSpeciesName %in% plant_sp_list)$AccSpeciesID |> 
    paste(collapse = ", ")
  
  
  trait_try = data.table::fread("data/plant_TRY.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
  trait_try = as_tibble(trait_try)
  trait_try = filter(trait_try, !is.na(TraitID), !is.na(OrigValueStr), OrigValueStr != "")
  trait_try = mutate(trait_try, 
                     OrigValueStr = utf8::utf8_encode(OrigValueStr), # some non utf-8 characters
                     OrigValueStr = gsub(" cm*$", "", OrigValueStr),
                     OrigValueStr = gsub(">|<", "", OrigValueStr),
                     catgNum = ifelse(grepl(pattern = "^[.0-9E-]*$", OrigValueStr), "num", "catg"))
  sort(unique(trait_try$AccSpeciesName))
  setdiff(trait_bien$sp, sort(unique(trait_try$AccSpeciesName)))
  
  filter(trait_try, TraitName == "Plant growth form") |> View()
  
  trait_try = filter(trait_try, TraitName %in% c(
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
    "Plant growth form",
    "Plant height vegetative",
    "Plant woodiness",
    "Pollination syndrome"
  )) |> 
    mutate(TraitName = recode(TraitName,
                              "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded" = "sla",
                              "Plant growth form" = "growth_form",
                              "Plant height vegetative" = "height",
                              "Plant woodiness" = "woodiness",
                              "Pollination syndrome" = "pollination"))
  
  trait_try = mutate(trait_try, OrigValueStr = tolower(OrigValueStr))
  
  catg_traits = c("growth_form", "pollination", "woodiness")
  num_traits = c("height", "sla")
  
  
  # growth form ---- 
  # https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1569
  
  gform = read_tsv("data/growth_form.txt")
  setdiff(plant_sp_list, gform$SPECIES_STD)
  gform2 = filter(gform, SPECIES_STD %in% plant_sp_list)
  
  filter(trait_try, AccSpeciesName %in% setdiff(plant_sp_list, gform$SPECIES_STD),
         TraitName == "growth_form") # no more data from TRY
  trait_bien
  
  gform3 = dplyr::select(gform2, sp = SPECIES_STD, growth_form = GROWTHFORM_STD) |> 
    distinct() |> 
    bind_rows( # BIEN data
      filter(trait_bien, sp %in% setdiff(plant_sp_list, gform$SPECIES_STD)) |> 
        dplyr::select(sp, growth_form) |> 
        drop_na()
    ) |> 
    distinct() |> 
    mutate(growth_form = tolower(growth_form))
  
  # combine GIFT
  names(trait_gift2)
  gform4 = bind_rows(gform3, 
                     dplyr::select(trait_gift2, sp = work_species, growth_form = trait_value_1.2.1) |> 
                       filter(sp %in% setdiff(plant_sp_list, gform3$sp)) |> 
                       drop_na()
  ) |> 
    distinct() |> 
    mutate(growth_form = tolower(growth_form))
  
  setdiff(plant_sp_list, gform4$sp)
  gform5 = bind_rows(gform4, 
                     tibble(sp = c("Asclepias texana", "Erythranthe erubescens", "Galax urceolata",
                                   "Ottleya strigosa", "Penstemon clevelandii", "Penstemon grinnellii",    
                                   "Penstemon rostriflorus", "Penstemon spectabilis", "Pulsatilla nuttalliana",
                                   "Ruellia drummondiana", "Sibbaldiopsis tridentata", "Sphaeralcea angustifolia",
                                   "Tiarella stolonifera", "Vachellia rigidula"),
                            growth_form = c(rep("herb", 13), "shrub")))
  all(plant_sp_list %in% gform5$sp)
  table(gform5$growth_form)
  gform5$growth_form[gform5$growth_form %in% c("climber", "herbaceous climber", "liana", "vine")] = "climber"
  gform5$growth_form[gform5$growth_form == "forb"] = "herb"
  write_csv(gform5, "data_output/trait_growth_form.csv")
  # done with growth form
  
  # pollinator ----
  polli_try = filter(trait_try, TraitName == "pollination") |> 
    dplyr::select(sp = AccSpeciesName,  pollination = OrigValueStr) |> 
    unique() 
  table(polli_try$pollination)
  polli_try$pollination[polli_try$pollination %in% c(
    "anemogamous", "anemophil", "Anemophily", "cleistogamy possible", "cleistogamy the rule",
    "pollination wind", "pseudocleistogamy possible", "self", "selfed", "selfing at failure of outcrossing",
    "selfing often", "selfing possible", "selfing the rule", "wind", "wind always",
    "wind possible", "wind the rule"
  )] = "abiotic"
  polli_try$pollination[polli_try$pollination != "abiotic"] = "biotic"
  polli_try = unique(polli_try)
  duplicated(polli_try$sp)
  polli_try2 = group_by(polli_try, sp) |> 
    count(pollination) |> 
    pivot_wider(names_from = "pollination", values_from = "n") |> 
    mutate(pollination = ifelse(biotic == 1 & is.na(abiotic), "biotic",
                                ifelse(is.na(biotic) & abiotic == 1, "abiotic", 
                                       ifelse(biotic == 1 & abiotic == 1, "biotic", NA)))) |> 
    dplyr::select(sp, pollination) |> ungroup() # all biotic?
  
  # bien
  polli_bien = dplyr::select(trait_bien, sp, pollination) |> 
    drop_na() |> unique()
  
  # gift
  trait_gift2 = as_tibble(trait_gift2)
  polli_gift = dplyr::select(trait_gift2, sp = work_species, pollination = trait_value_3.6.1) |> 
    drop_na() |> unique()
  polli_all = bind_rows(polli_try2, polli_gift) |> 
    bind_rows(polli_bien) |> 
    unique()
  filter(polli_all, sp %in% polli_all[which(duplicated(polli_all$sp)), ]$sp) |> View()
  
  polli_all$pollination[polli_all$sp == "Dactylis glomerata"] = "abiotic" # googled
  polli_all$pollination[polli_all$sp == "Plantago lanceolata"] = "abiotic" # googled
  
  polli_all = unique(polli_all) # 148 species....
  
  setdiff(plant_sp_list, polli_all$sp) 
  # look at the genus?
  polli_all = mutate(polli_all, genus = str_extract(sp, "^[A-Za-z]*"))
  
  tibble(sp = setdiff(plant_sp_list, polli_all$sp)) |> 
    mutate(genus = str_extract(sp, "^[A-Za-z]*"))
  
  # or just give it up and not use this trait ...
  
  # height ---- 
  height_bien = trait_bien |> 
    dplyr::select(sp, height = plant_height) |> 
    drop_na()
  
  height_gift = dplyr::select(trait_gift2, sp = work_species, height = trait_value_1.6.2) |> 
    drop_na() |> unique()
  
  height_try = filter(trait_try, TraitName == "height") |> 
    dplyr::select(sp = AccSpeciesName,  height = OrigValueStr, unit = OrigUnitStr) |> 
    drop_na(height)
  sort(unique(height_try$unit))
  sort(unique(height_try$height))
  height_try$height[height_try$height %in% c(".", "0")] = NA
  height_try$height[grepl(pattern = "-", height_try$height)] = NA
  height_try$unit[height_try$sp == "Ericameria linearifolia"] = "cm"
  height_try$unit[height_try$sp == "Kalmia angustifolia"] = "cm"
  height_try$unit[height_try$sp == "Kalmia angustifolia"] = "cm"
  height_try$unit[height_try$sp == "Glandularia bipinnatifida"] = "cm"
  height_try$unit[height_try$sp == "Calycoseris wrighti"] = "cm"
  height_try$height = as.numeric(height_try$height)
  hist(height_try$height)
  
  height_try = mutate(height_try, height = ifelse(unit == "mm", height * 0.001,
                                                  ifelse(unit == "feet", height * 0.3048,
                                                         ifelse(unit %in% c("cm", "Cm", "(cm)"), height / 100, height))))
  height_try$unit = NULL
  height_try = drop_na(height_try)
  height_try = height_try |> 
    group_by(sp) |> 
    summarise(height = mean(height, na.rm = T))
  
  height_all = bind_rows(height_bien, height_gift) |> 
    bind_rows(height_try) |> 
    group_by(sp) |> 
    summarise(height = mean(height, na.rm = T))
  
  setdiff(plant_sp_list, height_all$sp)
  
  
  # SLA ----
  sla_bien = dplyr::select(trait_bien, sp, sla) |> 
    drop_na() # m2 / kg
  sla_gift = dplyr::select(trait_gift2, sp = work_species, sla = trait_value_4.1.3) |> 
    drop_na() |> # cm2 /g
    mutate(sla = sla / 10) # m2 / kg
  sla_try 
  
  sla_try = filter(trait_try, TraitName == "sla") |> 
    dplyr::select(sp = AccSpeciesName,  sla = OrigValueStr, unit = OrigUnitStr) |> 
    drop_na(sla)
  sort(unique(sla_try$unit))
  sort(unique(sla_try$sla))
  n_distinct(sla_try$sp) # 236
  
  sla_try$sla[sla_try$sla %in% c(".", "0")] = NA
  sla_try$sla[grepl(pattern = "-", sla_try$sla)] = NA
  sla_try$unit[sla_try$sp == "Ericameria linearifolia"] = "cm"
  sla_try$unit[sla_try$sp == "Kalmia angustifolia"] = "cm"
  sla_try$unit[sla_try$sp == "Kalmia angustifolia"] = "cm"
  sla_try$unit[sla_try$sp == "Glandularia bipinnatifida"] = "cm"
  sla_try$unit[sla_try$sp == "Calycoseris wrighti"] = "cm"
  sla_try$sla = as.numeric(sla_try$sla)
  hist(sla_try$sla)
  
  sla_try = mutate(sla_try, sla = ifelse(unit == "mm", sla * 0.001,
                                         ifelse(unit == "feet", sla * 0.3048,
                                                ifelse(unit %in% c("cm", "Cm", "(cm)"), sla / 100, sla))))
  sla_try$unit = NULL
  sla_try = drop_na(sla_try)
  sla_try = sla_try |> 
    group_by(sp) |> 
    summarise(sla = mean(sla, na.rm = T))
  
  sla_all = bind_rows(sla_bien, sla_gift) |> 
    bind_rows(sla_try) |> 
    group_by(sp) |> 
    summarise(sla = mean(sla, na.rm = T))
  
  
  
  x = map(catg_traits, function(x){
    as.data.frame(table(filter(trait_try, TraitName == x)$OrigValueStr))
  })
  names(x) = catg_traits
  x
  
  x2 = map_dbl(catg_traits, function(x){
    n_distinct(filter(trait_try, TraitName == x)$AccSpeciesName)
  })
  names(x2) = catg_traits
  data.frame(x2) %>% View()
  
  
  summ3 = function(x){
    as.data.frame(table(x), stringsAsFactors = F)
  }
  trait_try_catg_summ = group_by(trait_try_catg, AccSpeciesName, TraitName) %>% 
    do(summ3(.$OrigValueStr))
  
  trait_try_num = filter(trait_try, TraitName %in% num_traits)
  sort(unique(trait_try_num$TraitName))
  sort(unique(trait_try$AccSpeciesName))
  trait_try_num_median = group_by(trait_try_num, AccSpeciesName, TraitName) %>% 
    summarise(value = median(StdValue, na.rm = T),
              unit = UnitName[1])
  sort(table(trait_try_num_median$TraitName))
  dplyr::select(trait_try_catg, SpeciesName, TraitName, OrigValueStr, OrigUnitStr) %>% View()
  
  filter(trait_try_num_median, TraitName == "Leaf area") %>% as.data.frame()
  names(trait_BIEN)
  trait_try_num_median = mutate(trait_try_num_median, 
                                TraitName = recode(TraitName,
                                                   "Leaf area" = "leaf_area",
                                                   "Leaf area (in case of compond leaves: leaflet, petiole and rachis excluded)" = "leaf_area",
                                                   "Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis excluded" = "sla",
                                                   "Leaf area per leaf dry mass (specific leaf area, SLA)" = "sla",
                                                   "Leaf carbon (C) content per leaf dry mass" = "leaf_carbon",
                                                   "Leaf nitrogen (N) content per leaf dry mass" = "leaf_nitrogen",
                                                   "Root dry mass per plant" = "root_mass",
                                                   "Plant height generative" = "plant_height"
                                ))
  trait_try_num_median = filter(trait_try_num_median, TraitName %in% c("leaf_area", "sla", "leaf_carbon", "leaf_nitrogen", "root_mass", "plant_height"))
  dplyr::select(ungroup(trait_try_num_median), TraitName, unit) %>% distinct
  trait_units
  trait_try_num_median = ungroup(trait_try_num_median) %>% 
    group_by(AccSpeciesName, TraitName) %>% 
    summarise(value = mean(value, na.rm = T))
  trait_try_num_median_wide = spread(ungroup(trait_try_num_median), TraitName, value)
  
  traits_num = bind_rows(
    dplyr::select(trait_BIEN, sp, leaf_area:plant_height) %>% 
      gather("TraitName", "value", -sp),
    rename(ungroup(trait_try_num_median), sp = AccSpeciesName))
  traits_num = group_by(traits_num, sp, TraitName) %>% 
    summarise(value = mean(value, na.rm = T)) %>% ungroup() %>% 
    mutate(value = ifelse(is.nan(value), NA, value)) %>% 
    spread(TraitName, value) 
  
  
  trait_all = left_join(dplyr::select(trait_BIEN, sp:wood_herb),
                        traits_num)
  
  trait_all = rename(trait_all, leaf_area_mm2 = leaf_area,
                     leaf_carbon_mg_g = leaf_carbon,
                     leaf_nitrogen_mg_g = leaf_nitrogen,
                     plant_height_m = plant_height,
                     root_mass_g = root_mass,
                     seed_mass_mg = seed_mass, 
                     sla_m2_kg = sla)
  write_csv(trait_all, "data_output/trait_all.csv")
  
  trait_all = read_csv("data_output/trait_all.csv")
  sp2 = setdiff(sort(unique(dat_agg$sp)), trait_all$sp)
  
  
  
  sp_traits = sp2 %>%
    purrr::map(function(x){
      BIEN::BIEN_trait_species(x)
    })
  map(sp_traits, dim)
  sp_traits[[1]]
  sp_traits = plyr::ldply(sp_traits)
  sp_traits = as.tibble(sp_traits)
  sort(unique(sp_traits$trait_name))
  
  target_traits = c(
    "flower color", "flower pollination syndrome", "leaf area",
    "leaf area per leaf dry mass", "leaf carbon content per leaf dry mass",
    "leaf life span", "leaf nitrogen content per leaf dry mass",
    "leaf phosphorus content per leaf dry mass", "longest whole plant longevity",
    "root dry mass", "seed mass", "whole plant dispersal syndrome",
    "whole plant growth form", "whole plant height",
    "whole plant sexual system", "whole plant vegetative phenology",
    "whole plant woodiness"
  )
  sp_traits2 = filter(sp_traits, trait_name %in% target_traits)
  sp_traits2 = left_join(sp_traits2,
                         data_frame(trait_name = target_traits,
                                    catg = c("char", "char", "num", "num", "num",
                                             "num", "num", "num", "num", "num", "num", "char",
                                             "char", "num", "char", "char", "char")))
  
  summ2 = function(x){
    if(all(x$catg == "char")){
      xx = tolower(str_trim(x$trait_value))
      xx = gsub(pattern = "[*]$", "", xx)
      as.data.frame(table(xx))
    } else {
      data.frame(xx = NA, Freq = median(as.numeric(x$trait_value, na.rm = T)))
    }
  }
  sp_traits3 = dplyr::select(sp_traits2, sp = scrubbed_species_binomial, trait_name, trait_value, unit, catg) %>%
    group_by(sp, trait_name) %>%
    do(summ2(.))
  
  sp_trait_catg = dplyr::filter(sp_traits3, !is.na(xx))
  sp_trait_num = dplyr::filter(sp_traits3, is.na(xx))
  
  group_by(sp_trait_catg, sp, trait_name) %>% tally() %>% filter(n > 1) %>%
    filter(trait_name == "whole plant growth form") %>% pull(sp)
  dplyr::select(-n) %>% pull(trait_name)
  unique(filter(sp_traits2, trait_name == "whole plant growth form")$trait_value) %>% tolower() %>% unique() %>% sort()
  filter(sp_trait_catg, sp == "Juniperus communis", trait_name == "whole plant growth form")
  filter(sp_trait_catg, sp == "Prosopis glandulosa", trait_name == "whole plant growth form")
  sp_trait_catg2 = sp_trait_catg %>% group_by(sp, trait_name) %>%
    arrange(desc(Freq)) %>% slice(1)
  group_by(sp_trait_catg2, sp, trait_name) %>% tally() %>% arrange(desc(n))
  
  dplyr::select(sp_trait_catg2, -Freq) %>% rename(value = xx) %>%
    spread(trait_name, value)
  
  trait_bien = left_join(
    dplyr::select(sp_trait_catg2, -Freq) %>% rename(value = xx)%>%
      spread(trait_name, value),
    rename(sp_trait_num, value = Freq) %>% dplyr::select(-xx)%>%
      spread(trait_name, value))
  trait_bien = dplyr::select(trait_bien, sp, 
                             flower_color = `flower color`, 
                             growth_form = `whole plant growth form`,
                             ever_deciduous = `whole plant vegetative phenology`,
                             wood_herb = `whole plant woodiness`, 
                             leaf_area = `leaf area`, 
                             sla = `leaf area per leaf dry mass`,
                             leaf_nitrogen = `leaf nitrogen content per leaf dry mass`,
                             seed_mass = `seed mass`, 
                             plant_height = `whole plant height`)
  
  write_csv(trait_bien, path = "data_output/trait_bien.csv")
  trait_bien = read_csv("data_output/trait_bien.csv")
  trait_bien = rename(trait_bien, leaf_area_mm2 = leaf_area,
                      leaf_nitrogen_mg_g = leaf_nitrogen,
                      plant_height_m = plant_height,
                      seed_mass_mg = seed_mass, 
                      sla_m2_kg = sla)
  trait_all = bind_rows(trait_all, trait_bien)
  
  map_dbl(trait_all, ~sum(is.na(.)))
  
}


