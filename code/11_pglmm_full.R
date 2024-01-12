xfun::pkg_attach2(c("tidyverse", "lme4", "lmerTest", "sjPlot", "knitr", "kableExtra", "phyr", "ape"))
d2 = readRDS("data_output/d_for_stat.rds")

# group_by(d2, taxon) |> 
#   summarise(nsp = n_distinct(sp))
# summary(m_onset_final)

if(!file.exists("data_output/tre_insect3_all.tre")){
  tre_insect = read.tree("data/lepTree_branchLengths_Nov26Update.tre")
  plot(tre_insect)
  # no Plebejus samuelis & Anisota pellucida since those got demoted to subspecies
  
  splist_insect = str_replace_all(unique(filter(d2, taxon == "insect")$sp), " ", "_")
  tre_insect$tip.label = rtrees:::cap_first_letter(tre_insect$tip.label)
  setdiff(splist_insect, tre_insect$tip.label)
  
  sp_insect_tre = read_csv("data/spList_phyloNames.csv")
  sp_insect_tre = mutate(sp_insect_tre, sp = str_replace_all(sp, " ", "_"), 
                         phyloName = str_replace_all(phyloName, " ", "_"))
  setdiff(splist_insect, sp_insect_tre$sp) # 2 species not there
  grep("Anisota", tre_insect$tip.label, value = T)
  grep("Plebejus", tre_insect$tip.label, value = T)
  
  sp_insect_tre = filter(sp_insect_tre, sp %in% splist_insect)
  
  tre_insect$tip.label = str_replace_all(tre_insect$tip.label, "Aricia", "Icaricia")
  tre_insect$tip.label = str_replace_all(tre_insect$tip.label, "Enodia", "Lethe")
  tre_insect$tip.label = str_replace_all(tre_insect$tip.label, "Sphingicampa", "Syssphinx")
  tre_insect$tip.label = str_replace_all(tre_insect$tip.label, "Speyeria", "Argynnis")
  tre_insect$tip.label[tre_insect$tip.label == "Deidamia_inscripta"] = "Deidamia_inscriptum"
  tre_insect$tip.label[tre_insect$tip.label == "Lycaena_arota"] = "Tharsalea_arota"
  tre_insect$tip.label[tre_insect$tip.label == "Cecropterus_pylades"] = "Thorybes_pylades"
  tre_insect$tip.label[tre_insect$tip.label == "Incisalia_eryphon"] = "Callophrys_eryphon"
  tre_insect$tip.label[tre_insect$tip.label == "Satyrodes_eurydice"] = "Lethe_eurydice"
  tre_insect$tip.label[tre_insect$tip.label == "Argynnis_aphrodite"] = "Speyeria_aphrodite"
  tre_insect$tip.label[tre_insect$tip.label == "Argynnis_hesperis"] = "Speyeria_hesperis"
  sp_insect_tre$phyloName = str_replace_all(sp_insect_tre$phyloName, "Argynnis", "Speyeria")
  
  sort(setdiff(tre_insect$tip.label, sp_insect_tre$phyloName))
  sort(setdiff(sp_insect_tre$phyloName, tre_insect$tip.label)) |> 
    str_extract("^[^_]+") |> unique() |> setdiff(str_extract(tre_insect$tip.label, "^[^_]+"))
  
  tre_insect2 = rtrees::get_tree(sp_list = sp_insect_tre$phyloName, tree = tre_insect, tree_by_user = T)
  n_distinct(sp_insect_tre$sp)
  n_distinct(sp_insect_tre$phyloName)
  sp_insect_tre2 = sp_insect_tre[-which(duplicated(sp_insect_tre$phyloName)),]
  
  tipnames = tibble(phyloName = tre_insect2$tip.label) |> 
    left_join(sp_insect_tre2) 
  tre_insect2$tip.label = tipnames$sp
  
  setdiff(splist_insect, sp_insect_tre2$sp)
  tre_insect3 = rtrees::get_tree(splist_insect, tree = tre_insect2, tree_by_user = T)
  
  tre_insect3$graft_status
  
  setdiff(splist_insect, tre_insect3$tip.label)
  setdiff(tre_insect3$tip.label, splist_insect)
  
  ape::write.tree(tre_insect3, "data_output/tre_insect3_all.tre")
  tre_insect3$tip.label = str_replace_all(tre_insect3$tip.label, "_", " ")
} else {
  tre_insect3 = ape::read.tree("data_output/tre_insect3_all.tre")
  tre_insect3$tip.label = str_replace_all(tre_insect3$tip.label, "_", " ")
}

tre_plant = read.tree("data_output/tre_plant.tre")
tre_plant$tip.label = str_replace_all(tre_plant$tip.label, "_", " ")

# setdiff(unique(filter(d2, taxon == "plant")$sp), tre_plant$tip.label)

# summary(m_onset_final)

d2_insect = filter(d2, taxon == "insect")
d2_plant = filter(d2, taxon == "plant")

# m_plant_onset = lmer(onset ~ tmean_5_yr_ave + temp_seasonality_5_yr_ave +  precip_5_yr_ave +
#                        precip_seasonality_5_yr_ave + 
#                        prop_extreme_warm_days_before_onset_logit +
#                        prop_extreme_cold_days_before_onset_logit +
#                        prop_extreme_wet_days_before_onset_logit +
#                        prop_extreme_dry_days_before_onset_logit + n_days_log + 
#                        tmean_5_yr_ave:prop_extreme_warm_days_before_onset_logit + 
#                        tmean_5_yr_ave:prop_extreme_cold_days_before_onset_logit +  
#                        precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit + 
#                        precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit +  
#                        prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit +
#                        (1 | sp) + (1 | id_cells) + (1 | yr) +
#                        (0 + prop_extreme_warm_days_before_onset_logit | sp) + 
#                        (0 + prop_extreme_cold_days_before_onset_logit | sp) + 
#                        (0 + prop_extreme_wet_days_before_onset_logit | sp) + 
#                        (0 +  prop_extreme_dry_days_before_onset_logit | sp) + 
#                        (0 + precip_5_yr_ave | sp) + (0 + temp_seasonality_5_yr_ave | sp) + 
#                        (0 + precip_seasonality_5_yr_ave | sp) + (0 + tmean_5_yr_ave | sp),
#                      data = d2_plant)
# 
# summary(m_plant_onset)
# 
# m_insect_onset = lmer(onset ~ tmean_5_yr_ave + temp_seasonality_5_yr_ave +  precip_5_yr_ave +
#                        precip_seasonality_5_yr_ave + 
#                        prop_extreme_warm_days_before_onset_logit +
#                        prop_extreme_cold_days_before_onset_logit +
#                        prop_extreme_wet_days_before_onset_logit +
#                        prop_extreme_dry_days_before_onset_logit + n_days_log + 
#                        tmean_5_yr_ave:prop_extreme_warm_days_before_onset_logit + 
#                        tmean_5_yr_ave:prop_extreme_cold_days_before_onset_logit +  
#                        precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit + 
#                        precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit +  
#                        prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit +
#                        (1 | sp) + (1 | id_cells) + (1 | yr) +
#                        (0 + prop_extreme_warm_days_before_onset_logit | sp) + 
#                        (0 + prop_extreme_cold_days_before_onset_logit | sp) + 
#                        (0 + prop_extreme_wet_days_before_onset_logit | sp) + 
#                        (0 +  prop_extreme_dry_days_before_onset_logit | sp) + 
#                        (0 + precip_5_yr_ave | sp) + (0 + temp_seasonality_5_yr_ave | sp) + 
#                        (0 + precip_seasonality_5_yr_ave | sp) + (0 + tmean_5_yr_ave | sp),
#                      data = d2_insect)



m_insect_offset_pglmm = phyr::pglmm(offset ~ tmean_5_yr_ave +  precip_5_yr_ave +
                                     precip_seasonality_5_yr_ave + 
                                     prop_extreme_warm_days_within_duration_logit +
                                     prop_extreme_cold_days_within_duration_logit +
                                     prop_extreme_wet_days_within_duration_logit +
                                     prop_extreme_dry_days_within_duration_logit + n_days_log + 
                                      tmean_5_yr_ave:prop_extreme_cold_days_within_duration_logit +     
                                      precip_5_yr_ave:prop_extreme_wet_days_within_duration_logit +  
                                      prop_extreme_warm_days_within_duration_logit:prop_extreme_dry_days_within_duration_logit +  
                                      prop_extreme_warm_days_within_duration_logit:prop_extreme_wet_days_within_duration_logit +
                                     (1 | sp__) + (1 | id_cells) + (1 | yr) +
                                     (0 + prop_extreme_warm_days_within_duration_logit | sp__) + 
                                     (0 + prop_extreme_cold_days_within_duration_logit | sp__) + 
                                     (0 + prop_extreme_wet_days_within_duration_logit | sp__) + 
                                     (0 +  prop_extreme_dry_days_within_duration_logit | sp__) + 
                                     (0 + precip_5_yr_ave | sp__) + 
                                    # (0 + temp_seasonality_5_yr_ave | sp__) + 
                                     (0 + precip_seasonality_5_yr_ave | sp__) + (0 + tmean_5_yr_ave | sp__),
                                   data = d2_insect, cov_ranef = list(sp = tre_insect3), bayes = T,
                                   s2.init = c(985.70, 110.20, 62.22, 698.10, 
                                               5.903, 2.009, 0.0001, 10.43, 
                                               0.0205, 1.12, 60.03, 0.8796, 
                                               19.14, 1.111, 91.41, 0.2682, 
                                               121.10, 8.911))
sink("data_output/m_offset_insect_pglmm_bayes.txt")
summary(m_insect_offset_pglmm)
sink()

m_insect_duration_pglmm = phyr::pglmm(duration_log ~ tmean_5_yr_ave + temp_seasonality_5_yr_ave +
                                        precip_5_yr_ave +
                                      prop_extreme_warm_days_total_logit +
                                      prop_extreme_cold_days_total_logit +
                                      prop_extreme_wet_days_total_logit +
                                      prop_extreme_dry_days_total_logit + n_days_log + 
                                        tmean_5_yr_ave:prop_extreme_warm_days_total_logit +  
                                        tmean_5_yr_ave:prop_extreme_cold_days_total_logit + 
                                        precip_5_yr_ave:prop_extreme_wet_days_total_logit +  
                                        precip_5_yr_ave:prop_extreme_dry_days_total_logit +
                                      (1 | sp__) + (1 | id_cells) + (1 | yr) +
                                      (0 + prop_extreme_warm_days_total_logit | sp__) + 
                                      (0 + prop_extreme_cold_days_total_logit | sp__) + 
                                      (0 + prop_extreme_wet_days_total_logit | sp__) + 
                                      (0 + prop_extreme_dry_days_total_logit | sp__) + 
                                      (0 + precip_5_yr_ave | sp__) + 
                                      (0 + temp_seasonality_5_yr_ave | sp__) +
                                      (0 + tmean_5_yr_ave | sp__),
                                    data = d2_insect, cov_ranef = list(sp = tre_insect3), bayes = T,
                                    s2.init = c(0.23080, 0.00004, 0.00563, 0.00046,
                                                0.00033, 0.00005, 0.00270, 0.00003, 
                                                0.00057, 0.00001, 0.01250, 0.00002, 
                                                0.00416, 0.00003, 0.00894, 0.00003, 
                                                0.02361, 0.00001))
sink("data_output/m_duration_insect_pglmm_bayes.txt")
summary(m_insect_duration_pglmm)
sink()



m_insect_onset_pglmm = phyr::pglmm(onset ~ tmean_5_yr_ave + temp_seasonality_5_yr_ave +  precip_5_yr_ave +
                                     precip_seasonality_5_yr_ave + 
                                     prop_extreme_warm_days_before_onset_logit +
                                     prop_extreme_cold_days_before_onset_logit +
                                     prop_extreme_wet_days_before_onset_logit +
                                     prop_extreme_dry_days_before_onset_logit + n_days_log + 
                                     tmean_5_yr_ave:prop_extreme_warm_days_before_onset_logit + 
                                     tmean_5_yr_ave:prop_extreme_cold_days_before_onset_logit +  
                                     precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit + 
                                     precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit +  
                                     prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit +
                                     (1 | sp__) + (1 | id_cells) + (1 | yr) +
                                     (0 + prop_extreme_warm_days_before_onset_logit | sp__) + 
                                     (0 + prop_extreme_cold_days_before_onset_logit | sp__) + 
                                     (0 + prop_extreme_wet_days_before_onset_logit | sp__) + 
                                     (0 +  prop_extreme_dry_days_before_onset_logit | sp__) + 
                                     (0 + precip_5_yr_ave | sp__) + (0 + temp_seasonality_5_yr_ave | sp__) + 
                                     (0 + precip_seasonality_5_yr_ave | sp__) + (0 + tmean_5_yr_ave | sp__),
                                   data = d2_insect, cov_ranef = list(sp = tre_insect3), bayes = T,
                                   s2.init = c(985.70, 110.20, 62.22, 698.10, 5.903, 2.009, 0.0001,
                                               10.43, 0.0205, 1.12, 60.03, 0.8796, 19.14, 1.111,
                                               91.41, 0.2682, 121.10, 8.911))
sink("data_output/m_onset_insect_pglmm_bayes.txt")
summary(m_insect_onset_pglmm)
sink()

# plants ----
m_plant_onset_pglmm = phyr::pglmm(onset ~ tmean_5_yr_ave + temp_seasonality_5_yr_ave +  precip_5_yr_ave +
                       precip_seasonality_5_yr_ave + 
                       prop_extreme_warm_days_before_onset_logit +
                       prop_extreme_cold_days_before_onset_logit +
                       prop_extreme_wet_days_before_onset_logit +
                       prop_extreme_dry_days_before_onset_logit + n_days_log + 
                       tmean_5_yr_ave:prop_extreme_warm_days_before_onset_logit + 
                       tmean_5_yr_ave:prop_extreme_cold_days_before_onset_logit +  
                       precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit + 
                       precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit +  
                       prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit +
                       (1 | sp__) + (1 | id_cells) + (1 | yr) +
                       (0 + prop_extreme_warm_days_before_onset_logit | sp__) + 
                       (0 + prop_extreme_cold_days_before_onset_logit | sp__) + 
                       (0 + prop_extreme_wet_days_before_onset_logit | sp__) + 
                       (0 + prop_extreme_dry_days_before_onset_logit | sp__) + 
                       (0 + precip_5_yr_ave | sp__) + (0 + temp_seasonality_5_yr_ave | sp__) + 
                       (0 + precip_seasonality_5_yr_ave | sp__) + (0 + tmean_5_yr_ave | sp__),
                     data = d2_plant, cov_ranef = list(sp = tre_plant), bayes = T)

sink("data_output/m_onset_plant_pglmm_bayes.txt")
summary(m_plant_onset_pglmm)
sink()

m_plant_offset_pglmm = phyr::pglmm(offset ~ tmean_5_yr_ave +  precip_5_yr_ave +
                                      precip_seasonality_5_yr_ave + 
                                      prop_extreme_warm_days_within_duration_logit +
                                      prop_extreme_cold_days_within_duration_logit +
                                      prop_extreme_wet_days_within_duration_logit +
                                      prop_extreme_dry_days_within_duration_logit + n_days_log + 
                                      tmean_5_yr_ave:prop_extreme_cold_days_within_duration_logit +     
                                      precip_5_yr_ave:prop_extreme_wet_days_within_duration_logit +  
                                      prop_extreme_warm_days_within_duration_logit:prop_extreme_dry_days_within_duration_logit +  
                                      prop_extreme_warm_days_within_duration_logit:prop_extreme_wet_days_within_duration_logit +
                                      (1 | sp__) + (1 | id_cells) + (1 | yr) +
                                      (0 + prop_extreme_warm_days_within_duration_logit | sp__) + 
                                      (0 + prop_extreme_cold_days_within_duration_logit | sp__) + 
                                      (0 + prop_extreme_wet_days_within_duration_logit | sp__) + 
                                      (0 +  prop_extreme_dry_days_within_duration_logit | sp__) + 
                                      (0 + precip_5_yr_ave | sp__) + 
                                      # (0 + temp_seasonality_5_yr_ave | sp__) + 
                                      (0 + precip_seasonality_5_yr_ave | sp__) + (0 + tmean_5_yr_ave | sp__),
                                    data = d2_plant, cov_ranef = list(sp = tre_plant), bayes = T,
                                    s2.init = c(985.70, 110.20, 62.22, 698.10, 
                                                5.903, 2.009, 0.0001, 10.43, 
                                                0.0205, 1.12, 60.03, 0.8796, 
                                                19.14, 1.111, 91.41, 0.2682, 
                                                121.10, 8.911))
sink("data_output/m_offset_plant_pglmm_bayes.txt")
summary(m_plant_offset_pglmm)
sink()

m_plant_duration_pglmm = phyr::pglmm(duration_log ~ tmean_5_yr_ave +  temp_seasonality_5_yr_ave +
                                        precip_5_yr_ave +
                                        # precip_seasonality_5_yr_ave + 
                                        prop_extreme_warm_days_total_logit +
                                        prop_extreme_cold_days_total_logit +
                                        prop_extreme_wet_days_total_logit +
                                        prop_extreme_dry_days_total_logit + n_days_log + 
                                        tmean_5_yr_ave:prop_extreme_warm_days_total_logit +  
                                        tmean_5_yr_ave:prop_extreme_cold_days_total_logit + 
                                        precip_5_yr_ave:prop_extreme_wet_days_total_logit +  
                                        precip_5_yr_ave:prop_extreme_dry_days_total_logit +
                                        (1 | sp__) + (1 | id_cells) + (1 | yr) +
                                        (0 + prop_extreme_warm_days_total_logit | sp__) + 
                                        (0 + prop_extreme_cold_days_total_logit | sp__) + 
                                        (0 + prop_extreme_wet_days_total_logit | sp__) + 
                                        (0 + prop_extreme_dry_days_total_logit | sp__) + 
                                        (0 + precip_5_yr_ave | sp__) + 
                                        (0 + temp_seasonality_5_yr_ave | sp__) +
                                        # (0 + precip_seasonality_5_yr_ave | sp__) +
                                        (0 + tmean_5_yr_ave | sp__),
                                      data = d2_plant, cov_ranef = list(sp = tre_plant), bayes = T,
                                      s2.init = c(0.23080, 0.00004, 0.00563, 0.00046,
                                                  0.00033, 0.00005, 0.00270, 0.00003, 
                                                  0.00057, 0.00001, 0.01250, 0.00002, 
                                                  0.00416, 0.00003, 0.00894, 0.00003, 
                                                  # 0.00002, 0.00631,
                                                  0.02361, 0.00001))
sink("data_output/m_duration_plant_pglmm_bayes.txt")
summary(m_plant_duration_pglmm)
sink()
