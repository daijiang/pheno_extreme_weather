xfun::pkg_attach2(c("tidyverse", "lme4", "lmerTest", "sjPlot", "knitr", "kableExtra", "phyr", "ape"))
d2 = readRDS("data_output/d_for_stat.rds")
load("data_output/models.RData")
load("data_output/models_insects.RData")
load("data_output/models_plants.RData")

# # phylogenies
# 
# # combine insects and plants into one tree?? 
# ## Decided not to do so given the uncertainy in combining them together
# 
# # all(unique(model.frame(m_onset_insect_final)$sp) %in% d2$sp)
# # write.csv(tibble(sp = unique(filter(d2, taxon == "insect")$sp)), "data_output/insect_splist.csv")
# 
# splist_insect = str_replace_all(unique(model.frame(m_onset_insect_final)$sp), " ", "_")
# splist_plant = str_replace_all(unique(model.frame(m_onset_plant_final)$sp), " ", "_")
# 
# # # insect tree
# # tre_insect = read.tree("data_output/MothPhy_wBranches.tre")
# # tre_insect$tip.label = rtrees:::cap_first_letter(tre_insect$tip.label)
# # for(i in sort(unique(rtrees::sp_list_df(setdiff(splist_insect, tre_insect$tip.label))$genus))){
# #   print(grep(i, x = tre_insect$tip.label, ignore.case = T, value = T))
# # }
# # 
# # c("Icaricia", "Syssphinx", "Tharsalea", "Vernia") # genus not in the tree
# # 
# # tre_insect = keep.tip(tre_insect, splist_insect)
# 
# if(!file.exists("data_output/tre_plant.tre")){
#   # splist_plant[splist_plant == "Campanulastrum_americanum"] = "Campanula_americana"
#   splist_plant[splist_plant == "Mycelis_muralis"] = "Lactuca_muralis"
#   tre_plant = rtrees::get_tree(splist_plant, taxon = "plant")
#   tre_plant$tip.label[tre_plant$tip.label == "Campanula_americana"] = "Campanulastrum_americanum"
#   tre_plant$tip.label[tre_plant$tip.label == "Lactuca_muralis"] = "Mycelis_muralis"
#   
#   setdiff(splist_plant, tre_plant$tip.label)
#   setdiff(tre_plant$tip.label, splist_plant)
#   
#   tre_plant$tip.label = str_replace_all(tre_plant$tip.label, "_", " ")
#   write.tree(tre_plant, "data_output/tre_plant.tre")
# } else {
#   tre_plant = read.tree("data_output/tre_plant.tre")
#   tre_plant$tip.label = str_replace_all(tre_plant$tip.label, "_", " ")
# }
# 
# 
# # formula(m_onset_plant_final)
# # m1_p0 = pglmm(onset ~ tmean_5_yr_ave + growth_form + temp_seasonality_5_yr_ave + 
# #                precip_5_yr_ave + precip_seasonality_5_yr_ave + prop_extreme_warm_days_before_onset_logit + 
# #                prop_extreme_wet_days_before_onset_logit + prop_extreme_dry_days_before_onset_logit + 
# #                n_days_log + (1 | sp) + (1 | id_cells) + (1 | yr) + 
# #                (prop_extreme_warm_days_before_onset_logit | sp) + 
# #                (prop_extreme_cold_days_before_onset_logit | sp) + 
# #                (prop_extreme_wet_days_before_onset_logit | sp) + 
# #                (prop_extreme_dry_days_before_onset_logit | sp) + 
# #                (precip_5_yr_ave | sp) + 
# #                (temp_seasonality_5_yr_ave | sp) + 
# #                (precip_seasonality_5_yr_ave | sp) + 
# #                (tmean_5_yr_ave | sp) + 
# #                growth_form:precip_5_yr_ave + growth_form:prop_extreme_warm_days_before_onset_logit + 
# #                growth_form:prop_extreme_dry_days_before_onset_logit + 
# #                growth_form:prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit + 
# #                growth_form:precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit, data = d2_plants, 
# #              cov_ranef = list(sp = tre_plant), bayes = F)
# # saveRDS(m1_p0, "data_output/m_onset_plant_final_pglmm_wo_phy.rds")
# # m1_p0 = readRDS("data_output/m_onset_plant_final_pglmm_wo_phy.rds")
# # summary(m_onset_plant_final)
# # summary(m1_p0)
# 
# job::job(
#   {
#     m1_p = pglmm(onset ~ tmean_5_yr_ave + growth_form + temp_seasonality_5_yr_ave + 
#                    precip_5_yr_ave + precip_seasonality_5_yr_ave + prop_extreme_warm_days_before_onset_logit + 
#                    prop_extreme_wet_days_before_onset_logit + prop_extreme_dry_days_before_onset_logit + 
#                    n_days_log + (1 | sp__) + (1 | id_cells) + (1 | yr) + 
#                    (prop_extreme_warm_days_before_onset_logit | sp) + 
#                    (prop_extreme_cold_days_before_onset_logit | sp) + 
#                    (prop_extreme_wet_days_before_onset_logit | sp) + 
#                    (prop_extreme_dry_days_before_onset_logit | sp) + 
#                    (precip_5_yr_ave | sp) + 
#                    (temp_seasonality_5_yr_ave | sp) + 
#                    (precip_seasonality_5_yr_ave | sp) + 
#                    (tmean_5_yr_ave | sp) + 
#                    growth_form:precip_5_yr_ave + growth_form:prop_extreme_warm_days_before_onset_logit + 
#                    growth_form:prop_extreme_dry_days_before_onset_logit + 
#                    growth_form:prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit + 
#                    growth_form:precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit, data = d2_plants, 
#                  cov_ranef = list(sp = tre_plant), bayes = T)
#     sink("data_output/m_onset_plant_final_pglmm_bayes.txt")
#     summary(m1_p)
#     sink()
#     # saveRDS(m1_p, "data_output/m_onset_plant_final_pglmm_bayes2.rds")
#   }
# )
#   
# 
# 
# 
# # m1_p = readRDS("data_output/m_onset_plant_final_pglmm_bayes.rds")
# 
# # formula(m_offset_plant_final)
# 
# job::job(
#   {
#     m2_p = pglmm(offset ~ tmean_5_yr_ave + growth_form + precip_5_yr_ave + precip_seasonality_5_yr_ave + 
#                    prop_extreme_warm_days_within_duration_logit + prop_extreme_wet_days_within_duration_logit + 
#                    prop_extreme_dry_days_within_duration_logit + n_days_log + 
#                    (1 | sp__) + (1 | id_cells) + (1 | yr) + 
#                    (0 + prop_extreme_warm_days_within_duration_logit | sp) +
#                    (0 + prop_extreme_cold_days_within_duration_logit | sp) + 
#                    (0 + prop_extreme_wet_days_within_duration_logit | sp) + 
#                    (0 + prop_extreme_dry_days_within_duration_logit | sp) + 
#                    (0 + precip_5_yr_ave | sp) + 
#                    (0 + temp_seasonality_5_yr_ave | sp) + 
#                    (0 + precip_seasonality_5_yr_ave | sp) + 
#                    (0 + tmean_5_yr_ave | sp) + 
#                    growth_form:precip_5_yr_ave + growth_form:prop_extreme_warm_days_within_duration_logit + 
#                    growth_form:prop_extreme_wet_days_within_duration_logit + 
#                    growth_form:prop_extreme_dry_days_within_duration_logit + 
#                    growth_form:prop_extreme_warm_days_within_duration_logit:prop_extreme_wet_days_within_duration_logit + 
#                    growth_form:precip_5_yr_ave:prop_extreme_dry_days_within_duration_logit, 
#                  data = d2_plants, 
#                  cov_ranef = list(sp = tre_plant), 
#                  bayes = T)
#     sink("data_output/m_offset_plant_final_pglmm_bayes.txt")
#     summary(m2_p)
#     sink()
#     # saveRDS(m2_p, "data_output/m_offset_plant_final_pglmm_bayes2.rds")
#   }
# )
# 
# job::job(
#   {
#     m3_p = pglmm(duration_log ~ tmean_5_yr_ave + growth_form + temp_seasonality_5_yr_ave + 
#                    precip_5_yr_ave + prop_extreme_warm_days_total_logit + prop_extreme_cold_days_total_logit + 
#                    prop_extreme_wet_days_total_logit + prop_extreme_dry_days_total_logit + 
#                    n_days_log + (1 | sp__) + (1 | id_cells) + (1 | yr) + 
#                    (prop_extreme_warm_days_total_logit | sp) + 
#                    (prop_extreme_cold_days_total_logit | sp) + 
#                    (prop_extreme_wet_days_total_logit | sp) + 
#                    (precip_5_yr_ave | sp) + 
#                    (temp_seasonality_5_yr_ave | sp) + 
#                    (tmean_5_yr_ave | sp) + 
#                    tmean_5_yr_ave:growth_form + 
#                    growth_form:temp_seasonality_5_yr_ave + growth_form:precip_5_yr_ave + 
#                    growth_form:prop_extreme_warm_days_total_logit + growth_form:prop_extreme_cold_days_total_logit + 
#                    growth_form:prop_extreme_wet_days_total_logit + growth_form:prop_extreme_dry_days_total_logit + 
#                    growth_form:precip_5_yr_ave:prop_extreme_wet_days_total_logit + 
#                    growth_form:precip_5_yr_ave:prop_extreme_dry_days_total_logit + 
#                    tmean_5_yr_ave:growth_form:prop_extreme_cold_days_total_logit + 
#                    growth_form:prop_extreme_warm_days_total_logit:prop_extreme_wet_days_total_logit, 
#                  data = d2_plants, 
#                  cov_ranef = list(sp = tre_plant), 
#                  bayes = T)
#     sink("data_output/m_duration_plant_final_pglmm_bayes.txt")
#     summary(m3_p)
#     sink()
#     # saveRDS(m3_p, "data_output/m_duration_plant_final_pglmm_bayes2.rds")
#     
#   }
# )

## insects ----

if(!file.exists("data_output/tre_insect3.tre")) {
  tre_insect = read.tree("data/lepTree_branchLengths_Nov26Update.tre")
  plot(tre_insect)
  # no Plebejus samuelis & Anisota pellucida since those got demoted to subspecies
  
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
  
  ape::write.tree(tre_insect3, "data_output/tre_insect3.tre")
  tre_insect3$tip.label = str_replace_all(tre_insect3$tip.label, "_", " ")
} else {
  tre_insect3 = ape::read.tree("data_output/tre_insect3.tre")
  tre_insect3$tip.label = str_replace_all(tre_insect3$tip.label, "_", " ")
}



d2_insects = readRDS("data_output/d2_insects_final_model.rds")


setdiff(d2_insects$sp, tre_insect3$tip.label)
setdiff(tre_insect3$tip.label, d2_insects$sp)

# formula(m_onset_insect_final)
# m1_p_insect = pglmm(onset ~ tmean_5_yr_ave * overwintering.stage + 
#                       temp_seasonality_5_yr_ave * overwintering.stage + 
#                       precip_5_yr_ave * overwintering.stage +
#                       precip_seasonality_5_yr_ave * overwintering.stage + 
#                       # pop_25km_log10 * overwintering.stage +
#                       prop_extreme_warm_days_before_onset_logit * overwintering.stage + 
#                       prop_extreme_cold_days_before_onset_logit * overwintering.stage + 
#                       prop_extreme_wet_days_before_onset_logit * overwintering.stage + 
#                       prop_extreme_dry_days_before_onset_logit * overwintering.stage + n_days_log +
#                       overwintering.stage:precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit +
#                       overwintering.stage:precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit +
#                       overwintering.stage:prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit +
#                       # overwintering.stage:prop_extreme_warm_days_before_onset_logit:pop_25km_log10 +
#                       overwintering.stage:tmean_5_yr_ave:prop_extreme_warm_days_before_onset_logit +
#                       overwintering.stage:tmean_5_yr_ave:prop_extreme_cold_days_before_onset_logit +
#                (1 | sp__) + (1 | id_cells) + (1 | yr) +
#                (0 + prop_extreme_warm_days_before_onset_logit | sp__) + 
#                (0 + prop_extreme_cold_days_before_onset_logit | sp__) + 
#                (0 + prop_extreme_wet_days_before_onset_logit | sp__) + 
#                (0 + prop_extreme_dry_days_before_onset_logit | sp__) + 
#                (0 + precip_5_yr_ave | sp__) + 
#                (0 + temp_seasonality_5_yr_ave | sp__) + 
#                (0 + precip_seasonality_5_yr_ave | sp__) + 
#                (0 + tmean_5_yr_ave | sp__), 
#              data = d2_insects, 
#              cov_ranef = list(sp = tre_insect3), bayes = T, verbose = F,
                    # s2.init = c(985.70, 110.20, 62.22, 698.10, 5.903, 2.009, 0.0001,
                    #             10.43, 0.0205, 1.12, 60.03, 0.8796, 19.14, 1.111,
                    #             91.41, 0.2682, 121.10, 8.911, 678.80))
# summary(m1_p_insect)
# sink("data_output/m_onset_insect_final_pglmm_bayes2.txt")
# summary(m1_p_insect)
# sink()

# formula(m_offset_insect_final)
# 
# m_0_offset_insect = pglmm(offset ~ tmean_5_yr_ave * overwintering.stage + precip_5_yr_ave * 
#                       overwintering.stage + precip_seasonality_5_yr_ave * overwintering.stage + 
#                       prop_extreme_warm_days_within_duration_logit * overwintering.stage + 
#                       prop_extreme_cold_days_within_duration_logit * overwintering.stage + 
#                       prop_extreme_wet_days_within_duration_logit * overwintering.stage + 
#                       prop_extreme_dry_days_within_duration_logit * overwintering.stage + 
#                       overwintering.stage:prop_extreme_warm_days_within_duration_logit:prop_extreme_wet_days_within_duration_logit + 
#                       overwintering.stage:precip_5_yr_ave:prop_extreme_wet_days_within_duration_logit +
#                       n_days_log + (1 | sp__) + (1 | id_cells) + (1 | yr) +
#                       (0 + prop_extreme_warm_days_within_duration_logit | sp) +
#                       (0 + prop_extreme_cold_days_within_duration_logit | sp) +
#                       (0 + prop_extreme_wet_days_within_duration_logit | sp) +
#                       (0 + prop_extreme_dry_days_within_duration_logit | sp) +
#                       (0 + precip_5_yr_ave | sp) +
#                       (0 + precip_seasonality_5_yr_ave | sp) +
#                       (0 + tmean_5_yr_ave | sp),
#                     data = d2_insects,
#                     cov_ranef = list(sp = tre_insect3), bayes = T,
#                     s2.init = c(918.552, 307.932, 55.435, 2.399, 
#                                 7.921, 23.227, 3.566, 65.222, 
#                                 20.4, 76.508, 149.664))
# sink("data_output/m_offset_insect_final_pglmm_bayes_0.txt")
# summary(m_0_offset_insect)
# sink()
# 
# 
# formula(m_duration_insect_final)
# m_0_dur_insect = pglmm(duration_log ~ tmean_5_yr_ave * overwintering.stage + temp_seasonality_5_yr_ave * 
#                       overwintering.stage + precip_5_yr_ave * overwintering.stage + 
#                       precip_seasonality_5_yr_ave * overwintering.stage + prop_extreme_warm_days_total_logit * 
#                       overwintering.stage + prop_extreme_cold_days_total_logit * 
#                       overwintering.stage + prop_extreme_wet_days_total_logit * 
#                       overwintering.stage + prop_extreme_dry_days_total_logit * 
#                       overwintering.stage + overwintering.stage:tmean_5_yr_ave:prop_extreme_warm_days_total_logit + 
#                       overwintering.stage:tmean_5_yr_ave:prop_extreme_cold_days_total_logit + n_days_log +
#                       (1 | sp__) + (1 | id_cells) + (1 | yr) +
#                       (0 + prop_extreme_warm_days_total_logit | sp) +
#                       (0 + prop_extreme_cold_days_total_logit | sp) +
#                       (0 + prop_extreme_wet_days_total_logit | sp) +
#                       (0 + prop_extreme_dry_days_total_logit | sp) +
#                       (0 + precip_5_yr_ave | sp) +
#                       (0 + temp_seasonality_5_yr_ave | sp) +
#                       (0 + precip_seasonality_5_yr_ave | sp) +
#                       (0 + tmean_5_yr_ave | sp),
#                     data = d2_insects,
#                     cov_ranef = list(sp = tre_insect3), bayes = T,
#                     s2.init = c(
#                       0.2162020,  0.0039824, 0.0055650, 0.0004488, 
#                       0.0009635, 0.0031692, 0.0007673, 0.0124347, 
#                       0.0044469, 0.0091631, 0.0143060, 0.0248923
#                     ))
# sink("data_output/m_duration_insect_final_pglmm_bayes_0.txt")
# summary(m_0_dur_insect)
# sink()




m2_p_insect = pglmm(offset ~ tmean_5_yr_ave * overwintering.stage + precip_5_yr_ave * 
                      overwintering.stage + precip_seasonality_5_yr_ave * overwintering.stage + 
                      prop_extreme_warm_days_within_duration_logit * overwintering.stage + 
                      prop_extreme_cold_days_within_duration_logit * overwintering.stage + 
                      prop_extreme_wet_days_within_duration_logit * overwintering.stage + 
                      prop_extreme_dry_days_within_duration_logit * overwintering.stage + 
                      overwintering.stage:prop_extreme_warm_days_within_duration_logit:prop_extreme_wet_days_within_duration_logit + 
                      overwintering.stage:precip_5_yr_ave:prop_extreme_wet_days_within_duration_logit +
                      n_days_log + (1 | sp__) + (1 | id_cells) + (1 | yr) +
                      (0 + prop_extreme_warm_days_within_duration_logit | sp__) +
                      (0 + prop_extreme_cold_days_within_duration_logit | sp__) +
                      (0 + prop_extreme_wet_days_within_duration_logit | sp__) +
                      (0 + prop_extreme_dry_days_within_duration_logit | sp__) +
                      (0 + precip_5_yr_ave | sp__) +
                      (0 + precip_seasonality_5_yr_ave | sp__) +
                      (0 + tmean_5_yr_ave | sp__),
                    data = d2_insects,
                    cov_ranef = list(sp = tre_insect3), bayes = T,
                    s2.init = c(985.70, 110.20, 62.22, 698.10, 5.903, 2.009, 0.0001,
                                10.43, 0.0205, 1.12, 60.03, 0.8796, 19.14, 1.111,
                                91.41, 0.2682, 121.10, 8.911))
sink("data_output/m_offset_insect_final_pglmm_bayes2.txt")
summary(m2_p_insect)
sink()


formula(m_duration_insect_final)
m3_p_insect = pglmm(duration_log ~ tmean_5_yr_ave * overwintering.stage + temp_seasonality_5_yr_ave * 
                      overwintering.stage + precip_5_yr_ave * overwintering.stage + 
                      precip_seasonality_5_yr_ave * overwintering.stage + prop_extreme_warm_days_total_logit * 
                      overwintering.stage + prop_extreme_cold_days_total_logit * 
                      overwintering.stage + prop_extreme_wet_days_total_logit * 
                      overwintering.stage + prop_extreme_dry_days_total_logit * 
                      overwintering.stage + overwintering.stage:tmean_5_yr_ave:prop_extreme_warm_days_total_logit + 
                      overwintering.stage:tmean_5_yr_ave:prop_extreme_cold_days_total_logit + n_days_log +
                      (1 | sp__) + (1 | id_cells) + (1 | yr) +
                      (0 + prop_extreme_warm_days_total_logit | sp__) +
                      (0 + prop_extreme_cold_days_total_logit | sp__) +
                      (0 + prop_extreme_wet_days_total_logit | sp__) +
                      (0 + prop_extreme_dry_days_total_logit | sp__) +
                      (0 + precip_5_yr_ave | sp__) +
                      (0 + temp_seasonality_5_yr_ave | sp__) +
                      (0 + precip_seasonality_5_yr_ave | sp__) +
                      (0 + tmean_5_yr_ave | sp__),
                    data = d2_insects,
                    cov_ranef = list(sp = tre_insect3), bayes = T,
                    s2.init = c(0.23080, 0.00004, 0.00563, 0.00046, 0.00033, 0.00005, 0.00270, 0.00003, 0.00057, 
                      0.00001, 0.01250, 0.00002, 0.00416, 0.00003, 0.00894, 0.00003, 0.00002, 0.00631,
                      0.02361, 0.00001))
sink("data_output/m_duration_insect_final_pglmm_bayes2.txt")
summary(m3_p_insect)
sink()
