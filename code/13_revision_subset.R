source("code/08_data_exploratory_analysis.R")
d2_subset = readRDS("data_output/d_subset_for_stat.rds")

load("data_output/models.RData")
formula_all_onset = formula(m_onset_final)
formula_all_offset = formula(m_offset_final)
formula_all_duration = formula(m_duration_final)


# subset species ----
m_onset_0 <- lmer(onset ~ tmean_5_yr_ave * taxon + 
                    temp_seasonality_5_yr_ave * taxon +
                    precip_5_yr_ave * taxon  +
                    precip_seasonality_5_yr_ave * taxon +
                    n_days_log +  # to count for collection effort
                    (1|sp) + (1 | id_cells) + (1 | yr) +
                    (0 + precip_5_yr_ave | sp) +
                    (0 + temp_seasonality_5_yr_ave | sp) +
                    (0 + precip_seasonality_5_yr_ave | sp) +
                    (0 + tmean_5_yr_ave | sp),
                  data = d2_subset, na.action = "na.fail", REML = F)

m_offset_0 <- lmer(offset ~ tmean_5_yr_ave * taxon + 
                     temp_seasonality_5_yr_ave * taxon +
                     precip_5_yr_ave * taxon  +
                     precip_seasonality_5_yr_ave * taxon +
                     n_days_log +  # to count for collection effort
                     (1|sp) + (1 | id_cells) + (1 | yr) +
                     (0 + precip_5_yr_ave | sp) +
                     (0 + temp_seasonality_5_yr_ave | sp) +
                     (0 + precip_seasonality_5_yr_ave | sp) +
                     (0 + tmean_5_yr_ave | sp),
                   data = d2_subset, na.action = "na.fail", REML = F)


m_duration_0 <- lmer(duration_log ~ tmean_5_yr_ave * taxon + 
                       temp_seasonality_5_yr_ave * taxon +
                       precip_5_yr_ave * taxon  +
                       precip_seasonality_5_yr_ave * taxon +
                       n_days_log +  # to count for collection effort
                       (1|sp) + (1 | id_cells) + (1 | yr) +
                       (0 + precip_5_yr_ave | sp) +
                       (0 + temp_seasonality_5_yr_ave | sp) +
                       (0 + precip_seasonality_5_yr_ave | sp) +
                       (0 + tmean_5_yr_ave | sp),
                     data = d2_subset, na.action = "na.fail", REML = F)


# same random effects as m_onset_0 with nested fixed effects fitted with ML, so that models are comparable.
m_onset_1 <- lmer(onset ~ tmean_5_yr_ave * taxon + 
                    temp_seasonality_5_yr_ave * taxon +
                    precip_5_yr_ave * taxon  +
                    precip_seasonality_5_yr_ave * taxon +
                    prop_extreme_warm_days_before_onset_logit * taxon +
                    prop_extreme_cold_days_before_onset_logit * taxon +
                    prop_extreme_wet_days_before_onset_logit * taxon +
                    prop_extreme_dry_days_before_onset_logit * taxon +
                    n_days_log +  # to count for collection effort
                    (1|sp) + (1 | id_cells) + (1 | yr) +
                    (0 + precip_5_yr_ave | sp) +
                    (0 + temp_seasonality_5_yr_ave | sp) +
                    (0 + precip_seasonality_5_yr_ave | sp) +
                    (0 + tmean_5_yr_ave | sp),
                  data = d2_subset, na.action = "na.fail", REML = F)

m_offset_1 <- lmer(offset ~ tmean_5_yr_ave * taxon + 
                     temp_seasonality_5_yr_ave * taxon +
                     precip_5_yr_ave * taxon  +
                     precip_seasonality_5_yr_ave * taxon +
                     prop_extreme_warm_days_within_duration_logit * taxon +
                     prop_extreme_cold_days_within_duration_logit * taxon +
                     prop_extreme_wet_days_within_duration_logit * taxon +
                     prop_extreme_dry_days_within_duration_logit * taxon +
                     n_days_log +  # to count for collection effort
                     (1|sp) + (1 | id_cells) + (1 | yr) +
                     (0 + precip_5_yr_ave | sp) +
                     (0 + temp_seasonality_5_yr_ave | sp) +
                     (0 + precip_seasonality_5_yr_ave | sp) +
                     (0 + tmean_5_yr_ave | sp),
                   data = d2_subset, na.action = "na.fail", REML = F)


m_duration_1 <- lmer(duration_log ~ tmean_5_yr_ave * taxon + 
                       temp_seasonality_5_yr_ave * taxon +
                       precip_5_yr_ave * taxon  +
                       precip_seasonality_5_yr_ave * taxon +
                       prop_extreme_warm_days_total_logit * taxon +
                       prop_extreme_cold_days_total_logit * taxon +
                       prop_extreme_wet_days_total_logit * taxon +
                       prop_extreme_dry_days_total_logit * taxon +
                       n_days_log +  # to count for collection effort
                       (1|sp) + (1 | id_cells) + (1 | yr) +
                       (0 + precip_5_yr_ave | sp) +
                       (0 + temp_seasonality_5_yr_ave | sp) +
                       (0 + precip_seasonality_5_yr_ave | sp) +
                       (0 + tmean_5_yr_ave | sp),
                     data = d2_subset, na.action = "na.fail", REML = F)

m_onset_0_subset = m_onset_0
m_offset_0_subset = m_offset_0
m_duration_0_subset = m_duration_0
m_onset_1_subset = m_onset_1
m_offset_1_subset = m_offset_1
m_duration_1_subset = m_duration_1

m_onset_final_subset_same_as_allsp = lmer(formula_all_onset, data = d2_subset, na.action = "na.fail", REML = F)
m_offset_final_subset_same_as_allsp = lmer(formula_all_offset, data = d2_subset, na.action = "na.fail", REML = F)
m_duration_final_subset_same_as_allsp = lmer(formula_all_duration, data = d2_subset, na.action = "na.fail", REML = F)

m_onset_final_2_subset_same_as_allsp = update(m_onset_final_subset_same_as_allsp, data = mutate(d2_subset, taxon = factor(taxon, levels = c("plant", "insect"))))
m_offset_final_2_subset_same_as_allsp = update(m_offset_final_subset_same_as_allsp, data = mutate(d2_subset, taxon = factor(taxon, levels = c("plant", "insect"))))
m_duration_final_2_subset_same_as_allsp = update(m_duration_final_subset_same_as_allsp, data = mutate(d2_subset, taxon = factor(taxon, levels = c("plant", "insect"))))


MuMIn::Weights(AIC(m_onset_final_subset_same_as_allsp, m_onset_1, m_onset_0))
MuMIn::Weights(AIC(m_offset_final_subset_same_as_allsp, m_offset_1, m_offset_0))
MuMIn::Weights(AIC(m_duration_final_subset_same_as_allsp, m_duration_1, m_duration_0))

c(m_onset_final_subset_same_as_allsp, m_onset_1, m_onset_0) |> purrr::map(MuMIn::r.squaredGLMM)
c(m_offset_final_subset_same_as_allsp, m_offset_1, m_offset_0) |> purrr::map(MuMIn::r.squaredGLMM)
c(m_duration_final_subset_same_as_allsp, m_duration_1, m_duration_0) |> purrr::map(MuMIn::r.squaredGLMM)

tibble(phenology = c("Onset", "Onset", "Onset", "Offset", "Offset", "Offset", "Duration", "Duration", "Duration"),
       models = rep(c("traiditional variables only", "with extreme weathers", "final model with extreme weathers and interactions"), 3),
       AIC = AIC(m_onset_0, m_onset_1, m_onset_final_subset_same_as_allsp, m_offset_0, m_offset_1, m_offset_final_subset_same_as_allsp,
                 m_duration_0, m_duration_1, m_duration_final_subset_same_as_allsp)$AIC) |> 
  group_by(phenology) |> 
  mutate(delta_AIC = AIC - min(AIC)) |> 
  ungroup() |> 
  mutate(model_weight = rep(c(0, 0, 1), 3))


save(m_onset_0_subset, m_offset_0_subset, m_duration_0_subset, # models without extreme weather
     m_onset_1_subset, m_offset_1_subset, m_duration_1_subset, # models with extreme weather
     # m_onset_1_2_subset, m_offset_1_2_subset, m_duration_1_2_subset, # models with plants as base
     m_onset_final_subset_same_as_allsp, m_offset_final_subset_same_as_allsp,
     m_duration_final_subset_same_as_allsp, m_onset_final_2_subset_same_as_allsp,
     m_offset_final_2_subset_same_as_allsp, m_duration_final_2_subset_same_as_allsp,
     file = "data_output/models_subset.RData")

