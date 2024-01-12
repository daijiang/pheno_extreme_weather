source("code/08_data_exploratory_analysis.R")

names(d2)
d2 |> group_by(taxon) |> summarise(nsp = n_distinct(sp))
n_distinct(d2$id_cells)

# no extreme weather model ----

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
                data = d2, na.action = "na.fail", REML = F)
summary(m_onset_0)
AIC(m_onset_0)

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
                  data = d2, na.action = "na.fail", REML = F)


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
                   data = d2, na.action = "na.fail", REML = F)

# with extreme weather model, but no interactions ----

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
                  data = d2, na.action = "na.fail", REML = F)
AIC(m_onset_1)

m_onset_1_2 = update(m_onset_1, data = mutate(d2, taxon = factor(taxon, levels = c("plant", "insect"))))
summary(m_onset_1)
summary(m_onset_1_2)

# plot_model(m_onset_1, type = "pred", terms = c("tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_onset_1, type = "pred", terms = c("temp_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_onset_1, type = "pred", terms = c("precip_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_onset_1, type = "pred", terms = c("precip_seasonality_5_yr_ave", "taxon"), show.data = F)
# 
# 
# plot_model(m_onset_1, type = "pred", terms = c("prop_extreme_warm_days_before_onset_logit", "taxon"), show.data = F)
# plot_model(m_onset_1, type = "pred", terms = c("prop_extreme_cold_days_before_onset_logit", "taxon"), show.data = F)
# plot_model(m_onset_1, type = "pred", terms = c("prop_extreme_wet_days_before_onset_logit", "taxon"), show.data = F)
# plot_model(m_onset_1, type = "pred", terms = c("prop_extreme_dry_days_before_onset_logit", "taxon"), show.data = F)


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
                   data = d2, na.action = "na.fail", REML = F)

m_offset_1_2 = update(m_offset_1, data = mutate(d2, taxon = factor(taxon, levels = c("plant", "insect"))))

# plot_model(m_offset_1, type = "pred", terms = c("tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_1, type = "pred", terms = c("temp_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_1, type = "pred", terms = c("precip_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_1, type = "pred", terms = c("precip_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_1, type = "pred", terms = c("prop_extreme_warm_days_within_duration_logit", "taxon"), show.data = F)
# plot_model(m_offset_1, type = "pred", terms = c("prop_extreme_cold_days_within_duration_logit", "taxon"), show.data = F)
# plot_model(m_offset_1, type = "pred", terms = c("prop_extreme_wet_days_within_duration_logit", "taxon"), show.data = F)
# plot_model(m_offset_1, type = "pred", terms = c("prop_extreme_dry_days_within_duration_logit", "taxon"), show.data = F)
# summary(m_offset_1)
# summary(m_offset_1_2)

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
                     data = d2, na.action = "na.fail", REML = F)

m_duration_1_2 = update(m_duration_1, data = mutate(d2, taxon = factor(taxon, levels = c("plant", "insect"))))
summary(m_duration_1)
summary(m_duration_1_2)

# plot_model(m_duration_1, type = "pred", terms = c("tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_1, type = "pred", terms = c("temp_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_1, type = "pred", terms = c("precip_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_1, type = "pred", terms = c("precip_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_1, type = "pred", terms = c("prop_extreme_warm_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration_1, type = "pred", terms = c("prop_extreme_cold_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration_1, type = "pred", terms = c("prop_extreme_wet_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration_1, type = "pred", terms = c("prop_extreme_dry_days_total_logit", "taxon"), show.data = F)




# onset models ----

# m_onset_plant <- lmer(onset ~ tmean_5_yr_ave + temp_seasonality_5_yr_ave +
#                   precip_5_yr_ave  + precip_seasonality_5_yr_ave +
#                   prop_extreme_warm_days_before_onset_logit  + 
#                   prop_extreme_cold_days_before_onset_logit  +
#                   prop_extreme_wet_days_before_onset_logit  +
#                   prop_extreme_dry_days_before_onset_logit  +
#                   n_days_log +  # to count for collection effort
#                   (1|sp) + (1 | id_cells) + (1 | yr) +
#                   (0 + prop_extreme_warm_days_before_onset_logit | sp) + 
#                   (0 + prop_extreme_cold_days_before_onset_logit | sp) + 
#                   (0 + prop_extreme_wet_days_before_onset_logit | sp) +
#                   (0 + prop_extreme_dry_days_before_onset_logit | sp) + 
#                   (0 + precip_5_yr_ave | sp) +
#                   (0 + temp_seasonality_5_yr_ave | sp) +
#                   (0 + precip_seasonality_5_yr_ave | sp) +
#                   (0 + tmean_5_yr_ave | sp),
#                 data = filter(d2, taxon == "plant"), na.action = "na.fail")
# summary(m_onset_plant)
# 
# m_onset_insect <- lmer(onset ~ tmean_5_yr_ave + 
#                          precip_5_yr_ave  +
#                          prop_extreme_warm_days_before_onset_logit  + 
#                          prop_extreme_cold_days_before_onset_logit  +
#                          prop_extreme_wet_days_before_onset_logit  +
#                          prop_extreme_dry_days_before_onset_logit  +
#                          n_days_log +  # to count for collection effort
#                          (1|sp) + (1 | id_cells) + (1 | yr) +
#                          (0 + prop_extreme_warm_days_before_onset_logit | sp) + 
#                          (0 + prop_extreme_cold_days_before_onset_logit | sp) + 
#                          (0 + prop_extreme_wet_days_before_onset_logit | sp) +
#                          (0 + prop_extreme_dry_days_before_onset_logit | sp) + 
#                          (0 + precip_5_yr_ave | sp) +
#                          (0 + tmean_5_yr_ave | sp),
#                       data = filter(d2, taxon == "insect"), na.action = "na.fail")
# summary(m_onset_insect)
# plot_model(m_onset_insect, type = "pred", terms = c("tmean_5_yr_ave", "pop_25km_log10"), show.data = F)
# # plot_model(m_onset_plant, type = "pred", terms = c("tmean_5_yr_ave", "pop_25km_log10"), show.data = F)
# 
# m_onset_mean <- lmer(onset ~ tmean_5_yr_ave * taxon + 
#                   precip_5_yr_ave * taxon  +
#                   n_days_log +  # to count for collection effort
#                   (1|sp) + (1 | id_cells) + (1 | yr) +
#                   (0 + precip_5_yr_ave | sp) +
#                   (0 + tmean_5_yr_ave | sp),
#                 data = d2, na.action = "na.fail", REML = F)
# summary(m_onset_mean)
# m_onset_mean

m_onset <- lmer(onset ~ tmean_5_yr_ave * taxon + 
                  temp_seasonality_5_yr_ave * taxon +
                    precip_5_yr_ave * taxon +
                    precip_seasonality_5_yr_ave * taxon +
                    prop_extreme_warm_days_before_onset_logit * taxon +
                    prop_extreme_cold_days_before_onset_logit * taxon +
                    prop_extreme_wet_days_before_onset_logit * taxon +
                    prop_extreme_dry_days_before_onset_logit * taxon +
                    prop_extreme_warm_days_before_onset_logit * tmean_5_yr_ave * taxon +
                    prop_extreme_cold_days_before_onset_logit * tmean_5_yr_ave * taxon +
                    prop_extreme_wet_days_before_onset_logit * precip_5_yr_ave * taxon +
                    prop_extreme_dry_days_before_onset_logit * precip_5_yr_ave * taxon +
                    prop_extreme_warm_days_before_onset_logit * prop_extreme_dry_days_before_onset_logit * taxon +
                    prop_extreme_warm_days_before_onset_logit * prop_extreme_wet_days_before_onset_logit * taxon +
                    n_days_log + # to count for collection effort
                    (1 | sp) + (1 | id_cells) + (1 | yr) +
                    # too many parameters if include cor between intercept and slopes
                  (0 + prop_extreme_warm_days_before_onset_logit | sp) + 
                  (0 + prop_extreme_cold_days_before_onset_logit | sp) + 
                  (0 + prop_extreme_wet_days_before_onset_logit | sp) +
                  (0 + prop_extreme_dry_days_before_onset_logit | sp) + 
                  (0 + precip_5_yr_ave | sp) +
                  (0 + temp_seasonality_5_yr_ave | sp) +
                  (0 + precip_seasonality_5_yr_ave | sp) +
                  (0 + tmean_5_yr_ave | sp),
                data = d2, na.action = "na.fail", REML = F)
# summary(m_onset)
# step_onset = step(m_onset)
# 
# m_onset_final = get_model(step_onset)
# m_onset_final = update(m_onset_final, REML = TRUE)
# summary(m_onset_final)

# m_onset2 = lmer(onset ~ tmean_5_yr_ave + taxon + temp_seasonality_5_yr_ave +
#                   precip_5_yr_ave + precip_seasonality_5_yr_ave + 
#                   prop_extreme_warm_days_before_onset_logit +  
#                   prop_extreme_cold_days_before_onset_logit + 
#                   prop_extreme_wet_days_before_onset_logit +  
#                   prop_extreme_dry_days_before_onset_logit +
#                   n_days_log + 
#                   taxon:tmean_5_yr_ave + 
#                   taxon:precip_5_yr_ave + 
#                   taxon:precip_seasonality_5_yr_ave +  
#                   taxon:prop_extreme_cold_days_before_onset_logit +
#                   taxon:prop_extreme_wet_days_before_onset_logit +  
#                   taxon:prop_extreme_dry_days_before_onset_logit + 
#                   precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit +  
#                   precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit + 
#                   taxon:precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit +  
#                   taxon:precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit +
#                   (1 | sp) + (1 | id_cells) + 
#                   (0 + prop_extreme_warm_days_before_onset_logit | sp) + 
#                   (0 + prop_extreme_cold_days_before_onset_logit | sp) + 
#                   (0 +  prop_extreme_wet_days_before_onset_logit | sp) + 
#                   (0 + prop_extreme_dry_days_before_onset_logit |  sp) + 
#                   (0 + precip_5_yr_ave | sp) + (0 + temp_seasonality_5_yr_ave | sp) + 
#                   (0 + precip_seasonality_5_yr_ave | sp) + (0 + tmean_5_yr_ave | sp),
#                 data = d2)
# summary(m_onset2)
# 
# MuMIn::Weights(AIC(m_onset, m_onset_mean))
# MuMIn::r.squaredGLMM(m_onset_mean) 
# MuMIn::r.squaredGLMM(m_onset_final) 



# offset

# offset models ----



m_offset <- lmer(offset ~ tmean_5_yr_ave * taxon + 
                   temp_seasonality_5_yr_ave * taxon +
                   precip_5_yr_ave * taxon  +
                   precip_seasonality_5_yr_ave * taxon +
                   prop_extreme_warm_days_within_duration_logit * taxon  + 
                   prop_extreme_cold_days_within_duration_logit * taxon  +
                   prop_extreme_wet_days_within_duration_logit * taxon  +
                   prop_extreme_dry_days_within_duration_logit * taxon  +
                   prop_extreme_warm_days_within_duration_logit * tmean_5_yr_ave * taxon +
                   prop_extreme_cold_days_within_duration_logit * tmean_5_yr_ave * taxon +
                   prop_extreme_wet_days_within_duration_logit * precip_5_yr_ave * taxon +
                   prop_extreme_dry_days_within_duration_logit * precip_5_yr_ave * taxon +
                   prop_extreme_warm_days_within_duration_logit * prop_extreme_dry_days_within_duration_logit * taxon +
                   prop_extreme_warm_days_within_duration_logit * prop_extreme_wet_days_within_duration_logit * taxon +
                   # prop_extreme_dry_days_within_duration_logit * prop_extreme_wet_days_within_duration_logit * taxon +
                   # prop_extreme_warm_days_within_duration_logit * prop_extreme_cold_days_within_duration_logit * taxon +
                   n_days_log +  # to count for collection effort
                   (1|sp) + (1 | id_cells) + (1 | yr) +
                   (0 + prop_extreme_warm_days_within_duration_logit | sp) + 
                   (0 + prop_extreme_cold_days_within_duration_logit | sp) + 
                   (0 + prop_extreme_wet_days_within_duration_logit | sp) +
                   (0 + prop_extreme_dry_days_within_duration_logit | sp) + 
                   (0 + precip_5_yr_ave | sp) +
                   (0 + temp_seasonality_5_yr_ave | sp) +
                   (0 + precip_seasonality_5_yr_ave | sp) +
                   (0 + tmean_5_yr_ave | sp),
                 data = d2, na.action = "na.fail", REML = F)




# duration models ----



m_duration <- lmer(duration_log ~ tmean_5_yr_ave * taxon + 
                     temp_seasonality_5_yr_ave * taxon +
                     precip_5_yr_ave * taxon  +
                     precip_seasonality_5_yr_ave * taxon +
                     prop_extreme_warm_days_total_logit * taxon  + 
                     prop_extreme_cold_days_total_logit * taxon  +
                     prop_extreme_wet_days_total_logit * taxon  +
                     prop_extreme_dry_days_total_logit * taxon  +
                     prop_extreme_warm_days_total_logit * tmean_5_yr_ave * taxon +
                     prop_extreme_cold_days_total_logit * tmean_5_yr_ave * taxon +
                     prop_extreme_wet_days_total_logit * precip_5_yr_ave * taxon +
                     prop_extreme_dry_days_total_logit * precip_5_yr_ave * taxon +
                     prop_extreme_warm_days_total_logit * prop_extreme_dry_days_total_logit * taxon +
                     prop_extreme_warm_days_total_logit * prop_extreme_wet_days_total_logit * taxon +
                     n_days_log +  # to count for collection effort
                     (1|sp) + (1 | id_cells) + (1 | yr) +
                     (0 + prop_extreme_warm_days_total_logit | sp) + 
                     (0 + prop_extreme_cold_days_total_logit | sp) + 
                     (0 + prop_extreme_wet_days_total_logit | sp) +
                     (0 + prop_extreme_dry_days_total_logit | sp) + 
                     (0 + precip_5_yr_ave | sp) +
                     (0 + temp_seasonality_5_yr_ave | sp) +
                     (0 + precip_seasonality_5_yr_ave | sp) +
                     (0 + tmean_5_yr_ave | sp),
                   data = d2, na.action = "na.fail", REML = F)


step_onset = step(m_onset)
step_offset = step(m_offset)
step_duration = step(m_duration)

# some convergence warning, double checked, and the results are robust

m_onset_final = get_model(step_onset)
m_offset_final = get_model(step_offset)
m_duration_final = get_model(step_duration)


summary(m_onset_final)

anova(m_onset_final, m_onset_1, m_onset_0)
anova(m_offset_final, m_offset_1, m_offset_0)
anova(m_duration_final, m_duration_1, m_duration_0)

MuMIn::Weights(AIC(m_onset_final, m_onset_1, m_onset_0))
MuMIn::Weights(AIC(m_offset_final, m_offset_1, m_offset_0))
MuMIn::Weights(AIC(m_duration_final, m_duration_1, m_duration_0))

c(m_onset_final, m_onset_1, m_onset_0) |> purrr::map(MuMIn::r.squaredGLMM)
c(m_offset_final, m_offset_1, m_offset_0) |> purrr::map(MuMIn::r.squaredGLMM)
c(m_duration_final, m_duration_1, m_duration_0) |> purrr::map(MuMIn::r.squaredGLMM)

tibble(phenology = c("Onset", "Onset", "Onset", "Offset", "Offset", "Offset", "Duration", "Duration", "Duration"),
       models = rep(c("traiditional variables only", "with extreme weathers", "final model with extreme weathers and interactions"), 3),
       AIC = AIC(m_onset_0, m_onset_1, m_onset_final, m_offset_0, m_offset_1, m_offset_final,
                 m_duration_0, m_duration_1, m_duration_final)$AIC) |> 
  group_by(phenology) |> 
  mutate(delta_AIC = AIC - min(AIC)) |> 
  ungroup()


# different baseline to get plants' coef
# m_onset_final = update(m_onset_final, REML= T)
m_onset_final_2 = update(m_onset_final, data = mutate(d2, taxon = factor(taxon, levels = c("plant", "insect"))))

# m_offset_final = update(m_offset_final, REML= T)
m_offset_final_2 = update(m_offset_final, data = mutate(d2, taxon = factor(taxon, levels = c("plant", "insect"))))

# m_duration_final = update(m_duration_final, REML= T)
m_duration_final_2 = update(m_duration_final, data = mutate(d2, taxon = factor(taxon, levels = c("plant", "insect"))))

save(m_onset_0, m_offset_0, m_duration_0, # models without extreme weather
     m_onset_1, m_offset_1, m_duration_1, # models with extreme weather
     m_onset_1_2, m_offset_1_2, m_duration_1_2, # models with plants as base
     m_onset_final, m_offset_final, m_duration_final, 
     m_onset_final_2, m_offset_final_2, m_duration_final_2,  # models with plants as base
     file = "data_output/models.RData")

# summary(m_onset_final_2) 
# 
# summary(m_onset_final) 
# summary(m_offset_final)
# summary(m_duration_final)
# 
# 
# car::vif(m_onset_final) |> unname() |> sort()
# car::vif(m_offset_final) |> unname() |> sort()
# car::vif(m_duration_final) |> unname() |> sort()
# 
# MuMIn::r.squaredGLMM(m_onset_final)
# MuMIn::r.squaredGLMM(m_offset_final)
# MuMIn::r.squaredGLMM(m_duration_final)
# 
# anova(m_duration_final)
# plot_model(m_duration_final, type = "pred", terms = c("tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("precip_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("temp_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("precip_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("pop_25km_log10", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("prop_extreme_warm_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("prop_extreme_cold_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("prop_extreme_wet_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("prop_extreme_dry_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("prop_extreme_wet_days_total_logit", "precip_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("prop_extreme_warm_days_total_logit", "tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("prop_extreme_warm_days_total_logit", "prop_extreme_dry_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("prop_extreme_warm_days_total_logit", "prop_extreme_wet_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration_final, type = "pred", terms = c("prop_extreme_dry_days_total_logit", "prop_extreme_wet_days_total_logit", "taxon"), show.data = F)
# 
# plot_model(m_offset_final, type = "pred", terms = c("tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("precip_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("temp_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("precip_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_warm_days_within_duration_logit", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_cold_days_within_duration_logit", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_wet_days_within_duration_logit", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_dry_days_within_duration_logit", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_wet_days_within_duration_logit", "precip_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_warm_days_within_duration_logit", "tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_warm_days_within_duration_logit", "prop_extreme_dry_days_within_duration_logit", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_warm_days_within_duration_logit", "prop_extreme_wet_days_within_duration_logit", "taxon"), show.data = F)
# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_dry_days_within_duration_logit", "prop_extreme_wet_days_within_duration_logit", "taxon"), show.data = F)
# 
# plot_model(m_onset_final, type = "pred", terms = c("tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_onset_final, type = "pred", terms = c("precip_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_onset_final, type = "pred", terms = c("temp_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_onset_final, type = "pred", terms = c("precip_seasonality_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_warm_days_before_onset_logit", "taxon"), show.data = F)
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_cold_days_before_onset_logit", "taxon"), show.data = F)
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_wet_days_before_onset_logit", "taxon"), show.data = F)
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_dry_days_before_onset_logit", "taxon"), show.data = F)
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_warm_days_before_onset_logit", "tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_warm_days_before_onset_logit", "pop_25km_log10", "taxon"), show.data = F)
# 
# 
# 
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_wet_days_before_onset_logit", "precip_5_yr_ave", "taxon"), show.data = F)
# # sig for insect, not significant for plants
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_dry_days_before_onset_logit", "precip_5_yr_ave", "taxon"), show.data = F)
# # sig for insect, not significant for plants
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_warm_days_before_onset_logit", "prop_extreme_dry_days_before_onset_logit", "taxon"), show.data = F) 
# # sig for insect, not significant for plants
# plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_warm_days_before_onset_logit", "prop_extreme_wet_days_before_onset_logit", "taxon"), show.data = F)
# # sig for insect, not significant for plants
# 
# # plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_dry_days_before_onset_logit", "prop_extreme_wet_days_before_onset_logit", "taxon"), show.data = F)
# 
# mutate(as.data.frame(effects::Effect(focal.predictors = c("tmean_5_yr_ave", "taxon"), mod = m_duration_final, se = T, xlevels = 20)), var = "tmean") |> rename(x = tmean_5_yr_ave)
# 
# 
# 
# 
# # hot & dry?
# 
# 
# 
# # # interaction between teamn and pop is not significant, remove
# m_duration = update(m_duration, . ~ . - tmean_5_yr_ave : pop_25km_log10)
# 
# summary(m_duration)
# 
# MuMIn::r.squaredGLMM(m_duration) 
# 
# car::vif(m_duration)
# 
# 
# plot_model(m_duration, type = "pred", terms = c("tmean_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration, type = "pred", terms = c("precip_5_yr_ave", "taxon"), show.data = F)
# plot_model(m_duration, type = "pred", terms = c("prop_extreme_warm_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration, type = "pred", terms = c("prop_extreme_cold_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration, type = "pred", terms = c("mRAI_total", "taxon"), show.data = F)
# 
# # sig
# plot_model(m_duration, type = "pred", terms = c("prop_extreme_cold_days_total_logit", "prop_extreme_warm_days_total_logit", "taxon"), show.data = F)
# plot_model(m_duration, type = "pred", terms = c("prop_extreme_cold_days_total_logit", "tmean_5_yr_ave", "taxon"), show.data = F)
# # plot_model(m_duration, type = "pred", terms = c("prop_extreme_wet_days_total_logit", "taxon"), show.data = F)
# 
# formula(m_duration_final)
# 
# anova(m_duration_final)
# 
# m_duration_plot_data = 
#   bind_rows(
#     mutate(as.data.frame(effects::Effect(focal.predictors = c("tmean_5_yr_ave", "taxon"), mod = m_duration_final, se = T, xlevels = 20)), var = "tmean") |> rename(x = tmean_5_yr_ave),
#     mutate(as.data.frame(effects::Effect(focal.predictors = c("temp_seasonality_5_yr_ave", "taxon"), mod = m_duration_final, se = T, xlevels = 20)), var = "temp_seasonality") |> rename(x = temp_seasonality_5_yr_ave),
#     mutate(as.data.frame(effects::Effect(focal.predictors = c("precip_5_yr_ave", "taxon"), mod = m_duration_final, se = T, xlevels = 20)), var = "precip") |> rename(x = precip_5_yr_ave),
#     mutate(as.data.frame(effects::Effect(focal.predictors = c("precip_seasonality_5_yr_ave", "taxon"), mod = m_duration_final, se = T, xlevels = 20)), var = "precip_seasonality") |> rename(x = precip_seasonality_5_yr_ave),
#     mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_total_logit", "taxon"), mod = m_duration_final, se = T, xlevels = 20)), var = "extreme_warm") |> rename(x = prop_extreme_warm_days_total_logit),
#     mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_total_logit", "taxon"), mod = m_duration_final, se = T, xlevels = 20)), var = "extreme_cold") |> rename(x = prop_extreme_cold_days_total_logit),
#     mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_total_logit", "taxon"), mod = m_duration_final, se = T, xlevels = 20)), var = "extreme_wet") |> rename(x = prop_extreme_wet_days_total_logit),
#     mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_dry_days_total_logit", "taxon"), mod = m_duration_final, se = T, xlevels = 20)), var = "extreme_dry") |> rename(x = prop_extreme_dry_days_total_logit)
#   ) |> 
#   as_tibble()
# m_duration_plot_data$sig = "Y"
# m_duration_plot_data$sig[m_duration_plot_data$var == "precip" & m_duration_plot_data$taxon == "plant"] = "N"
# m_duration_plot_data$sig[m_duration_plot_data$var == "precip" & m_duration_plot_data$taxon == "insect"] = "N"
# m_duration_plot_data$sig[m_duration_plot_data$var == "mRAI" & m_duration_plot_data$taxon == "insect"] = "N"
# m_duration_plot_data = as_tibble(m_duration_plot_data)
# m_duration_plot_data = mutate(m_duration_plot_data, sig = factor(sig, levels = c("Y", "N"))) |> 
#   rename(Taxon = taxon, Sig. = sig)
# 
# 
# m_duration_plot_data = mutate(m_duration_plot_data, 
#                               Taxon = case_match(Taxon, "plant" ~ "Plant", "insect" ~ "Insect"),
#                               var =  case_match(var, "tmean" ~ "Annual average temperature (***)",
#                                                   "precip" ~ "Annual total precipitation (NS)",
#                                                   "warm" ~ "Number of extremely warm days (**)",
#                                                   "cold" ~ "Number of extremely cold days (***)",
#                                                   "mRAI" ~ "Modified rainfall anomaly index (NS)"),
#                               var = factor(var, levels = c("Annual average temperature (***)", 
#                                                            "Annual total precipitation (NS)",
#                                                            "Number of extremely warm days (**)",
#                                                            "Number of extremely cold days (***)",
#                                                            "Modified rainfall anomaly index (NS)"))
#                               )
# 
#  names(m_duration_plot_data)
#  
# 
# p_dur = ggplot(m_duration_plot_data, aes(x = x, y = fit)) +
#   geom_ribbon(alpha = 0.2, aes(ymin = lower, ymax = upper, fill = Taxon)) +
#   geom_line(aes(color = Taxon, linetype = Sig.)) +
#   facet_wrap(~var, scales = "free") +
#   labs(x = "Scaled predictors", y = "Duration in days (log transformed)") +
#   cowplot::theme_cowplot() +
#   theme(legend.position = c(0.02, 0.89))
# 
# p_map = readRDS("figures/all_cells_w_data.rds") + theme_void()
# 
# p_proposal = p_dur + patchwork::inset_element(p_map, left = 0.66, bottom = 0, right = 1, top = 0.52)
# ggsave("figures/fig_proposal_nc.png", plot = p_proposal, width = 12, height = 9)
