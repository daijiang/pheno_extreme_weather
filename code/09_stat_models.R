source("code/08_data_exploratory_analysis.R")

names(d2)

# onset models ----

m_onset <- lmer(onset ~ tmean_5_yr_ave * pop_25km_log10 + precip_5_yr_ave +
                  n_unusual_warm_days_before_onset_log + 
                  n_unusual_cold_days_before_onset_log +
                  n_unusual_wet_days_before_onset_log +
                  n_days_log +  # to count for collection effort
                  (1|sp) + (1 | id_cells) + ( 1 | yr) +
                  (0 + n_unusual_warm_days_before_onset_log | sp) + 
                  (0 + n_unusual_cold_days_before_onset_log | sp) + 
                  (0 + n_unusual_wet_days_before_onset_log | sp) + 
                  (0 + precip_5_yr_ave | sp) +
                  (0 + tmean_5_yr_ave | sp) +
                  (0 + pop_25km_log10 | sp), 
                data = d2, na.action = "na.fail")

summary(m_onset)

# # interaction between tmean and pop is not significant, remove
m_onset = update(m_onset, . ~ . - tmean_5_yr_ave : pop_25km_log10 - n_unusual_wet_days_before_onset_log)

summary(m_onset)

MuMIn::r.squaredGLMM(m_onset) 

car::vif(m_onset)


m_onset_plot_data = sjPlot::get_model_data(m_onset, type = "pred")
plot(m_onset_plot_data$tmean_5_yr_ave) +
  geom_point(data = d2, aes(x = tmean_5_yr_ave, y = onset), inherit.aes = F, alpha = 0.3)

plot_model(m_onset, type = "pred", terms = c("tmean_5_yr_ave"), show.data = T)
plot_model(m_onset, type = "pred", terms = c("precip_5_yr_ave"), show.data = T)
plot_model(m_onset, type = "pred", terms = c("pop_25km_log10"), show.data = T)
plot_model(m_onset, type = "pred", terms = c("n_unusual_warm_days_before_onset_log"), show.data = T)
plot_model(m_onset, type = "pred", terms = c("n_unusual_cold_days_before_onset_log"), show.data = T)

# offset

# offset models ----

m_offset <- lmer(offset ~ tmean_5_yr_ave * pop_25km_log10 + precip_5_yr_ave +
                  n_unusual_warm_days_within_duration_log + 
                  n_unusual_cold_days_within_duration_log +
                  n_unusual_wet_days_within_duration_log +
                  n_days_log +  # to count for collection effort
                  (1|sp) + (1 | id_cells) + ( 1 | yr) +
                  (0 + n_unusual_warm_days_within_duration_log | sp) + 
                  (0 + n_unusual_cold_days_within_duration_log | sp) + 
                  (0 + n_unusual_wet_days_within_duration_log | sp) + 
                  (0 + precip_5_yr_ave | sp) +
                  (0 + tmean_5_yr_ave | sp) +
                  (0 + pop_25km_log10 | sp), 
                data = d2, na.action = "na.fail")

summary(m_offset)

# # interaction between teamn and pop is not significant, remove
m_offset = update(m_offset, . ~ . - tmean_5_yr_ave : pop_25km_log10 - pop_25km_log10 -  (0 + pop_25km_log10 | sp))

summary(m_offset)

MuMIn::r.squaredGLMM(m_offset) 

car::vif(m_offset)


plot_model(m_offset, type = "pred", terms = c("tmean_5_yr_ave"), show.data = T)
plot_model(m_offset, type = "pred", terms = c("precip_5_yr_ave"), show.data = T)
plot_model(m_offset, type = "pred", terms = c("n_unusual_warm_days_within_duration_log"), show.data = T)
plot_model(m_offset, type = "pred", terms = c("n_unusual_cold_days_within_duration_log"), show.data = T)

# duration models ----

m_duration <- lmer(duration_log ~ tmean_5_yr_ave * pop_25km_log10 + precip_5_yr_ave +
                     n_unusual_warm_days_total_log + 
                     n_unusual_cold_days_total_log +
                     n_unusual_wet_days_total_log +
                   n_days_log +  # to count for collection effort
                   (1|sp) + (1 | id_cells) + # ( 1 | yr) +
                   (0 + n_unusual_warm_days_total_log | sp) + 
                   (0 + n_unusual_cold_days_total_log | sp) + 
                   (0 + n_unusual_wet_days_total_log | sp) + 
                   (0 + precip_5_yr_ave | sp) +
                   (0 + tmean_5_yr_ave | sp) +
                   (0 + pop_25km_log10 | sp), 
                 data = d2, na.action = "na.fail")

summary(m_duration)

# # interaction between teamn and pop is not significant, remove
m_duration = update(m_duration, . ~ . - tmean_5_yr_ave : pop_25km_log10)

summary(m_duration)

MuMIn::r.squaredGLMM(m_duration) 

car::vif(m_duration)


plot_model(m_duration, type = "pred", terms = c("tmean_5_yr_ave"), show.data = T)
plot_model(m_duration, type = "pred", terms = c("precip_5_yr_ave"), show.data = T)
plot_model(m_duration, type = "pred", terms = c("pop_25km_log10"), show.data = T)
plot_model(m_duration, type = "pred", terms = c("n_unusual_warm_days_total_log"), show.data = T)
plot_model(m_duration, type = "pred", terms = c("n_unusual_cold_days_total_log"), show.data = F)
plot_model(m_duration, type = "pred", terms = c("n_unusual_wet_days_total_log"), show.data = T)
plot_model(m_duration, type = "pred", rm.terms = c("n_days_log"), grid = T)

m_duration_plot_data = sjPlot::get_model_data(m_duration, type = "pred", rm.terms = "n_days_log")
m_duration_plot_data = m_duration_plot_data[1:6]
m_duration_plot_data = purrr::map(m_duration_plot_data, as_tibble) |> 
  purrr::map_dfr(bind_rows)
m_duration_plot_data = mutate(m_duration_plot_data, group = as.character(group)) |> 
  case_match(group, )

p_dur = ggplot(m_duration_plot_data, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  facet_wrap(~group, scales = "free_x") +
  labs(x = "Scaled predictors", y = "Flowering duration in days (log transformed)") +
  cowplot::theme_cowplot()

p_proposal = p_dur + patchwork::inset_element(readRDS("figures/all_cells_w_data.rds"), left = 0.09, bottom = 0.55, right = 0.54, top = 0.8)
ggsave("figures/fig_proposal.png", plot = p_proposal, width = 16, height = 12)
