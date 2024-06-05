source("code/08_data_exploratory_analysis.R")
load("data_output/models.RData")
d2_ave_std = readRDS("data_output/d_ave_std.rds")
names(d2)

d = readRDS("data_output/d_raw_for_stat.rds") |> 
  mutate(taxon = rtrees:::cap_first_letter(taxon))

# Fig 3 onset ----
g_levels = c(-1.5, 0, 1.5)
ribbon_alpha = 0.15
legend_position = c(0.8, 0.7)
line_linewidth = 1.5
color_values = c("#75D054FF", "#2A788EFF", "#414487FF")
d1_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_before_onset_logit",
                                                              "prop_extreme_dry_days_before_onset_logit",
                                                              "taxon"), 
                                         mod = m_onset_final, se = T, 
                                         xlevels = list(prop_extreme_warm_days_before_onset_logit = 20, 
                                                        prop_extreme_dry_days_before_onset_logit = g_levels))) |> 
  rename(`Extreme warm` = prop_extreme_warm_days_before_onset_logit,
         `Extreme dry` = prop_extreme_dry_days_before_onset_logit) |> 
  mutate(`Extreme dry` = recode(`Extreme dry`, "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Extreme dry` = as.factor(`Extreme dry`),
         taxon = rtrees:::cap_first_letter(taxon)) |> 
  mutate(`Extreme warm` = `Extreme warm` * filter(d2_ave_std, var == "prop_extreme_warm_days_before_onset_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_warm_days_before_onset_logit")$ave,
         `Extreme warm` = logit_back_to_prop(`Extreme warm`) * 61) 
onset_p1 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_warm_days_before_onset, y = onset), alpha = 0.1) +
  geom_ribbon(data = d1_onset, mapping = aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                                             fill = `Extreme dry`,ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                color = `Extreme dry`), 
            data = d1_onset,
            linewidth = line_linewidth) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "", x = "Number of extreme warm days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.75, 0.8)) 


d2_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_before_onset_logit", 
                                                              "precip_5_yr_ave", "taxon"), 
                                         mod = m_onset_final, se = T, 
                                         xlevels = list(prop_extreme_wet_days_before_onset_logit = 20, 
                                                        precip_5_yr_ave = g_levels))) |> 
  rename(`Extreme wet` = prop_extreme_wet_days_before_onset_logit,
         Precip. = precip_5_yr_ave) |> 
  mutate(Precip. = recode(Precip., "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         Precip. = as.factor(Precip.),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "No", "Yes")) |> 
  mutate(`Extreme wet` = `Extreme wet` * filter(d2_ave_std, var == "prop_extreme_wet_days_before_onset_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_wet_days_before_onset_logit")$ave,
         `Extreme wet` = logit_back_to_prop(`Extreme wet`) * 61)
onset_p2 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_wet_days_before_onset, y = onset), alpha = 0.1) +
  geom_ribbon(data = d2_onset, mapping = aes(x = `Extreme wet`, y = fit, group = `Precip.`,
                                             fill = `Precip.`,ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme wet`, y = fit, group = `Precip.`,
                color = `Precip.`), 
            data = d2_onset,
            linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "", x = "Number of extreme wet days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.75, 0.8), legend.box = "horizontal") 

d3_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_dry_days_before_onset_logit", 
                                                              "precip_5_yr_ave", "taxon"), 
                                         mod = m_onset_final, se = T, 
                                         xlevels = list(prop_extreme_dry_days_before_onset_logit = 20, 
                                                        precip_5_yr_ave = g_levels))) |> 
  rename(`Extreme dry` = prop_extreme_dry_days_before_onset_logit,
         Precip. = precip_5_yr_ave) |> 
  mutate(Precip. = recode(Precip., "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         Precip. = as.factor(Precip.),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "No", "Yes")) |> 
  mutate(`Extreme dry` = `Extreme dry` * filter(d2_ave_std, var == "prop_extreme_dry_days_before_onset_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_dry_days_before_onset_logit")$ave,
         `Extreme dry` = logit_back_to_prop(`Extreme dry`) * 61)
onset_p3 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_dry_days_before_onset, y = onset), alpha = 0.1) +
  geom_ribbon(data = d3_onset, mapping = aes(x = `Extreme dry`, y = fit, group = `Precip.`,
                                             fill = `Precip.`,ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme dry`, y = fit, group = `Precip.`,
                color = `Precip.`, linetype = `Sig. Inter.`), 
            data = d3_onset,
            linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75)) +
  labs(y = "Onset (day of year)", x = "Number of extreme dry days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.65, 0.8), legend.box = "horizontal") 

d4_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_before_onset_logit", 
                                                              "tmean_5_yr_ave", "taxon"), 
                                         mod = m_onset_final, se = T, 
                                         xlevels = list(prop_extreme_cold_days_before_onset_logit = 20, 
                                                        tmean_5_yr_ave = g_levels))) |> 
  rename(`Extreme cold` = prop_extreme_cold_days_before_onset_logit,
         Temp. = tmean_5_yr_ave) |> 
  mutate(Temp. = recode(Temp., "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         Temp. = as.factor(Temp.),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "No", "Yes")) |> 
  mutate(`Extreme cold` = `Extreme cold` * filter(d2_ave_std, var == "prop_extreme_cold_days_before_onset_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_cold_days_before_onset_logit")$ave,
         `Extreme cold` = logit_back_to_prop(`Extreme cold`) * 61)
onset_p4 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_cold_days_before_onset, y = onset), alpha = 0.1) +
  geom_ribbon(data = d4_onset, mapping = aes(x = `Extreme cold`, y = fit, group = `Temp.`,
                                             fill = `Temp.`,ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme cold`, y = fit, group = `Temp.`,
                color = `Temp.`, linetype = `Sig. Inter.`), 
            data = d4_onset,
            linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Onset (day of year)", x = "Number of extreme cold days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.63, 0.87), legend.box = "horizontal") 


library(patchwork)

layout <- "
BBCC
DDEE
"
onset_pall = onset_p4 + onset_p2 + onset_p3 + onset_p1 +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')

# onset_pall = cowplot::plot_grid(onset_p3, onset_p2, onset_p1, onset_p4, nrow = 2, labels = "AUTO")
ggsave("figures/fig_3_onset_prop_w_data.pdf", plot = onset_pall, width = 12, height = 9.5)
ggsave("figures/fig_3_onset_prop_w_data.png", plot = onset_pall, width = 12, height = 9.5)

# Fig 4 offset -----


# plot_model(m_offset_final, type = "pred", terms = c("prop_extreme_warm_days_within_duration_logit", "tmean_5_yr_ave", "taxon"), show.data = F) +
#   ggtitle("Neither insects nor plants have siginificant interaction between extreme warm and tmean",
#           subtitle = "though insects and plants have different relationship between extreme warm and tmean") +
#   theme(legend.position = c(0.8, 0.85))
# # decided not to plot this one given that neither insects nor plants have significant interaction between extreme warm and temp.

d1_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_within_duration_logit", "tmean_5_yr_ave", "taxon"), 
                                          mod = m_offset_final, se = T, 
                                          xlevels = list(prop_extreme_cold_days_within_duration_logit = 20, 
                                                         tmean_5_yr_ave = g_levels))) |> 
  rename(`Extreme cold` = prop_extreme_cold_days_within_duration_logit,
         `Temp.` = tmean_5_yr_ave) |> 
  mutate(Temp. = recode(Temp., "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Temp.` = as.factor(`Temp.`),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "No", "No")) |> 
  mutate(`Extreme cold` = `Extreme cold` * filter(d2_ave_std, var == "prop_extreme_cold_days_within_duration_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_cold_days_within_duration_logit")$ave,
         `Extreme cold` = logit_back_to_prop(`Extreme cold`))
offset_p1 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_cold_days_within_duration/duration, y = offset), alpha = 0.1) +
  geom_ribbon(data = d1_offset, mapping = aes(x = `Extreme cold`, y = fit, group = `Temp.`,
                                             fill = `Temp.`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme cold`, y = fit, group = `Temp.`,
                color = `Temp.`, linetype = `Sig. Inter.`), 
            data = d1_offset,
            linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Offset (day of year)", x = "Proportion of extreme cold days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.65, 0.83), legend.box = "horizontal") 

d2_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_within_duration_logit", "precip_5_yr_ave"), 
                                          mod = m_offset_final, se = T, 
                                          xlevels = list(prop_extreme_wet_days_within_duration_logit = 20, 
                                                         precip_5_yr_ave = g_levels))) |> 
  rename(`Extreme wet` = prop_extreme_wet_days_within_duration_logit,
         Precip. = precip_5_yr_ave) |> 
  mutate(Precip. = recode(Precip., "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         Precip. = as.factor(Precip.)) |> 
  mutate(`Extreme wet` = `Extreme wet` * filter(d2_ave_std, var == "prop_extreme_wet_days_within_duration_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_wet_days_within_duration_logit")$ave,
         `Extreme wet` = logit_back_to_prop(`Extreme wet`))
offset_p2 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_wet_days_within_duration/duration, y = offset), alpha = 0.1) +
  geom_ribbon(data = d2_offset, mapping = aes(x = `Extreme wet`, y = fit, group = `Precip.`,
                                              fill = `Precip.`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme wet`, y = fit, group = `Precip.`,
                color = `Precip.`), 
            data = d2_offset,
            linewidth = line_linewidth) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "", x = "Proportion of extreme wet days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.7, 0.85)) 

# d3_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_within_duration_logit", 
#                                                                "prop_extreme_dry_days_within_duration_logit"), 
#                                           mod = m_offset_final, se = T, 
#                                           xlevels = list(prop_extreme_warm_days_within_duration_logit = 20, 
#                                                          tmean_5_yr_ave = g_levels))) |> 
#   rename(`Extreme warm` = prop_extreme_warm_days_within_duration_logit,
#          Temp. = tmean_5_yr_ave) |> 
#   mutate(Temp. = as.factor(Temp.))
# offset_p3 = ggplot(d3_offset, aes(x = `Extreme warm`, y = fit, group = Temp.,
#                                   fill = Temp.)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
#   geom_line(aes(color = Temp.), linewidth = line_linewidth) +
#   scale_fill_manual(values = color_values) +
#   scale_color_manual(values = color_values) +
#   labs(y = "Offset") +
#   cowplot::theme_cowplot() +
#   theme(legend.position = c(0.1, 0.86)) 

d3_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_within_duration_logit", 
                                                               "prop_extreme_dry_days_within_duration_logit", "taxon"), 
                                          mod = m_offset_final, se = T, 
                                          xlevels = list(prop_extreme_warm_days_within_duration_logit = 20, 
                                                         prop_extreme_dry_days_within_duration_logit = g_levels))) |> 
  rename(`Extreme warm` = prop_extreme_warm_days_within_duration_logit,
         `Extreme dry` = prop_extreme_dry_days_within_duration_logit) |> 
  mutate(`Extreme dry` = recode(`Extreme dry`, "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Extreme dry` = as.factor(`Extreme dry`),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "No", "No")) |> 
  mutate(`Extreme warm` = `Extreme warm` * filter(d2_ave_std, var == "prop_extreme_warm_days_within_duration_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_warm_days_within_duration_logit")$ave,
         `Extreme warm` = logit_back_to_prop(`Extreme warm`))
offset_p3 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_warm_days_within_duration/duration, y = offset), alpha = 0.1) +
  geom_ribbon(data = d3_offset, mapping = aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                                              fill = `Extreme dry`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                color = `Extreme dry`, linetype = `Sig. Inter.`), 
            data = d3_offset,
            linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75)) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "", x = "Proportion of extreme warm days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.56, 0.8), legend.box = "horizontal")

d4_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_within_duration_logit", 
                                                               "prop_extreme_wet_days_within_duration_logit"), 
                                          mod = m_offset_final, se = T, 
                                          xlevels = list(prop_extreme_warm_days_within_duration_logit = 20, 
                                                         prop_extreme_wet_days_within_duration_logit = g_levels))) |> 
  rename(`Extreme warm` = prop_extreme_warm_days_within_duration_logit,
         `Extreme wet` = prop_extreme_wet_days_within_duration_logit) |> 
  mutate(`Extreme wet` = recode(`Extreme wet`, "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Extreme wet` = as.factor(`Extreme wet`)) |> 
  mutate(`Extreme warm` = `Extreme warm` * filter(d2_ave_std, var == "prop_extreme_warm_days_within_duration_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_warm_days_within_duration_logit")$ave,
         `Extreme warm` = logit_back_to_prop(`Extreme warm`))
offset_p4 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_warm_days_within_duration/duration, y = offset), alpha = 0.1) +
  geom_ribbon(data = d4_offset, mapping = aes(x = `Extreme warm`, y = fit, group = `Extreme wet`,
                                              fill = `Extreme wet`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme warm`, y = fit, group = `Extreme wet`,
                color = `Extreme wet`), 
            data = d4_offset,
            linewidth = line_linewidth) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Offset (day of year)", x = "Proportion of extreme warm days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.7, 0.8)) 


layout <- "
BBCC
DDEE
"
offset_pall = offset_p1 + offset_p3 + offset_p4 + offset_p2 +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')

ggsave("figures/fig_4_offset_prop_w_data.pdf", plot = offset_pall, width = 12, height = 9.5)
ggsave("figures/fig_4_offset_prop_w_data.png", plot = offset_pall, width = 12, height = 9.5)

# fig 5 duration -----
d1_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_total_logit", "precip_5_yr_ave", "taxon"), 
                                            mod = m_duration_final, se = T, 
                                            xlevels = list(prop_extreme_wet_days_total_logit = 20, 
                                                           precip_5_yr_ave = g_levels))) |> 
  rename(`Extreme wet` = prop_extreme_wet_days_total_logit,
         `Precip.` = precip_5_yr_ave) |> 
  mutate(`Precip.` = recode(`Precip.`, "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Precip.` = as.factor(`Precip.`),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "Yes", "No")) |> 
  mutate(`Extreme wet` = `Extreme wet` * filter(d2_ave_std, var == "prop_extreme_wet_days_total_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_wet_days_total_logit")$ave,
         `Extreme wet` = logit_back_to_prop(`Extreme wet`),
         fit = exp(fit), lower = exp(lower), upper = exp(upper))
duration_p1 =  ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_wet_days_total/(duration + 61), y = duration), alpha = 0.1) +
  geom_ribbon(data = d1_duration, mapping = aes(x = `Extreme wet`, y = fit, group = `Precip.`,
                                              fill = `Precip.`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme wet`, y = fit, group = `Precip.`,
                color = `Precip.`, linetype = `Sig. Inter.`), 
            data = d1_duration,
            linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3)) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Duration (number of days)",
       x = "Proportion of extreme wet days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.62, 0.8), legend.box = "horizontal") 


d2_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_dry_days_total_logit", "precip_5_yr_ave", "taxon"), 
                                            mod = m_duration_final, se = T, 
                                            xlevels = list(prop_extreme_dry_days_total_logit = 20, 
                                                           precip_5_yr_ave = g_levels))) |> 
  rename(`Extreme dry` = prop_extreme_dry_days_total_logit,
         `Precip.` = precip_5_yr_ave) |> 
  mutate(`Precip.` = recode(`Precip.`, "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Precip.` = as.factor(`Precip.`),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "Yes", "No")) |> 
  mutate(`Extreme dry` = `Extreme dry` * filter(d2_ave_std, var == "prop_extreme_dry_days_total_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_dry_days_total_logit")$ave,
         `Extreme dry` = logit_back_to_prop(`Extreme dry`),
         fit = exp(fit), lower = exp(lower), upper = exp(upper))
duration_p2 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_dry_days_total/(duration + 61), y = duration), alpha = 0.1) +
  geom_ribbon(data = d2_duration, mapping = aes(x = `Extreme dry`, y = fit, group = `Precip.`,
                                                fill = `Precip.`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme dry`, y = fit, group = `Precip.`,
                color = `Precip.`, linetype = `Sig. Inter.`), 
            data = d2_duration,
            linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9)) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "",
       x = "Proportion of extreme dry days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.62, 0.8), legend.box = "horizontal") 

d3_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_total_logit", 
                                                                 "prop_extreme_wet_days_total_logit", "taxon"), 
                                            mod = m_duration_final, se = T, 
                                            xlevels = list(prop_extreme_warm_days_total_logit = 20, 
                                                           prop_extreme_wet_days_total_logit = g_levels))) |> 
  rename(`Extreme warm` = prop_extreme_warm_days_total_logit,
         `Extreme wet` = prop_extreme_wet_days_total_logit) |> 
  mutate(`Extreme wet` = recode(`Extreme wet`, "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Extreme wet` = as.factor(`Extreme wet`),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "Yes", "No")) |> 
  mutate(`Extreme warm` = `Extreme warm` * filter(d2_ave_std, var == "prop_extreme_warm_days_total_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_warm_days_total_logit")$ave,
         `Extreme warm` = logit_back_to_prop(`Extreme warm`),
         fit = exp(fit), lower = exp(lower), upper = exp(upper))
duration_p3 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_warm_days_total/(duration + 61), y = duration), alpha = 0.1) +
  geom_ribbon(data = d3_duration, mapping = aes(x = `Extreme warm`, y = fit, group = `Extreme wet`,
                                                fill = `Extreme wet`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme warm`, y = fit, group = `Extreme wet`,
                color = `Extreme wet`, linetype = `Sig. Inter.`), 
            data = d3_duration,
            linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Duration (number of days)",
       x = "Proportion of extreme warm days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.61, 0.8), legend.box = "horizontal") 

d4_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_total_logit", 
                                                                 "tmean_5_yr_ave"), 
                                            mod = m_duration_final, se = T, 
                                            xlevels = list(prop_extreme_warm_days_total_logit = 20, 
                                                           tmean_5_yr_ave = g_levels))) |> 
  rename(`Extreme warm` = prop_extreme_warm_days_total_logit,
         `Temp.` = tmean_5_yr_ave) |> 
  mutate(`Temp.` = recode(`Temp.`, "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Temp.` = as.factor(`Temp.`)) |> 
  mutate(`Extreme warm` = `Extreme warm` * filter(d2_ave_std, var == "prop_extreme_warm_days_total_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_warm_days_total_logit")$ave,
         `Extreme warm` = logit_back_to_prop(`Extreme warm`),
         fit = exp(fit), lower = exp(lower), upper = exp(upper))
mutate(d, `Extreme warm` = logit_back_to_prop(prop_extreme_warm_days_total_logit))

duration_p4 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_warm_days_total/(duration + 61), y = duration), alpha = 0.1) +
  geom_ribbon(data = d4_duration, mapping = aes(x = `Extreme warm`, y = fit, group = `Temp.`,
                                                fill = `Temp.`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme warm`, y = fit, group = `Temp.`,
                color = `Temp.`), 
            data = d4_duration,
            linewidth = line_linewidth) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Duration (number of days)",
       x = "Proportion of extreme warm days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.7, 0.9), legend.box = "horizontal") 

d5_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_total_logit", 
                                                                 "tmean_5_yr_ave", "taxon"), 
                                            mod = m_duration_final, se = T, 
                                            xlevels = list(prop_extreme_cold_days_total_logit = 20, 
                                                           tmean_5_yr_ave = g_levels))) |> 
  rename(`Extreme cold` = prop_extreme_cold_days_total_logit,
         `Temp.` = tmean_5_yr_ave) |> 
  mutate(`Temp.` = recode(`Temp.`, "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Temp.` = as.factor(`Temp.`),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Insect", "Yes", "No")) |> 
  mutate(`Extreme cold` = `Extreme cold` * filter(d2_ave_std, var == "prop_extreme_cold_days_total_logit")$std +
           filter(d2_ave_std, var == "prop_extreme_cold_days_total_logit")$ave,
         `Extreme cold` = logit_back_to_prop(`Extreme cold`),
         fit = exp(fit), lower = exp(lower), upper = exp(upper))
duration_p5 = ggplot() +
  geom_point(data = d, mapping = aes(x = n_extreme_cold_days_total/(duration + 61), y = duration), alpha = 0.1) +
  geom_ribbon(data = d5_duration, mapping = aes(x = `Extreme cold`, y = fit, group = `Temp.`,
                                                fill = `Temp.`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(aes(x = `Extreme cold`, y = fit, group = `Temp.`,
                color = `Temp.`, linetype = `Sig. Inter.`), 
            data = d5_duration,
            linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "", x = "Proportion of extreme cold days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.6, 0.8), legend.box = "horizontal") 

duration_p4 # no diff between plant and insect, not plotting it?



layout <- "
BBCC
DDEE
"
duration_pall =  duration_p4 + duration_p5 + duration_p1 + duration_p2 +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
ggsave("figures/fig_5_duration_prop_w_data.pdf", plot = duration_pall, width = 12, height = 9.5)
ggsave("figures/fig_5_duration_prop_w_data.png", plot = duration_pall, width = 12, height = 9.5)

