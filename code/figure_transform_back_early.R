source("code/08_data_exploratory_analysis.R")
load("data_output/models_early.RData")
d2_early = readRDS("data_output/d_early_for_stat.rds")
d2_ave_std_early = readRDS("data_output/d_early_ave_std.rds")
names(d2)

summary(m_onset_final_early_same_as_allsp)


m_final_coef = 
  bind_rows(broom.mixed::tidy(m_onset_final_early_same_as_allsp) |> mutate(Phenology = "Onset"),
            broom.mixed::tidy(m_offset_final_early_same_as_allsp) |> mutate(Phenology = "Offset"),
            broom.mixed::tidy(m_duration_final_early_same_as_allsp) |> mutate(Phenology = "Duration")
  ) |> 
  filter(effect == "fixed") |> 
  dplyr::select(-effect, -group) |> 
  mutate(term = str_remove_all(term, "prop_"),
         term = str_remove_all(term, "_5_yr_ave"),
         term = str_remove_all(term, "_days_before_onset_logit"),
         term = str_remove_all(term, "_days_within_duration_logit"),
         term = str_remove_all(term, "_days_total_logit"),
         term = str_replace_all(term, pattern = "plant", replacement = "Plant"),
         term = str_replace_all(term, pattern = "extreme", replacement = "Extreme"),
         term = str_replace_all(term, pattern = "temp", replacement = "Temp"),
         term = str_replace_all(term, pattern = "precip", replacement = "Precip"),
         term = str_replace_all(term, pattern = "n_days_log", replacement = "Distinct observation days"),
         term = str_replace_all(term, pattern = "[(]Intercept[)]", replacement = "Intercept"),
         estimate = round(estimate, 3)) |> 
  dplyr::select(-std.error, -statistic, -df) |> 
  mutate(term = rtrees:::cap_first_letter(term))
m_final_coef = 
  mutate(m_final_coef, p = ifelse(p.value < 0.001, "***",
                                  ifelse(p.value < 0.01, "**",
                                         ifelse(p.value < 0.05, "*", ""))),
         estimate = paste0(estimate, p)) |> 
  dplyr::select(-p.value, -p) |> 
  pivot_wider(names_from = "Phenology", values_from = "estimate") |> 
  mutate(term = str_replace_all(term, pattern = "Tmean:taxonPlant", replacement = "TaxonPlant:Tmean"))
m_final_coef = tibble(term = c("Intercept", "TaxonPlant", "Tmean", "Temp_seasonality", "Precip", "Precip_seasonality", "Extreme_warm", "Extreme_cold", "Extreme_wet", "Extreme_dry", "Distinct observation days", "TaxonPlant:Tmean", "TaxonPlant:Temp_seasonality", "TaxonPlant:Precip", "TaxonPlant:Precip_seasonality", "TaxonPlant:Extreme_warm", "TaxonPlant:Extreme_cold", "TaxonPlant:Extreme_wet", "TaxonPlant:Extreme_dry", "Tmean:Extreme_warm", "Tmean:Extreme_cold", "Precip:Extreme_wet", "Precip:Extreme_dry", "Extreme_warm:Extreme_dry", "Extreme_warm:Extreme_wet", "TaxonPlant:Tmean:Extreme_warm", "TaxonPlant:Tmean:Extreme_cold", "TaxonPlant:Precip:Extreme_wet", "TaxonPlant:Precip:Extreme_dry", "TaxonPlant:Extreme_warm:Extreme_dry", "TaxonPlant:Extreme_warm:Extreme_wet")) |> 
  left_join(m_final_coef) 
r2s = bind_rows(
  MuMIn::r.squaredGLMM(m_onset_final_early_same_as_allsp) |> as.data.frame() |> mutate(Phenology = "Onset"),
  MuMIn::r.squaredGLMM(m_offset_final_early_same_as_allsp) |> as.data.frame() |> mutate(Phenology = "Offset"),
  MuMIn::r.squaredGLMM(m_duration_final_early_same_as_allsp) |> as.data.frame() |> mutate(Phenology = "Duration")
) |> 
  pivot_longer(cols = starts_with("R2"), names_to = "term") |> 
  mutate(term = recode(term, "R2m" = "Marginal R2", "R2c" = "Conditional R2"),
         value = as.character(round(value, 3))) |> 
  pivot_wider(names_from = "Phenology", values_from = "value")
bind_rows(m_final_coef, r2s) |> 
  rename(Term = term) |> 
  kableExtra::kable(booktabs = T, 
                    linesep = c(rep('', 10), '\\addlinespace', rep('', 3), '\\addlinespace', rep('', 3), '\\addlinespace', rep('', 5), '\\addlinespace', rep('', 5), '\\addlinespace', '')) |> 
  kableExtra::kable_styling(bootstrap_options = c("striped", "scale_down", "condensed", "hover"), 
                            latex_options = c("striped", "condensed"))



# Fig 3 onset ----
g_levels = c(-1.5, 0, 1.5)
ribbon_alpha = 0.15
legend_position = c(0.8, 0.7)
line_linewidth = 1.5
color_values = c("#75D054FF", "#2A788EFF", "#414487FF")
d1_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_before_onset_logit",
                                                              "prop_extreme_dry_days_before_onset_logit", "taxon"), 
                                         mod = m_onset_final_early_same_as_allsp, se = T, 
                                         xlevels = list(prop_extreme_warm_days_before_onset_logit = 20, 
                                                        prop_extreme_dry_days_before_onset_logit = g_levels))) |> 
  rename(`Extreme warm` = prop_extreme_warm_days_before_onset_logit,
         `Extreme dry` = prop_extreme_dry_days_before_onset_logit) |> 
  mutate(`Extreme dry` = recode(`Extreme dry`, "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         `Extreme dry` = as.factor(`Extreme dry`),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "Yes", "Yes")) |> 
  mutate(`Extreme warm` = `Extreme warm` * filter(d2_ave_std_early, var == "prop_extreme_warm_days_before_onset_logit")$std +
           filter(d2_ave_std_early, var == "prop_extreme_warm_days_before_onset_logit")$ave,
         `Extreme warm` = logit_back_to_prop(`Extreme warm`) * 61) 
onset_p1 = ggplot(d1_onset, aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                                fill = `Extreme dry`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Extreme dry`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "", x = "Number of extreme warm days", tag = "E (NS)") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.51, 0.83), legend.box = "horizontal") 


d2_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_before_onset_logit", 
                                                              "precip_5_yr_ave", "taxon"), 
                                         mod = m_onset_final_early_same_as_allsp, se = T, 
                                         xlevels = list(prop_extreme_wet_days_before_onset_logit = 20, 
                                                        precip_5_yr_ave = g_levels))) |> 
  rename(`Extreme wet` = prop_extreme_wet_days_before_onset_logit,
         Precip. = precip_5_yr_ave) |> 
  mutate(Precip. = recode(Precip., "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         Precip. = as.factor(Precip.),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "No", "No")) |> 
  mutate(`Extreme wet` = `Extreme wet` * filter(d2_ave_std_early, var == "prop_extreme_wet_days_before_onset_logit")$std +
           filter(d2_ave_std_early, var == "prop_extreme_wet_days_before_onset_logit")$ave,
         `Extreme wet` = logit_back_to_prop(`Extreme wet`) * 61)
onset_p2 = ggplot(d2_onset, aes(x = `Extreme wet`, y = fit, group = Precip.,
                                fill = Precip.)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = Precip., linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "", x = "Number of extreme wet days", tag = "C (NS)") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.51, 0.83), legend.box = "horizontal") 

d3_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_dry_days_before_onset_logit", 
                                                              "precip_5_yr_ave", "taxon"), 
                                         mod = m_onset_final_early_same_as_allsp, se = T, 
                                         xlevels = list(prop_extreme_dry_days_before_onset_logit = 20, 
                                                        precip_5_yr_ave = g_levels))) |> 
  rename(`Extreme dry` = prop_extreme_dry_days_before_onset_logit,
         Precip. = precip_5_yr_ave) |> 
  mutate(Precip. = recode(Precip., "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         Precip. = as.factor(Precip.),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "No", "Yes")) |> 
  mutate(`Extreme dry` = `Extreme dry` * filter(d2_ave_std_early, var == "prop_extreme_dry_days_before_onset_logit")$std +
           filter(d2_ave_std_early, var == "prop_extreme_dry_days_before_onset_logit")$ave,
         `Extreme dry` = logit_back_to_prop(`Extreme dry`) * 61)
onset_p3 = ggplot(d3_onset, aes(x = `Extreme dry`, y = fit, group = Precip.,
                                fill = Precip.)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = Precip., linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75)) +
  labs(y = "Onset (day of year)", x = "Number of extreme dry days", tag = "D") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.51, 0.8), legend.box = "horizontal") 

d4_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_before_onset_logit", 
                                                              "tmean_5_yr_ave", "taxon"), 
                                         mod = m_onset_final_early_same_as_allsp, se = T, 
                                         xlevels = list(prop_extreme_cold_days_before_onset_logit = 20, 
                                                        tmean_5_yr_ave = g_levels))) |> 
  rename(`Extreme cold` = prop_extreme_cold_days_before_onset_logit,
         Temp. = tmean_5_yr_ave) |> 
  mutate(Temp. = recode(Temp., "-1.5" = "-1.5 sd", "0" = "0 (mean)", "1.5" = "1.5 sd"),
         Temp. = as.factor(Temp.),
         taxon = rtrees:::cap_first_letter(taxon),
         `Sig. Inter.` = ifelse(taxon == "Plant", "No", "Yes")) |> 
  mutate(`Extreme cold` = `Extreme cold` * filter(d2_ave_std_early, var == "prop_extreme_cold_days_before_onset_logit")$std +
           filter(d2_ave_std_early, var == "prop_extreme_cold_days_before_onset_logit")$ave,
         `Extreme cold` = logit_back_to_prop(`Extreme cold`) * 61)
onset_p4 = ggplot(d4_onset, aes(x = `Extreme cold`, y = fit, group = Temp.,
                                fill = Temp.)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = Temp., linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Onset (day of year)", x = "Number of extreme cold days", tag = "B") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.51, 0.87), legend.box = "horizontal") 

# no diff between insect and plant, not plotting it?
plot_model(m_onset_final_early_same_as_allsp, type = "pred", terms = c("prop_extreme_warm_days_before_onset_logit", "tmean_5_yr_ave"), show.data = T)
d5_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_before_onset_logit", 
                                                              "tmean_5_yr_ave"), 
                                         mod = m_onset_final_early_same_as_allsp, se = T, 
                                         xlevels = list(prop_extreme_warm_days_before_onset_logit = 20, 
                                                        tmean_5_yr_ave = g_levels))) |> 
  rename(`Extreme warm` = prop_extreme_warm_days_before_onset_logit,
         `Temp.` = tmean_5_yr_ave) |> 
  mutate(`Temp.` = as.factor(`Temp.`))
onset_p5 = ggplot(d5_onset, aes(x = `Extreme warm`, y = fit, group = `Temp.`,
                                fill = `Temp.`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Temp.`), linewidth = line_linewidth) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "onset") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.8, 0.9), legend.box = "horizontal")
onset_p5

# plot coefs
val = c(
  # "tmean_5_yr_ave", "temp_seasonality_5_yr_ave", "precip_5_yr_ave", 
  #       "precip_seasonality_5_yr_ave", "prop_extreme_warm_days_before_onset_logit",
  #       "prop_extreme_cold_days_before_onset_logit", "prop_extreme_wet_days_before_onset_logit", 
  #       "prop_extreme_dry_days_before_onset_logit",
  "tmean_5_yr_ave:prop_extreme_warm_days_before_onset_logit",
  "tmean_5_yr_ave:prop_extreme_cold_days_before_onset_logit",
  "precip_5_yr_ave:prop_extreme_wet_days_before_onset_logit",
  "precip_5_yr_ave:prop_extreme_dry_days_before_onset_logit",
  "prop_extreme_warm_days_before_onset_logit:prop_extreme_dry_days_before_onset_logit"
)
x_insect = confint(m_onset_final_early_same_as_allsp, level = 0.95, method = "Wald", parm = val)
x_plant = confint(m_onset_final_2_early_same_as_allsp, level = 0.95, method = "Wald", parm = val)

x_insect_2 = broom::tidy(m_onset_final_early_same_as_allsp) |> 
  filter(effect == "fixed", term %in% val) |> 
  dplyr::select(-effect, -group) |> 
  left_join(rownames_to_column(as.data.frame(x_insect), "term"), by = "term") |> 
  left_join(rownames_to_column(as.data.frame(confint(m_onset_final_early_same_as_allsp, level = 0.85, method = "Wald", parm = val)), "term"), by = "term") |> 
  mutate(taxon = "Insect")

x_plant_2 = broom::tidy(m_onset_final_2_early_same_as_allsp) |> 
  filter(effect == "fixed", term %in% val) |> 
  dplyr::select(-effect, -group) |> 
  left_join(rownames_to_column(as.data.frame(x_plant), "term"), by = "term") |> 
  left_join(rownames_to_column(as.data.frame(confint(m_onset_final_2_early_same_as_allsp, level = 0.85, method = "Wald", parm = val)), "term"), by = "term") |> 
  mutate(taxon = "Plant")

x_both_onset = bind_rows(x_insect_2, x_plant_2) |> 
  mutate(term = str_remove_all(term, "prop_"),
         term = str_remove_all(term, "_5_yr_ave"),
         term = str_remove_all(term, "_days_before_onset_logit"),
         term = str_remove_all(term, "_days_within_duration_logit"),
         term = str_remove_all(term, "_days_total_logit"),
         term = str_replace_all(term, pattern = "plant", replacement = "Plant"),
         term = str_replace_all(term, pattern = "extreme", replacement = "Extreme"),
         term = str_replace_all(term, pattern = "tmean", replacement = "Temp"),
         term = str_replace_all(term, pattern = "temp", replacement = "Temp"),
         term = str_replace_all(term, pattern = "precip", replacement = "Precip")) |> 
  mutate(term = recode(term, "Temp:Extreme_cold" = "Temp:Extreme_cold (*)", 
                       "Precip:Extreme_wet" = "Precip:Extreme_wet (.)", 
                       "Precip:Extreme_dry" = "Precip:Extreme_dry (*)")) |> 
  mutate(term = factor(term, levels = c(
    "Temp:Extreme_warm", "Extreme_warm:Extreme_dry", "Precip:Extreme_dry (*)",
    "Precip:Extreme_wet (.)", "Temp:Extreme_cold (*)"
  )))

p_coef_final_onset = ggplot(x_both_onset, aes(x = estimate, y = term, color = taxon)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(position = position_dodge(width = 0.3), 
                  aes(xmin = `2.5 %`, xmax = `97.5 %`), 
                  alpha = ifelse(x_both_onset$p.value < 0.05, 1, 0.2)) +
  geom_linerange(position = position_dodge(width = 0.3), 
                 aes(xmin = `7.5 %`, xmax = `92.5 %`), 
                 linewidth = 2, 
                 alpha = ifelse(x_both_onset$p.value < 0.05, 1, 0.2)) +
  labs(x = "Onset Coefficients", y = "", color = "Taxon") +
  # scale_x_continuous(breaks = c(-15, -10, -5, 0, 5)) +
  cowplot::theme_cowplot(font_size = 14) +
  theme(legend.position = "top") 

library(patchwork)

layout <- "
ABBCC
ADDEE
"
onset_pall = p_coef_final_onset + onset_p4 + onset_p2 + onset_p3 + onset_p1 +
  plot_layout(design = layout)# + plot_annotation(tag_levels = 'A')

# onset_pall = cowplot::plot_grid(onset_p3, onset_p2, onset_p1, onset_p4, nrow = 2, labels = "AUTO")
ggsave("figures/fig_3_onset_prop_early.pdf", plot = onset_pall, width = 15, height = 9.5)
ggsave("figures/fig_3_onset_prop_early.png", plot = onset_pall, width = 15, height = 9.5)

