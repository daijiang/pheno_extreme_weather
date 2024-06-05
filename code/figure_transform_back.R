source("code/08_data_exploratory_analysis.R")
load("data_output/models.RData")
d2_ave_std = readRDS("data_output/d_ave_std.rds")
names(d2)

# Fig 1: map -----
length(unique(d2$id_cells))

grids_usa_used = read_rds("data_output/grids_usa_used.rds")

all(unique(d2$id_cells) %in% grids_usa_used$id_cells)

grid_plot = filter(grids_usa_used, id_cells %in% unique(d2$id_cells))

us_map = readRDS("data/usa_map.rds")
plt_base = ggplot() +
  geom_sf(data = us_map) +
  geom_sf(data = readRDS("data/lakes.rds"), color = "gray50", fill = "white")

fig_1 = plt_base +
  geom_sf(data = st_geometry(grid_plot), fill = "purple") +
  theme_minimal()

ggsave(filename = "figures/fig_1_map.pdf", width = 7, height = 5)
ggsave(filename = "figures/fig_1_map.png", width = 7, height = 5)

# Fig 2: extreme weathers -----
m_main_effect_plot_data = 
  bind_rows(
    bind_rows(
      mutate(as.data.frame(effects::Effect(focal.predictors = c("tmean_5_yr_ave", "taxon"), mod = m_onset_1, se = T, xlevels = 20)), var = "tmean") |> rename(x = tmean_5_yr_ave),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("temp_seasonality_5_yr_ave", "taxon"), mod = m_onset_1, se = T, xlevels = 20)), var = "temp_seasonality") |> rename(x = temp_seasonality_5_yr_ave),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("precip_5_yr_ave", "taxon"), mod = m_onset_1, se = T, xlevels = 20)), var = "precip") |> rename(x = precip_5_yr_ave),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("precip_seasonality_5_yr_ave", "taxon"), mod = m_onset_1, se = T, xlevels = 20)), var = "precip_seasonality") |> rename(x = precip_seasonality_5_yr_ave),
      # mutate(as.data.frame(effects::Effect(focal.predictors = c("pop_25km_log10", "taxon"), mod = m_onset_1, se = T, xlevels = 20)), var = "urbanization") |> rename(x = pop_25km_log10),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_before_onset_logit", "taxon"), mod = m_onset_1, se = T, xlevels = 20)), var = "extreme_warm") |> rename(x = prop_extreme_warm_days_before_onset_logit),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_before_onset_logit", "taxon"), mod = m_onset_1, se = T, xlevels = 20)), var = "extreme_cold") |> rename(x = prop_extreme_cold_days_before_onset_logit),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_before_onset_logit", "taxon"), mod = m_onset_1, se = T, xlevels = 20)), var = "extreme_wet") |> rename(x = prop_extreme_wet_days_before_onset_logit),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_dry_days_before_onset_logit", "taxon"), mod = m_onset_1, se = T, xlevels = 20)), var = "extreme_dry") |> rename(x = prop_extreme_dry_days_before_onset_logit)
    ) |> 
      mutate(phenology = "Onset"),
    bind_rows(
      mutate(as.data.frame(effects::Effect(focal.predictors = c("tmean_5_yr_ave", "taxon"), mod = m_offset_1, se = T, xlevels = 20)), var = "tmean") |> rename(x = tmean_5_yr_ave),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("temp_seasonality_5_yr_ave", "taxon"), mod = m_offset_1, se = T, xlevels = 20)), var = "temp_seasonality") |> rename(x = temp_seasonality_5_yr_ave),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("precip_5_yr_ave", "taxon"), mod = m_offset_1, se = T, xlevels = 20)), var = "precip") |> rename(x = precip_5_yr_ave),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("precip_seasonality_5_yr_ave", "taxon"), mod = m_offset_1, se = T, xlevels = 20)), var = "precip_seasonality") |> rename(x = precip_seasonality_5_yr_ave),
      # mutate(as.data.frame(effects::Effect(focal.predictors = c("pop_25km_log10", "taxon"), mod = m_offset_1, se = T, xlevels = 20)), var = "urbanization") |> rename(x = pop_25km_log10),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_within_duration_logit", "taxon"), mod = m_offset_1, se = T, xlevels = 20)), var = "extreme_warm") |> rename(x = prop_extreme_warm_days_within_duration_logit),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_within_duration_logit", "taxon"), mod = m_offset_1, se = T, xlevels = 20)), var = "extreme_cold") |> rename(x = prop_extreme_cold_days_within_duration_logit),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_within_duration_logit", "taxon"), mod = m_offset_1, se = T, xlevels = 20)), var = "extreme_wet") |> rename(x = prop_extreme_wet_days_within_duration_logit),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_dry_days_within_duration_logit", "taxon"), mod = m_offset_1, se = T, xlevels = 20)), var = "extreme_dry") |> rename(x = prop_extreme_dry_days_within_duration_logit)
    ) |> 
      mutate(phenology = "Offset"),
    bind_rows(
      mutate(as.data.frame(effects::Effect(focal.predictors = c("tmean_5_yr_ave", "taxon"), mod = m_duration_1, se = T, xlevels = 20)), var = "tmean") |> rename(x = tmean_5_yr_ave),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("temp_seasonality_5_yr_ave", "taxon"), mod = m_duration_1, se = T, xlevels = 20)), var = "temp_seasonality") |> rename(x = temp_seasonality_5_yr_ave),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("precip_5_yr_ave", "taxon"), mod = m_duration_1, se = T, xlevels = 20)), var = "precip") |> rename(x = precip_5_yr_ave),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("precip_seasonality_5_yr_ave", "taxon"), mod = m_duration_1, se = T, xlevels = 20)), var = "precip_seasonality") |> rename(x = precip_seasonality_5_yr_ave),
      # mutate(as.data.frame(effects::Effect(focal.predictors = c("pop_25km_log10", "taxon"), mod = m_duration_1, se = T, xlevels = 20)), var = "urbanization") |> rename(x = pop_25km_log10),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_total_logit", "taxon"), mod = m_duration_1, se = T, xlevels = 20)), var = "extreme_warm") |> rename(x = prop_extreme_warm_days_total_logit),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_total_logit", "taxon"), mod = m_duration_1, se = T, xlevels = 20)), var = "extreme_cold") |> rename(x = prop_extreme_cold_days_total_logit),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_total_logit", "taxon"), mod = m_duration_1, se = T, xlevels = 20)), var = "extreme_wet") |> rename(x = prop_extreme_wet_days_total_logit),
      mutate(as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_dry_days_total_logit", "taxon"), mod = m_duration_1, se = T, xlevels = 20)), var = "extreme_dry") |> rename(x = prop_extreme_dry_days_total_logit)
    ) |> 
      mutate(phenology = "Duration")
  ) |> as_tibble()

# summary(m_onset_1)
m_main_effect_plot_data$sig = "Y"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Onset" & m_main_effect_plot_data$taxon == "insect" & m_main_effect_plot_data$var == "precip"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Onset" & m_main_effect_plot_data$taxon == "insect" & m_main_effect_plot_data$var == "precip_seasonality"] = "N"
# m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Onset" & m_main_effect_plot_data$taxon == "plant" & m_main_effect_plot_data$var == "extreme_dry"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Onset" & m_main_effect_plot_data$taxon == "plant" & m_main_effect_plot_data$var == "extreme_cold"] = "N"

m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Offset" & m_main_effect_plot_data$taxon == "insect" & m_main_effect_plot_data$var == "temp_seasonality"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Offset" & m_main_effect_plot_data$taxon == "plant" & m_main_effect_plot_data$var == "temp_seasonality"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Offset" & m_main_effect_plot_data$taxon == "insect" & m_main_effect_plot_data$var == "tmean"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Offset" & m_main_effect_plot_data$taxon == "insect" & m_main_effect_plot_data$var == "precip"] = "N"
# m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Offset" & m_main_effect_plot_data$taxon == "insect" & m_main_effect_plot_data$var == "extreme_cold"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Offset" & m_main_effect_plot_data$taxon == "plant" & m_main_effect_plot_data$var == "extreme_cold"] = "N"

m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Duration" & m_main_effect_plot_data$taxon == "insect" & m_main_effect_plot_data$var == "precip"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Duration" & m_main_effect_plot_data$taxon == "plant" & m_main_effect_plot_data$var == "precip"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Duration" & m_main_effect_plot_data$taxon == "insect" & m_main_effect_plot_data$var == "temp_seasonality"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Duration" & m_main_effect_plot_data$taxon == "plant" & m_main_effect_plot_data$var == "precip_seasonality"] = "N"
m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Duration" & m_main_effect_plot_data$taxon == "plant" & m_main_effect_plot_data$var == "extreme_cold"] = "N"
# m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Duration" & m_main_effect_plot_data$taxon == "insect" & m_main_effect_plot_data$var == "urbanization"] = "N"
# m_main_effect_plot_data$sig[m_main_effect_plot_data$phenology == "Duration" & m_main_effect_plot_data$taxon == "plant" & m_main_effect_plot_data$var == "urbanization"] = "N"


m_main_effect_plot_data_sig = dplyr::select(m_main_effect_plot_data, phenology, var) |> 
  distinct() |> 
  mutate(sig_interaction = TRUE)
# after checking the models, the following interactions are not siginificant
# I probably can code it up to automatically extract this information...
# m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Onset" & m_main_effect_plot_data_sig$var == "extreme_warm"] = FALSE
# m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Onset" & m_main_effect_plot_data_sig$var == "urbanization"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Onset" & m_main_effect_plot_data_sig$var == "temp_seasonality"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Onset" & m_main_effect_plot_data_sig$var == "precip_seasonality"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Onset" & m_main_effect_plot_data_sig$var == "tmean"] = FALSE

m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Offset" & m_main_effect_plot_data_sig$var == "temp_seasonality"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Offset" & m_main_effect_plot_data_sig$var == "precip_seasonality"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Offset" & m_main_effect_plot_data_sig$var == "extreme_warm"] = FALSE
# m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Offset" & m_main_effect_plot_data_sig$var == "extreme_cold"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Offset" & m_main_effect_plot_data_sig$var == "extreme_wet"] = FALSE

# m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Duration" & m_main_effect_plot_data_sig$var == "extreme_warm"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Duration" & m_main_effect_plot_data_sig$var == "precip"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Duration" & m_main_effect_plot_data_sig$var == "extreme_warm"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Duration" & m_main_effect_plot_data_sig$var == "extreme_cold"] = FALSE
m_main_effect_plot_data_sig$sig_interaction[m_main_effect_plot_data_sig$phenology == "Duration" & m_main_effect_plot_data_sig$var == "extreme_wet"] = FALSE

m_main_effect_plot_data_sig = mutate(m_main_effect_plot_data_sig, 
                                     phenology = factor(phenology, levels = c("Onset", "Offset", "Duration")),
                                     var = factor(var, levels = c("tmean", "precip", "temp_seasonality", "precip_seasonality", # "urbanization",
                                                                  "extreme_warm", "extreme_cold", "extreme_wet", "extreme_dry"))
)

m_main_effect_plot_data = mutate(m_main_effect_plot_data, sig = factor(sig, levels = c("Y", "N")),
                                 phenology = factor(phenology, levels = c("Onset", "Offset", "Duration")),
                                 var = factor(var, levels = c("tmean", "precip", "temp_seasonality", "precip_seasonality",  # "urbanization",
                                                              "extreme_warm", "extreme_cold", "extreme_wet", "extreme_dry"))
)
# 
# p_tradition = ggplot(filter(m_main_effect_plot_data, var %in% c("tmean", "precip", "temp_seasonality", "precip_seasonality")), 
#                      aes(x = x, y = fit)) +
#   geom_ribbon(alpha = 0.2, aes(ymin = lower, ymax = upper, fill = taxon)) +
#   geom_line(aes(color = taxon, linetype = sig), show.legend = F) +
#   geom_text(
#     data = filter(m_main_effect_plot_data_sig, var %in% c("tmean", "precip", "temp_seasonality", "precip_seasonality")),
#     mapping = aes(x = -Inf, y = Inf, label = ifelse(sig_interaction, "*", "")),
#     hjust = -0.1, vjust = 1.2, size = 12) +
#   facet_wrap(~ phenology + var, scales = "free_y", nrow = 3, dir = "h") +
#   labs(x = "Scaled predictors", y = "Fitted values") +
#   cowplot::theme_cowplot() +
#   theme(legend.position = c(0.92, 0.59))
# ggsave("figures/traditional_var.pdf", plot = p_tradition, width = 11, height = 10)
# ggsave("figures/traditional_var.png", plot = p_tradition, width = 11, height = 10)
# 
# p_extreme = ggplot(filter(m_main_effect_plot_data, var %in% c("extreme_warm", "extreme_cold", "extreme_wet", "extreme_dry")),
#                    aes(x = x, y = fit)) +
#   geom_ribbon(alpha = 0.2, aes(ymin = lower, ymax = upper, fill = taxon)) +
#   geom_line(aes(color = taxon, linetype = sig), show.legend = F) +
#   geom_text(
#     data = filter(m_main_effect_plot_data_sig, var %in% c("extreme_warm", "extreme_cold", "extreme_wet", "extreme_dry")),
#     mapping = aes(x = -Inf, y = Inf, label = ifelse(sig_interaction, "*", "")),
#     hjust = -0.1, vjust = 1.2, size = 12) +
#   facet_wrap(~ phenology + var, scales = "free", nrow = 3, dir = "h") +
#   labs(x = "Scaled predictors", y = "Fitted values") +
#   cowplot::theme_cowplot() +
#   theme(legend.position = c(0.92, 0.5))
# ggsave("figures/extreme_var.pdf", plot = p_extreme, width = 11, height = 10)
# ggsave("figures/extreme_var.png", plot = p_extreme, width = 11, height = 10)

# fig s1 -----

de = filter(m_main_effect_plot_data, var %in% c("tmean", "precip", "temp_seasonality", "precip_seasonality"))
de_onset = filter(de, phenology == "Onset")
de_offset = filter(de, phenology == "Offset")
de_dur = filter(de, phenology == "Duration")

p_on = ggplot(de_onset, aes(x = x, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lower, ymax = upper, fill = taxon)) +
  geom_line(aes(color = taxon, linetype = sig)) +
  scale_fill_manual(values = c("indianred3", "blue")) +
  scale_color_manual(values = c("indianred3", "blue")) +
  geom_text(
    data = filter(m_main_effect_plot_data_sig, var %in% c("tmean", "precip", "temp_seasonality", "precip_seasonality"), 
                  phenology == "Onset"),
    mapping = aes(x = -Inf, y = Inf, label = ifelse(sig_interaction, "*", "")),
    hjust = -0.1, vjust = 1.2, size = 12) +
  facet_grid(phenology ~ var) +
  labs(x = "Scaled predictors", y = "Onset (Day of Year)") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none", # axis.ticks.x = element_blank(), 
        # axis.text.x = element_blank(), 
        axis.title.x = element_blank()) 

p_off = ggplot(de_offset, aes(x = x, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lower, ymax = upper, fill = taxon)) +
  geom_line(aes(color = taxon, linetype = sig)) +
  scale_fill_manual(values = c("indianred3", "blue")) +
  scale_color_manual(values = c("indianred3", "blue")) +
  geom_text(
    data = filter(m_main_effect_plot_data_sig, var %in% c("tmean", "precip", "temp_seasonality", "precip_seasonality"), 
                  phenology == "Offset"),
    mapping = aes(x = -Inf, y = Inf, label = ifelse(sig_interaction, "*", "")),
    hjust = -0.1, vjust = 1.2, size = 12) +
  facet_grid(phenology ~ var) +
  labs(x = "Scaled predictors", y = "Offset (Day of Year)", color = "Taxon", fill = "Taxon") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.7, 0.2), # axis.ticks.x = element_blank(), 
        # axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        strip.text.x = element_blank()) +
  guides(linetype = "none")

p_dur = ggplot(de_dur, aes(x = x, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lower, ymax = upper, fill = taxon)) +
  geom_line(aes(color = taxon, linetype = sig), show.legend = F) +
  scale_fill_manual(values = c("indianred3", "blue")) +
  scale_color_manual(values = c("indianred3", "blue")) +
  geom_text(
    data = filter(m_main_effect_plot_data_sig, var %in% c("tmean", "precip", "temp_seasonality", "precip_seasonality"), 
                  phenology == "Duration"),
    mapping = aes(x = -Inf, y = Inf, label = ifelse(sig_interaction, "*", "")),
    hjust = -0.1, vjust = 1.2, size = 12) +
  facet_grid(phenology ~ var) +
  labs(x = "Scaled predictors (mean = 0; sd = 1)", y = "Duration (log number of days)") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none", strip.text.x = element_blank())

p_fig_s1 = cowplot::plot_grid(p_on, p_off, p_dur, align = "v", nrow = 3)
ggsave("figures/fig_s1_traditional_var.pdf", plot = p_fig_s1, width = 11, height = 10)
ggsave("figures/fig_s1_traditional_var.png", plot = p_fig_s1, width = 11, height = 10)


# fig s2 ----

de2 = filter(m_main_effect_plot_data, var %in% c("extreme_warm", "extreme_cold", "extreme_wet", "extreme_dry"))
de2_onset = filter(de2, phenology == "Onset")
de2_offset = filter(de2, phenology == "Offset")
de2_dur = filter(de2, phenology == "Duration")

p_on = ggplot(de2_onset, aes(x = x, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lower, ymax = upper, fill = taxon)) +
  geom_line(aes(color = taxon, linetype = sig)) +
  scale_fill_manual(values = c("indianred3", "blue")) +
  scale_color_manual(values = c("indianred3", "blue")) +
  geom_text(
    data = filter(m_main_effect_plot_data_sig, var %in% c("extreme_warm", "extreme_cold", "extreme_wet", "extreme_dry"), 
                  phenology == "Onset"),
    mapping = aes(x = -Inf, y = Inf, label = ifelse(sig_interaction, "*", "")),
    hjust = -0.1, vjust = 1.2, size = 12) +
  facet_grid(phenology ~ var) +
  labs(x = "Scaled predictors", y = "Onset (Day of Year)") +
  scale_x_continuous(limits = c(-2.2, 4.2), breaks = seq(-2, 4, by = 2)) +
  scale_y_continuous(limits = c(90, 180), breaks = seq(100, 175, by = 25)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none", axis.title.x = element_blank()) 

p_off = ggplot(de2_offset, aes(x = x, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lower, ymax = upper, fill = taxon)) +
  geom_line(aes(color = taxon, linetype = sig)) +
  scale_fill_manual(values = c("indianred3", "blue")) +
  scale_color_manual(values = c("indianred3", "blue")) +
  geom_text(
    data = filter(m_main_effect_plot_data_sig, var %in% c("extreme_warm", "extreme_cold", "extreme_wet", "extreme_dry"), 
                  phenology == "Offset"),
    mapping = aes(x = -Inf, y = Inf, label = ifelse(sig_interaction, "*", "")),
    hjust = -0.1, vjust = 1.2, size = 12) +
  facet_grid(phenology ~ var) +
  labs(x = "Scaled predictors", y = "Offset (Day of Year)", color = "Taxon", fill = "Taxon") +
  scale_x_continuous(limits = c(-2.2, 4.2), breaks = seq(-2, 4, by = 2)) +
  scale_y_continuous(limits = c(140, 260), breaks = seq(140, 220, by = 20)) +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.7, 0.15), 
        axis.title.x = element_blank(),
        strip.text.x = element_blank()) +
  guides(linetype = "none")

p_dur = ggplot(de2_dur, aes(x = x, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lower, ymax = upper, fill = taxon)) +
  geom_line(aes(color = taxon, linetype = sig), show.legend = F) +
  scale_fill_manual(values = c("indianred3", "blue")) +
  scale_color_manual(values = c("indianred3", "blue")) +
  geom_text(
    data = filter(m_main_effect_plot_data_sig, var %in% c("extreme_warm", "extreme_cold", "extreme_wet", "extreme_dry"), 
                  phenology == "Duration"),
    mapping = aes(x = -Inf, y = Inf, label = ifelse(sig_interaction, "*", "")),
    hjust = -0.1, vjust = 1.2, size = 12) +
  facet_grid(phenology ~ var, scales = "free") +
  labs(x = "Scaled predictors (mean = 0; sd = 1)", y = "Duration (log number of days)") +
  scale_x_continuous(limits = c(-2.2, 4.2), breaks = seq(-2, 4, by = 2)) +
  scale_y_continuous(limits = c(3.4, 4.5), breaks = c(3.5, 4, 4.5)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none", strip.text.x = element_blank())

p_fig_s2 = cowplot::plot_grid(p_on, p_off, p_dur, align = "v", nrow = 3)

ggsave("figures/fig_s2_extreme_var.pdf", plot = p_fig_s2, width = 11, height = 10)
ggsave("figures/fig_s2_extreme_var.png", plot = p_fig_s2, width = 11, height = 10)



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
onset_p1 = ggplot(d1_onset, aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                                fill = `Extreme dry`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Extreme dry`), linewidth = line_linewidth) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "", x = "Number of extreme warm days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.55, 0.8)) 
# add obs data?
ggplot() +
  geom_point(data = unique(m_onset_final@frame[, c("onset", "prop_extreme_warm_days_before_onset_logit", "taxon")]) |> 
               rename(`Extreme warm` = "prop_extreme_warm_days_before_onset_logit") |> 
               mutate(`Extreme warm` = `Extreme warm` * filter(d2_ave_std, var == "prop_extreme_warm_days_before_onset_logit")$std +
                        filter(d2_ave_std, var == "prop_extreme_warm_days_before_onset_logit")$ave,
                      `Extreme warm` = logit_back_to_prop(`Extreme warm`),
                      taxon = rtrees:::cap_first_letter(taxon)),
             aes(x = `Extreme warm`, y = onset), inherit.aes = F, alpha = 0.3
) + facet_wrap(~taxon) +
  geom_ribbon(data = d1_onset, aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                                   fill = `Extreme dry`, ymin = lower, ymax = upper), 
              alpha = ribbon_alpha) +
  geom_line(data = d1_onset, aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                                 fill = `Extreme dry`, color = `Extreme dry`), 
            linewidth = line_linewidth) +
  # facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.8, 0.8)) 


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
onset_p2 = ggplot(d2_onset, aes(x = `Extreme wet`, y = fit, group = Precip.,
                                fill = Precip.)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = Precip., linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "", x = "Number of extreme wet days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.51, 0.8), legend.box = "horizontal") 

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
onset_p3 = ggplot(d3_onset, aes(x = `Extreme dry`, y = fit, group = Precip.,
                                fill = Precip.)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = Precip., linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75)) +
  labs(y = "Onset (day of year)", x = "Number of extreme dry days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.51, 0.8), legend.box = "horizontal") 

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
onset_p4 = ggplot(d4_onset, aes(x = `Extreme cold`, y = fit, group = Temp.,
                                fill = Temp.)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = Temp., linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Onset (day of year)", x = "Number of extreme cold days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.51, 0.87), legend.box = "horizontal") 

# no diff between insect and plant, not plotting it?
plot_model(m_onset_final, type = "pred", terms = c("prop_extreme_warm_days_before_onset_logit", "tmean_5_yr_ave"), show.data = F)
d5_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_before_onset_logit", 
                                                              "tmean_5_yr_ave"), 
                                         mod = m_onset_final, se = T, 
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
x_insect = confint(m_onset_final, level = 0.95, method = "Wald", parm = val)
x_plant = confint(m_onset_final_2, level = 0.95, method = "Wald", parm = val)

x_insect_2 = broom::tidy(m_onset_final) |> 
  filter(effect == "fixed", term %in% val) |> 
  dplyr::select(-effect, -group) |> 
  left_join(rownames_to_column(as.data.frame(x_insect), "term"), by = "term") |> 
  left_join(rownames_to_column(as.data.frame(confint(m_onset_final, level = 0.85, method = "Wald", parm = val)), "term"), by = "term") |> 
  mutate(taxon = "Insect")

x_plant_2 = broom::tidy(m_onset_final_2) |> 
  filter(effect == "fixed", term %in% val) |> 
  dplyr::select(-effect, -group) |> 
  left_join(rownames_to_column(as.data.frame(x_plant), "term"), by = "term") |> 
  left_join(rownames_to_column(as.data.frame(confint(m_onset_final_2, level = 0.85, method = "Wald", parm = val)), "term"), by = "term") |> 
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
                       "Precip:Extreme_wet" = "Precip:Extreme_wet (*)", 
                       "Precip:Extreme_dry" = "Precip:Extreme_dry (*)", 
                       "Extreme_warm:Extreme_dry" = "Extreme_warm:Extreme_dry (*)")) |> 
  mutate(term = factor(term, levels = c(
    "Temp:Extreme_warm", "Extreme_warm:Extreme_dry (*)", "Precip:Extreme_dry (*)",
    "Precip:Extreme_wet (*)", "Temp:Extreme_cold (*)"
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
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')

# onset_pall = cowplot::plot_grid(onset_p3, onset_p2, onset_p1, onset_p4, nrow = 2, labels = "AUTO")
ggsave("figures/fig_3_onset_prop.pdf", plot = onset_pall, width = 15, height = 9.5)
ggsave("figures/fig_3_onset_prop.png", plot = onset_pall, width = 15, height = 9.5)

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
offset_p1 = ggplot(d1_offset, aes(x = `Extreme cold`, y = fit, group = `Temp.`,
                                  fill = `Temp.`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  # geom_line(aes(linetype = sig), linewidth = line_linewidth, show.legend = FALSE) +
  geom_line(aes(color = Temp., linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Offset (day of year)", x = "Proportion of extreme cold days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.51, 0.83), legend.box = "horizontal") 

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
offset_p2 = ggplot(d2_offset, aes(x = `Extreme wet`, y = fit, group = Precip.,
                                  fill = Precip.)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = Precip.), linewidth = line_linewidth) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "", x = "Proportion of extreme wet days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.1, 0.85)) 

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
offset_p3 = ggplot(d3_offset, aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                                  fill = `Extreme dry`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  # geom_line(aes(linetype = sig), linewidth = line_linewidth, show.legend = FALSE) +
  geom_line(aes(color = `Extreme dry`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
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
  theme(legend.position = c(0.53, 0.8), legend.box = "horizontal")

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
offset_p4 = ggplot(d4_offset, aes(x = `Extreme warm`, y = fit, group = `Extreme wet`,
                                  fill = `Extreme wet`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Extreme wet`), linewidth = line_linewidth) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Offset (day of year)", x = "Proportion of extreme warm days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.1, 0.86)) 

# plot model coef
val = c(
  # "tmean_5_yr_ave", "temp_seasonality_5_yr_ave", "precip_5_yr_ave", 
  #       "precip_seasonality_5_yr_ave", "prop_extreme_warm_days_within_duration_logit",
  #       "prop_extreme_cold_days_within_duration_logit", "prop_extreme_wet_days_within_duration_logit", 
  #       "prop_extreme_dry_days_within_duration_logit",
  # "tmean_5_yr_ave:prop_extreme_warm_days_within_duration_logit",
  "tmean_5_yr_ave:prop_extreme_cold_days_within_duration_logit",
  "precip_5_yr_ave:prop_extreme_wet_days_within_duration_logit",
  # "precip_5_yr_ave:prop_extreme_dry_days_within_duration_logit",
  "prop_extreme_warm_days_within_duration_logit:prop_extreme_dry_days_within_duration_logit",
  "prop_extreme_warm_days_within_duration_logit:prop_extreme_wet_days_within_duration_logit"
)
x_insect = confint(m_offset_final, level = 0.95, method = "Wald", parm = val)
x_plant = confint(m_offset_final_2, level = 0.95, method = "Wald", parm = val)

x_insect_2 = broom::tidy(m_offset_final) |> 
  filter(effect == "fixed", term %in% val) |> 
  dplyr::select(-effect, -group) |> 
  left_join(rownames_to_column(as.data.frame(x_insect), "term"), by = "term") |> 
  left_join(rownames_to_column(as.data.frame(confint(m_offset_final, level = 0.85, method = "Wald", parm = val)), "term"), by = "term") |> 
  mutate(taxon = "Insect")

x_plant_2 = broom::tidy(m_offset_final_2) |> 
  filter(effect == "fixed", term %in% val) |> 
  dplyr::select(-effect, -group) |> 
  left_join(rownames_to_column(as.data.frame(x_plant), "term"), by = "term") |> 
  left_join(rownames_to_column(as.data.frame(confint(m_offset_final_2, level = 0.85, method = "Wald", parm = val)), "term"), by = "term") |> 
  mutate(taxon = "Plant")

x_both_offset = bind_rows(x_insect_2, x_plant_2) |> 
  mutate(term = str_remove_all(term, "prop_"),
         term = str_remove_all(term, "_5_yr_ave"),
         term = str_remove_all(term, "_days_before_offset_logit"),
         term = str_remove_all(term, "_days_within_duration_logit"),
         term = str_remove_all(term, "_days_total_logit"),
         term = str_replace_all(term, pattern = "plant", replacement = "Plant"),
         term = str_replace_all(term, pattern = "extreme", replacement = "Extreme"),
         term = str_replace_all(term, pattern = "tmean", replacement = "Temp"),
         term = str_replace_all(term, pattern = "temp", replacement = "Temp"),
         term = str_replace_all(term, pattern = "precip", replacement = "Precip")) |> 
  mutate(term = recode(term, "Temp:Extreme_cold" = "Temp:Extreme_cold (*)", 
                       # "Precip:Extreme_wet" = "Precip:Extreme_wet (*)", 
                       # "Precip:Extreme_dry" = "Precip:Extreme_dry (*)", 
                       "Extreme_warm:Extreme_dry" = "Extreme_warm:Extreme_dry (*)")) |> 
  mutate(term = factor(term, levels = c("Precip:Extreme_wet", "Extreme_warm:Extreme_wet",
                                        "Extreme_warm:Extreme_dry (*)", "Temp:Extreme_cold (*)")))

p_coef_final_offset = ggplot(x_both_offset, aes(x = estimate, y = term, color = taxon)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(position = position_dodge(width = 0.3), 
                  aes(xmin = `2.5 %`, xmax = `97.5 %`), 
                  alpha = ifelse(x_both_offset$p.value < 0.05, 1, 0.2)) +
  geom_linerange(position = position_dodge(width = 0.3), 
                 aes(xmin = `7.5 %`, xmax = `92.5 %`), 
                 linewidth = 2, 
                 alpha = ifelse(x_both_offset$p.value < 0.05, 1, 0.2)) +
  labs(x = "Offset Coefficients", y = "", color = "Taxon") +
  # scale_x_continuous(breaks = c(-15, -10, -5, 0, 5)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "top") 

layout <- "
ABBCC
ADDEE
"
offset_pall = p_coef_final_offset + offset_p1 + offset_p3 + offset_p4 + offset_p2 +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')

ggsave("figures/fig_4_offset_prop.pdf", plot = offset_pall, width = 15, height = 9.5)
ggsave("figures/fig_4_offset_prop.png", plot = offset_pall, width = 15, height = 9.5)

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
duration_p1 = ggplot(d1_duration, aes(x = `Extreme wet`, y = fit, group = `Precip.`,
                                      fill = `Precip.`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = Precip., linetype = `Sig. Inter.`), linewidth = line_linewidth) +
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
  theme(legend.position = c(0.52, 0.8), legend.box = "horizontal") 


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
duration_p2 = ggplot(d2_duration, aes(x = `Extreme dry`, y = fit, group = `Precip.`,
                                      fill = `Precip.`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = Precip., linetype = `Sig. Inter.`), linewidth = line_linewidth) +
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
  theme(legend.position = c(0.52, 0.8), legend.box = "horizontal") 

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
duration_p3 = ggplot(d3_duration, aes(x = `Extreme warm`, y = fit, group = `Extreme wet`,
                                      fill = `Extreme wet`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Extreme wet`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
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
  theme(legend.position = c(0.52, 0.8), legend.box = "horizontal") 

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
duration_p4 = ggplot(d4_duration, aes(x = `Extreme warm`, y = fit, group = `Temp.`,
                                      fill = `Temp.`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Temp.`), linewidth = line_linewidth) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Duration (number of days)",
       x = "Proportion of extreme warm days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.1, 0.9), legend.box = "horizontal") 

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
duration_p5 = ggplot(d5_duration, aes(x = `Extreme cold`, y = fit, group = `Temp.`,
                                      fill = `Temp.`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Temp.`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~taxon) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "", x = "Proportion of extreme cold days") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.52, 0.8), legend.box = "horizontal") 

duration_p4 # no diff between plant and insect, not plotting it?

# plot model coefs
val = c(
  # "tmean_5_yr_ave", "temp_seasonality_5_yr_ave", "precip_5_yr_ave", 
  #       "precip_seasonality_5_yr_ave", "prop_extreme_warm_days_within_duration_logit",
  #       "prop_extreme_cold_days_within_duration_logit", "prop_extreme_wet_days_within_duration_logit", 
  #       "prop_extreme_dry_days_within_duration_logit",
  "tmean_5_yr_ave:prop_extreme_warm_days_total_logit",
  "tmean_5_yr_ave:prop_extreme_cold_days_total_logit",
  "precip_5_yr_ave:prop_extreme_wet_days_total_logit",
  "precip_5_yr_ave:prop_extreme_dry_days_total_logit"
  # "prop_extreme_warm_days_within_duration_logit:prop_extreme_dry_days_within_duration_logit",
  # "prop_extreme_warm_days_within_duration_logit:prop_extreme_wet_days_within_duration_logit"
)
x_insect = confint(m_duration_final, level = 0.95, method = "Wald", parm = val)
x_plant = confint(m_duration_final_2, level = 0.95, method = "Wald", parm = val)

x_insect_2 = broom::tidy(m_duration_final) |> 
  filter(effect == "fixed", term %in% val) |> 
  dplyr::select(-effect, -group) |> 
  left_join(rownames_to_column(as.data.frame(x_insect), "term"), by = "term") |> 
  left_join(rownames_to_column(as.data.frame(confint(m_duration_final, level = 0.85, method = "Wald", parm = val)), "term"), by = "term") |> 
  mutate(taxon = "Insect")

x_plant_2 = broom::tidy(m_duration_final_2) |> 
  filter(effect == "fixed", term %in% val) |> 
  dplyr::select(-effect, -group) |> 
  left_join(rownames_to_column(as.data.frame(x_plant), "term"), by = "term") |> 
  left_join(rownames_to_column(as.data.frame(confint(m_duration_final_2, level = 0.85, method = "Wald", parm = val)), "term"), by = "term") |> 
  mutate(taxon = "Plant")

x_both_duration = bind_rows(x_insect_2, x_plant_2) |> 
  mutate(term = str_remove_all(term, "prop_"),
         term = str_remove_all(term, "_5_yr_ave"),
         term = str_remove_all(term, "_days_before_duration_logit"),
         term = str_remove_all(term, "_days_within_duration_logit"),
         term = str_remove_all(term, "_days_total_logit"),
         term = str_replace_all(term, pattern = "plant", replacement = "Plant"),
         term = str_replace_all(term, pattern = "extreme", replacement = "Extreme"),
         term = str_replace_all(term, pattern = "tmean", replacement = "Temp"),
         term = str_replace_all(term, pattern = "temp", replacement = "Temp"),
         term = str_replace_all(term, pattern = "precip", replacement = "Precip")) |> 
  mutate(term = recode(term, "Temp:Extreme_cold" = "Temp:Extreme_cold (*)", 
                       "Precip:Extreme_wet" = "Precip:Extreme_wet (*)",
                       "Precip:Extreme_dry" = "Precip:Extreme_dry (*)"
                       # "Extreme_warm:Extreme_dry" = "Extreme_warm:Extreme_dry (*)"
  )) 

p_coef_final_duration = ggplot(x_both_duration, aes(x = estimate, y = term, color = taxon)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(position = position_dodge(width = 0.3), 
                  aes(xmin = `2.5 %`, xmax = `97.5 %`), 
                  alpha = ifelse(x_both_duration$p.value < 0.05, 1, 0.2)) +
  geom_linerange(position = position_dodge(width = 0.3), 
                 aes(xmin = `7.5 %`, xmax = `92.5 %`), 
                 linewidth = 2, 
                 alpha = ifelse(x_both_duration$p.value < 0.05, 1, 0.2)) +
  labs(x = "Duration Coefficients", y = "", color = "Taxon") +
  # scale_x_continuous(breaks = c(-15, -10, -5, 0, 5)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "top") 


layout <- "
ABBCC
ADDEE
"
duration_pall = p_coef_final_duration + duration_p4 + duration_p5 + duration_p1 + duration_p2 +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
ggsave("figures/fig_5_duration_prop.pdf", plot = duration_pall, width = 15, height = 9.5)
ggsave("figures/fig_5_duration_prop.png", plot = duration_pall, width = 15, height = 9.5)

# Fig S plant onset ~ traits -----
g_levels = c(-1.5, 0, 1.5)
ribbon_alpha = 0.15
legend_position = c(0.8, 0.7)
line_linewidth = 1.5
color_values = c("#75D054FF", "#2A788EFF", "#414487FF")
d1_plant_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_before_onset_logit", "prop_extreme_dry_days_before_onset_logit", "woodiness"), 
                                               mod = m_onset_plant_final, se = T, 
                                               xlevels = list(prop_extreme_warm_days_before_onset_logit = 20, 
                                                              prop_extreme_dry_days_before_onset_logit = g_levels))) |> 
  rename(`Extreme warm` = prop_extreme_warm_days_before_onset_logit,
         `Extreme dry` = prop_extreme_dry_days_before_onset_logit) |> 
  mutate(`Extreme dry` = as.factor(`Extreme dry`),
         woodiness = rtrees:::cap_first_letter(woodiness),
         `Sig. Inter.` = ifelse(woodiness == "Woody", "No", "Yes"))
onset_plant_p1 = ggplot(d1_plant_onset, aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                                            fill = `Extreme dry`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Extreme dry`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~woodiness) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Onset") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.1, 0.23), legend.box = "horizontal") 

d2_plant_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_before_onset_logit", "tmean_5_yr_ave", "woodiness"), 
                                               mod = m_onset_plant_final, se = T, 
                                               xlevels = list(prop_extreme_warm_days_before_onset_logit = 20, 
                                                              tmean_5_yr_ave = g_levels))) |> 
  rename(`Temp.` = tmean_5_yr_ave,
         `Extreme warm` = prop_extreme_warm_days_before_onset_logit) |> 
  mutate(`Temp.` = as.factor(`Temp.`),
         woodiness = rtrees:::cap_first_letter(woodiness),
         `Sig. Inter.` = ifelse(woodiness == "Woody", "No", "Yes"))
onset_plant_p2 = ggplot(d2_plant_onset, aes(x = `Extreme warm`, y = fit, group = `Temp.`,
                                            fill = `Temp.`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Temp.`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~woodiness) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Onset") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.1, 0.2), legend.box = "horizontal") 

onset_pall_plant = cowplot::plot_grid(onset_plant_p2, onset_plant_p1, nrow = 1, labels = "AUTO")
ggsave("figures/fig_S_onset_plant.pdf", plot = onset_pall_plant, width = 11, height = 5)
ggsave("figures/fig_S_onset_plant.png", plot = onset_pall_plant, width = 11, height = 5)

# Fig S plant offset ~ traits -----
d1_plant_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_within_duration_logit", "precip_5_yr_ave", "woodiness"), 
                                                mod = m_offset_plant_final, se = T, 
                                                xlevels = list(prop_extreme_wet_days_within_duration_logit = 20, 
                                                               precip_5_yr_ave = g_levels))) |> 
  rename(`Extreme wet` = prop_extreme_wet_days_within_duration_logit,
         `Precip` = precip_5_yr_ave) |> 
  mutate(`Precip` = as.factor(`Precip`),
         woodiness = rtrees:::cap_first_letter(woodiness),
         `Sig. Inter.` = ifelse(woodiness == "Woody", "No", "Yes"))
offset_plant_p1 = ggplot(d1_plant_offset, aes(x = `Extreme wet`, y = fit, group = `Precip`,
                                              fill = `Precip`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Precip`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~woodiness) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Offset") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.05, 0.2), legend.box = "horizontal") 


# d2_plant_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_within_duration_logit", "precip_5_yr_ave", "woodiness"), 
#                                                mod = m_offset_plant_final, se = T, 
#                                                xlevels = list(prop_extreme_wet_days_within_duration_logit = 20, 
#                                                               precip_5_yr_ave = g_levels))) |> 
#   rename(`Precip` = precip_5_yr_ave,
#          `Extreme wet` = prop_extreme_wet_days_within_duration_logit) |> 
#   mutate(`Precip` = as.factor(`Precip`),
#          woodiness = rtrees:::cap_first_letter(woodiness),
#          `Sig. Inter.` = ifelse(woodiness == "Climber", "No", "Yes"))
# offset_plant_p2 = ggplot(d2_plant_offset, aes(x = `Extreme wet`, y = fit, group = `Precip`,
#                                             fill = `Precip`)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
#   geom_line(aes(color = `Precip`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
#   scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
#   facet_wrap(~woodiness) +
#   scale_fill_manual(values = color_values) +
#   scale_color_manual(values = color_values) +
#   labs(y = "Offset") +
#   cowplot::theme_cowplot() +
#   theme(legend.position = c(0.4, 0.8), legend.box = "horizontal") 

d3_plant_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_within_duration_logit", 
                                                                     "prop_extreme_wet_days_within_duration_logit", 
                                                                     "woodiness"), 
                                                mod = m_offset_plant_final, se = T, 
                                                xlevels = list(prop_extreme_warm_days_within_duration_logit = 20, 
                                                               prop_extreme_wet_days_within_duration_logit = g_levels))) |> 
  rename(`Extreme wet` = prop_extreme_wet_days_within_duration_logit,
         `Extreme warm` = prop_extreme_warm_days_within_duration_logit) |> 
  mutate(`Extreme wet` = as.factor(`Extreme wet`),
         woodiness = rtrees:::cap_first_letter(woodiness),
         `Sig. Inter.` = ifelse(woodiness == "Woody", "No", "Yes"))
offset_plant_p3 = ggplot(d3_plant_offset, aes(x = `Extreme warm`, y = fit, group = `Extreme wet`,
                                              fill = `Extreme wet`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Extreme wet`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~woodiness) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Offset") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.05, 0.2), legend.box = "horizontal") 

offset_pall_plant = cowplot::plot_grid(offset_plant_p1, offset_plant_p3, nrow = 1, labels = "AUTO")
ggsave("figures/fig_S_offset_plant.pdf", plot = offset_pall_plant, width = 11, height = 5)
ggsave("figures/fig_S_offset_plant.png", plot = offset_pall_plant, width = 11, height = 5)

# Fig S plant duration ~ traits -----
d1_plant_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_dry_days_total_logit", "precip_5_yr_ave", "woodiness"), 
                                                  mod = m_duration_plant_final, se = T, 
                                                  xlevels = list(prop_extreme_dry_days_total_logit = 20, 
                                                                 precip_5_yr_ave = g_levels))) |> 
  rename(`Extreme dry` = prop_extreme_dry_days_total_logit,
         `Precip` = precip_5_yr_ave) |> 
  mutate(`Precip` = as.factor(`Precip`),
         woodiness = rtrees:::cap_first_letter(woodiness))
duration_plant_p1 = ggplot(d1_plant_duration, aes(x = `Extreme dry`, y = fit, group = `Precip`,
                                                  fill = `Precip`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Precip`), linewidth = line_linewidth) +
  facet_wrap(~woodiness) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Duration") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.1, 0.2)) 


d2_plant_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_total_logit", "precip_5_yr_ave", "woodiness"), 
                                                  mod = m_duration_plant_final, se = T, 
                                                  xlevels = list(prop_extreme_wet_days_total_logit = 20, 
                                                                 precip_5_yr_ave = g_levels))) |> 
  rename(`Precip` = precip_5_yr_ave,
         `Extreme wet` = prop_extreme_wet_days_total_logit) |> 
  mutate(`Precip` = as.factor(`Precip`),
         woodiness = rtrees:::cap_first_letter(woodiness),
         `Sig. Inter.` = ifelse(woodiness == "Woody", "No", "Yes"))
duration_plant_p2 = ggplot(d2_plant_duration, aes(x = `Extreme wet`, y = fit, group = `Precip`,
                                                  fill = `Precip`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Precip`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~woodiness) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Duration") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.1, 0.2), legend.box = "horizontal") 

# d3_plant_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_total_logit", 
#                                                                        "prop_extreme_wet_days_total_logit", "woodiness"), 
#                                                 mod = m_duration_plant_final, se = T, 
#                                                 xlevels = list(prop_extreme_warm_days_total_logit = 20, 
#                                                                prop_extreme_wet_days_total_logit = g_levels))) |> 
#   rename(`Extreme wet` = prop_extreme_wet_days_total_logit,
#          `Extreme warm` = prop_extreme_warm_days_total_logit) |> 
#   mutate(`Extreme wet` = as.factor(`Extreme wet`),
#          woodiness = rtrees:::cap_first_letter(woodiness),
#          `Sig. Inter.` = ifelse(woodiness == "Woody", "Yes", "No"))
# duration_plant_p3 = ggplot(d3_plant_duration, aes(x = `Extreme warm`, y = fit, group = `Extreme wet`,
#                                               fill = `Extreme wet`)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
#   geom_line(aes(color = `Extreme wet`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
#   scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
#   facet_wrap(~woodiness) +
#   scale_fill_manual(values = color_values) +
#   scale_color_manual(values = color_values) +
#   labs(y = "Duration") +
#   cowplot::theme_cowplot() +
#   theme(legend.position = c(0.7, 0.83), legend.box = "horizontal") 

d4_plant_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_total_logit", "tmean_5_yr_ave", "woodiness"), 
                                                  mod = m_duration_plant_final, se = T, 
                                                  xlevels = list(prop_extreme_warm_days_total_logit = 20, 
                                                                 tmean_5_yr_ave = g_levels))) |> 
  rename(`Temp` = tmean_5_yr_ave,
         `Extreme warm` = prop_extreme_warm_days_total_logit) |> 
  mutate(`Temp` = as.factor(`Temp`),
         woodiness = rtrees:::cap_first_letter(woodiness),
         `Sig. Inter.` = ifelse(woodiness == "Nonwoody", "Yes", "No"))
duration_plant_p4 = ggplot(d4_plant_duration, aes(x = `Extreme warm`, y = fit, group = `Temp`,
                                                  fill = `Temp`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Temp`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~woodiness) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Duration") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.1, 0.2), legend.box = "horizontal") 

duration_pall_plant = cowplot::plot_grid(duration_plant_p1, duration_plant_p2, duration_plant_p4, nrow = 3, labels = "AUTO")
ggsave("figures/fig_S_duration_plant.pdf", plot = duration_pall_plant, width = 7, height = 11)
ggsave("figures/fig_S_duration_plant.png", plot = duration_pall_plant, width = 7, height = 11)

# Fig S insect onset ~ traits -----
d1_insect_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_before_onset_logit",
                                                                     "prop_extreme_dry_days_before_onset_logit", "overwintering.stage"), 
                                                mod = m_onset_insect_final, se = T, 
                                                xlevels = list(prop_extreme_warm_days_before_onset_logit = 20, 
                                                               prop_extreme_dry_days_before_onset_logit = g_levels))) |> 
  rename(`Extreme warm` = prop_extreme_warm_days_before_onset_logit,
         `Extreme dry` = prop_extreme_dry_days_before_onset_logit) |> 
  mutate(`Extreme dry` = as.factor(`Extreme dry`),
         overwintering.stage = rtrees:::cap_first_letter(overwintering.stage),
         `Sig. Inter.` = ifelse(overwintering.stage == "Egg", "No", "Yes"))
onset_insect_p1 = ggplot(d1_insect_onset, aes(x = `Extreme warm`, y = fit, group = `Extreme dry`,
                                              fill = `Extreme dry`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Extreme dry`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~overwintering.stage) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  # colorspace::scale_color_discrete_divergingx("Zissou 1") +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  labs(y = "Onset") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.7, 0.8), legend.box = "horizontal") 

d2_insect_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_dry_days_before_onset_logit", 
                                                                     "precip_5_yr_ave", "overwintering.stage"), 
                                                mod = m_onset_insect_final, se = T, 
                                                xlevels = list(prop_extreme_dry_days_before_onset_logit = 20, 
                                                               precip_5_yr_ave = g_levels))) |> 
  rename(`Precip` = precip_5_yr_ave,
         `Extreme dry` = prop_extreme_dry_days_before_onset_logit) |> 
  mutate(`Precip` = as.factor(`Precip`),
         overwintering.stage = rtrees:::cap_first_letter(overwintering.stage),
         `Sig. Inter.` = ifelse(overwintering.stage == "Larvae", "Yes", "No"))
onset_insect_p2 = ggplot(d2_insect_onset, aes(x = `Extreme dry`, y = fit, group = `Precip`,
                                              fill = `Precip`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Precip`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~overwintering.stage) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Onset") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.7, 0.8), legend.box = "horizontal") 

d3_insect_onset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_before_onset_logit", 
                                                                     "precip_5_yr_ave", "overwintering.stage"), 
                                                mod = m_onset_insect_final, se = T, 
                                                xlevels = list(prop_extreme_wet_days_before_onset_logit = 20, 
                                                               precip_5_yr_ave = g_levels))) |> 
  rename(`Precip` = precip_5_yr_ave,
         `Extreme wet` = prop_extreme_wet_days_before_onset_logit) |> 
  mutate(`Precip` = as.factor(`Precip`),
         overwintering.stage = rtrees:::cap_first_letter(overwintering.stage),
         `Sig. Inter.` = ifelse(overwintering.stage == "Larvae", "Yes", "No"))
onset_insect_p3 = ggplot(d3_insect_onset, aes(x = `Extreme wet`, y = fit, group = `Precip`,
                                              fill = `Precip`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Precip`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~overwintering.stage) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Onset") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.7, 0.8), legend.box = "horizontal") 


onset_pall_insect = cowplot::plot_grid( onset_insect_p1, onset_insect_p3, onset_insect_p2, nrow = 3, labels = "AUTO")
ggsave("figures/fig_S_onset_insect.pdf", plot = onset_pall_insect, width = 11, height = 11)
ggsave("figures/fig_S_onset_insect.png", plot = onset_pall_insect, width = 11, height = 11)

# Fig S insect offset ~ traits -----
# no significant differences
# d1_insect_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_within_duration_logit", "tmean_5_yr_ave", "overwintering.stage"), 
#                                                 mod = m_offset_insect_final, se = T, 
#                                                 xlevels = list(prop_extreme_cold_days_within_duration_logit = 20, 
#                                                                tmean_5_yr_ave = g_levels))) |> 
#   rename(`Extreme cold` = prop_extreme_cold_days_within_duration_logit,
#          `Temp` = tmean_5_yr_ave) |> 
#   mutate(`Temp` = as.factor(`Temp`),
#          overwintering.stage = rtrees:::cap_first_letter(overwintering.stage),
#          `Sig. Inter.` = ifelse(overwintering.stage == "Pupae", "Yes", "No"))
# offset_insect_p1 = ggplot(d1_insect_offset, aes(x = `Extreme cold`, y = fit, group = `Temp`,
#                                               fill = `Temp`)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
#   geom_line(aes(color = `Temp`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
#   scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
#   facet_wrap(~overwintering.stage) +
#   scale_fill_manual(values = color_values) +
#   scale_color_manual(values = color_values) +
#   # colorspace::scale_color_discrete_divergingx("Zissou 1") +
#   # scale_fill_viridis_d() +
#   # scale_color_viridis_d() +
#   labs(y = "Offset") +
#   cowplot::theme_cowplot() +
#   theme(legend.position = c(0.7, 0.8), legend.box = "horizontal") 
# 
# 
# d3_insect_offset = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_wet_days_within_duration_logit", "precip_5_yr_ave", "overwintering.stage"), 
#                                                 mod = m_offset_insect_final, se = T, 
#                                                 xlevels = list(prop_extreme_wet_days_within_duration_logit = 20, 
#                                                                precip_5_yr_ave = g_levels))) |> 
#   rename(`Precip` = precip_5_yr_ave,
#          `Extreme wet` = prop_extreme_wet_days_within_duration_logit) |> 
#   mutate(`Precip` = as.factor(`Precip`),
#          overwintering.stage = rtrees:::cap_first_letter(overwintering.stage),
#          `Sig. Inter.` = ifelse(overwintering.stage == "Egg", "No", "Yes"))
# offset_insect_p3 = ggplot(d3_insect_offset, aes(x = `Extreme wet`, y = fit, group = `Precip`,
#                                               fill = `Precip`)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
#   geom_line(aes(color = `Precip`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
#   scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
#   facet_wrap(~overwintering.stage) +
#   scale_fill_manual(values = color_values) +
#   scale_color_manual(values = color_values) +
#   labs(y = "Offset") +
#   cowplot::theme_cowplot() +
#   theme(legend.position = c(0.7, 0.8), legend.box = "horizontal") 
# 
# 
# offset_pall_insect = cowplot::plot_grid(offset_insect_p1, offset_insect_p3, nrow = 2, labels = "AUTO")
# ggsave("figures/fig_S_offset_insect.pdf", plot = offset_pall_insect, width = 11, height = 9)
# ggsave("figures/fig_S_offset_insect.png", plot = offset_pall_insect, width = 11, height = 9)

# Fig S Insect duration ~ traits ----
d1_insect_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_cold_days_total_logit",
                                                                        "tmean_5_yr_ave", "overwintering.stage"), 
                                                   mod = m_duration_insect_final, se = T, 
                                                   xlevels = list(prop_extreme_cold_days_total_logit = 20, 
                                                                  tmean_5_yr_ave = g_levels))) |> 
  rename(`Temp.` = tmean_5_yr_ave,
         `Extreme cold` = prop_extreme_cold_days_total_logit) |> 
  mutate(`Temp.` = as.factor(`Temp.`),
         overwintering.stage = rtrees:::cap_first_letter(overwintering.stage),
         `Sig. Inter.` = ifelse(overwintering.stage == "Pupae", "Yes", "No"))
duration_insect_p1 = ggplot(d1_insect_duration, aes(x = `Extreme cold`, y = fit, group = `Temp.`,
                                                    fill = `Temp.`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Temp.`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~overwintering.stage) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Duration") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.05, 0.85), legend.box = "horizontal") 

d2_insect_duration = as.data.frame(effects::Effect(focal.predictors = c("prop_extreme_warm_days_total_logit",
                                                                        "tmean_5_yr_ave", "overwintering.stage"), 
                                                   mod = m_duration_insect_final, se = T, 
                                                   xlevels = list(prop_extreme_warm_days_total_logit = 20, 
                                                                  tmean_5_yr_ave = g_levels))) |> 
  rename(`Temp.` = tmean_5_yr_ave,
         `Extreme warm` = prop_extreme_warm_days_total_logit) |> 
  mutate(`Temp.` = as.factor(`Temp.`),
         overwintering.stage = rtrees:::cap_first_letter(overwintering.stage),
         `Sig. Inter.` = ifelse(overwintering.stage == "Larvae", "Yes", "No"))
duration_insect_p2 = ggplot(d2_insect_duration, aes(x = `Extreme warm`, y = fit, group = `Temp.`,
                                                    fill = `Temp.`)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha) +
  geom_line(aes(color = `Temp.`, linetype = `Sig. Inter.`), linewidth = line_linewidth) +
  scale_linetype_manual(values = c("No" = 4, "Yes" = 1)) +
  facet_wrap(~overwintering.stage) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  labs(y = "Duration") +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.05, 0.85), legend.box = "horizontal") 

duration_pall_insect = cowplot::plot_grid(duration_insect_p1, duration_insect_p2, nrow = 2, labels = "AUTO")
ggsave("figures/fig_S_duration_insect.pdf", plot = duration_pall_insect, width = 11, height = 9)
ggsave("figures/fig_S_duration_insect.png", plot = duration_pall_insect, width = 11, height = 9)


