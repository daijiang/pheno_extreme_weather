library(tidyverse)

d = readRDS("data_output/d_raw_for_stat.rds")
names(d)
d_hist = dplyr::select(d, yr, n_extreme_cold_days_before_onset, n_extreme_warm_days_before_onset, 
              n_extreme_wet_days_before_onset, n_extreme_dry_days_before_onset,
              n_extreme_cold_days_within_duration, n_extreme_warm_days_within_duration,
              n_extreme_wet_days_within_duration, n_extreme_dry_days_within_duration,
              n_extreme_cold_days_total, n_extreme_warm_days_total,
              n_extreme_wet_days_total, n_extreme_dry_days_total, duration) |> 
  mutate(prop_extreme_cold_days_within_duration  = n_extreme_cold_days_within_duration / (duration + 1),
         prop_extreme_warm_days_within_duration = n_extreme_warm_days_within_duration / (duration + 1),
         prop_extreme_wet_days_within_duration  = n_extreme_wet_days_within_duration / (duration + 1),
         prop_extreme_dry_days_within_duration = n_extreme_dry_days_within_duration / (duration + 1),
         prop_extreme_cold_days_total  = n_extreme_cold_days_total / (duration + 62),
         prop_extreme_warm_days_total = n_extreme_warm_days_total / (duration + 62),
         prop_extreme_wet_days_total  = n_extreme_wet_days_total / (duration + 62),
         prop_extreme_dry_days_total = n_extreme_dry_days_total / (duration + 62))
distinct(d_hist)

d_hist = distinct(d_hist)
d_hist = dplyr::select(d_hist, n_extreme_cold_days_before_onset, n_extreme_warm_days_before_onset, 
                       n_extreme_wet_days_before_onset, n_extreme_dry_days_before_onset,
                       starts_with("prop"))

d_hist = pivot_longer(d_hist, cols = 1:12, names_to = "var", values_to = "value") |> 
  mutate(var2 = ifelse(grepl("onset", var), "Onset",
                       ifelse(grepl("duration", var), "Offset", "Duration")),
         var = str_extract(var, "extreme_[a-z]{3,4}"),
         var = rtrees:::cap_first_letter(var))
d_hist = filter(d_hist, value <= 60) |> 
  mutate(var = factor(var, levels = c("Extreme_cold", "Extreme_warm", "Extreme_dry", "Extreme_wet")))

p1 = ggplot(filter(d_hist, var2 == "Onset"), aes(x = value)) +
  geom_histogram() +
  scale_y_log10() +
  facet_grid(var2 ~ var, scales = "fixed") +
  labs(x = "Number of extreme days within the 60-day window before onset",
       y = "Count (log_10 scale)")

p2 = ggplot(filter(d_hist, var2 == "Offset"), aes(x = value)) +
  geom_histogram() +
  scale_y_log10() +
  facet_grid(var2 ~ var, scales = "fixed") +
  labs(x = "Proportion of extreme days",
       y = "Count (log_10 scale)")

p3 = ggplot(filter(d_hist, var2 == "Duration"), aes(x = value)) +
  geom_histogram() +
  scale_y_log10() +
  facet_grid(var2 ~ var, scales = "fixed") +
  labs(x = "Proportion of extreme days",
       y = "Count (log_10 scale)")

library(patchwork)

p_hist = p1 + p2 + p3 + plot_layout(ncol = 1)

ggsave("figures/fig_s_hist_ewe.pdf", plot = p_hist, width = 9, height = 7)
ggsave("figures/fig_s_hist_ewe.png", plot = p_hist, width = 9, height = 7)


sp_plt = unique(filter(d, taxon == "plant")$sp)
sp_plt_df = rtrees::sp_list_df(sp_plt, "plant")
paste(count(sp_plt_df, family, sort = T)$family[1:10], collapse = ", ")
