

ggplot(monthly_fresh_df) + 
  geom_linerange(aes(ymin = q_low,
                     ymax = h_low, x = latitude), color = "lightgreen", alpha = .6, linewidth = 1) +
  geom_point(aes(x = latitude, y = temp_mean)) +
  labs(x = "Latitude", y = "Water Temperature (1982-2025 month averages)") +
  theme_classic()

ggplot(monthly_fresh_thresholded_df) +
  geom_linerange(aes(ymin = q_low,
                     ymax = q_high, x = latitude), color = "lightgreen", alpha = .6, linewidth = 1) +
  geom_point(aes(x = latitude, y = temp_mean)) +
  labs(x = "Latitude", y = "Water Temperature (1982-2025 month averages)") +
  theme_classic()

monthly_fresh_thresholded_df_coarse <- monthly_fresh_thresholded_df %>%
  mutate(lat_bin = cut(latitude, breaks = seq(floor(min(latitude)),
                                              ceiling(max(latitude)),
                                              by = 1))) %>%
  group_by(lat_bin) %>%
  summarise(
    latitude = mean(latitude, na.rm = TRUE),
    mean_temp = mean(temp_mean, na.rm = TRUE),
    median_temp = median(temp_median, na.rm = TRUE),
    max_temp = max(temp_max, na.rm = TRUE),
    min_temp = min(temp_min, na.rm = TRUE),
    low_q = quantile(q_low, probs = 0.025, na.rm = TRUE),
    high_q = quantile(q_high, probs = 0.975, na.rm = TRUE))

ggplot(monthly_fresh_thresholded_df_coarse) +
  geom_linerange(aes(ymin = low_q,
                     ymax = high_q, x = latitude), color = "lightgreen", alpha = .6, linewidth = 1) +
  geom_point(aes(x = latitude, y = mean_temp)) +
  labs(x = "Latitude", y = "Water Temperature (1982-2025 month averages)") +
  theme_classic()
fitted_datasets <- readRDS(here("processed-data", "sorted_datasets_withparams.RDS"))
ggplot(monthly_fresh_thresholded_df_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = median_temp, ymax = high_q), fill = "lightgreen", alpha = .6, linewidth = 1.2) +
  geom_line(aes (y = median_temp), color = "darkgreen", size = 2) +
  geom_point(data = fitted_datasets %>%
               filter(land_or_sea == "terrestrial") %>%
               filter(topt_TF == TRUE), aes(x = latitude, y = topt), color = "black", alpha = .4) +
  labs(x = "latitude", y = "water temperature") +
  theme_classic()
ggplot(monthly_fresh_thresholded_df_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = mean_temp, ymax = high_q), fill = "lightgreen", alpha = .6, linewidth = 1.2) +
  geom_line(aes (y = mean_temp), color = "darkgreen", size = 2) +
  geom_point(data = fitted_datasets %>%
               filter(land_or_sea == "terrestrial") %>%
               filter(topt_TF == TRUE), aes(x = latitude, y = topt), color = "black", alpha = .4) +
  labs(x = "latitude", y = "water temperature") +
  theme_classic()

freshwater_to_save <- monthly_fresh_df %>%
  select(latitude, longitude, temp_mean, temp_sd, temp_median, temp_min, temp_max, temp_range)




