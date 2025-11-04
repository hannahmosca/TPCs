rm(list=ls())
library(here)
library(dplyr)
library(tidyverse)
library(ggplot2)
marine_sst_points <- readRDS(here("processed-data", "marine_sst_raw_temp.RDS"))
marine_sst_all_temps <- readRDS(here("processed-data", "marine_sst_all_temp.RDS"))
marine_sst_all_temps <- marine_sst_all_temps %>%
  rename(longitude = x) %>%
  rename(latitude = y)
install.packages("matrixStats")
library(matrixStats)
# Select numeric temperature columns
temp_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(marine_sst_all_temps), value = TRUE)
temp_matrix <- as.matrix(marine_sst_all_temps[, temp_cols])


marine_sst_all_temps$sst_mean   <- rowMeans(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_sd     <- rowSds(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_median <- rowMedians(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_min    <- rowMins(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_max    <- rowMaxs(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_range  <- marine_sst_all_temps$sst_max - marine_sst_all_temps$sst_min
marine_sst_all_temps$q_low <- rowQuantiles(temp_matrix, probs = 0.025, na.rm = TRUE)
marine_sst_all_temps$q_high <- rowQuantiles(temp_matrix, probs = 0.975, na.rm = TRUE)

marine_sst_points <- marine_sst_points %>%
  mutate(abs_lat = abs(latitude))

ggplot(marine_sst_all_temps, aes(x = latitude, y = sst_mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = sst_mean - sst_sd,
                  ymax = sst_mean + sst_sd),
              alpha = 0.1) +
  labs(x = "Latitude", y = "Sea Surface Temperature (°C)",
       title = "Mean SST by Latitude with SD shading") +
  theme_minimal()

ggplot(marine_sst_points, aes(x = latitude, y = sst_mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = sst_mean - sst_sd,
                  ymax = sst_mean + sst_sd),
              alpha = 0.1) +
  labs(x = "Latitude", y = "Sea Surface Temperature (°C)",
       title = "Mean SST by Latitude with ±1 SD shading") +
  theme_minimal()

marine_sst_long <- marine_sst_points %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),  # all monthly date columns
    names_to = "date",
    values_to = "sst"
  ) %>%
  mutate(
    date = ymd(date),
    year = year(date),
    month = month(date)
  )
sst_lat_summary <- marine_sst_long %>%
  group_by(latitude) %>%
  summarise(
    mean_temp = mean(sst, na.rm = TRUE),
    sd_temp   = sd(sst, na.rm = TRUE),
    q_low     = quantile(sst, 0.025, na.rm = TRUE),
    q_high    = quantile(sst, 0.975, na.rm = TRUE),
    min_temp  = min(sst, na.rm = TRUE),
    max_temp  = max(sst, na.rm = TRUE)
  )


p <- ggplot(marine_sst_all_temps, aes(x = latitude)) +
  geom_ribbon(aes(ymin = q_low, ymax = q_high), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = sst_mean), color = "navy", linewidth = 1) +
  labs(x = "Latitude", y = "Sea Surface Temperature (°C)",
       title = "Mean and Range of SST by Latitude (1982–2025)") +
  theme_classic(base_size = 14)
p
ggplot(marine_sst_all_temps, aes(x = latitude)) +
  geom_ribbon(aes(ymin = sst_min, ymax = sst_max), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = sst_mean), color = "navy", linewidth = 1) +
  labs(x = "Latitude", y = "Sea Surface Temperature (°C)",
       title = "Mean and Range of SST by Latitude (1982–2025)") +
  theme_classic(base_size = 14)

