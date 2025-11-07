### this script is for testing hypoths ###
#packages
install.packages("lmerTest")
library(lme4)
library(lmerTest)
library(performance)
library(car)

library(here)
library(dplyr)
library(tidyverse)
#load data
fitted_datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))
curves <- readRDS(here('processed-data', 'wild-tpcs.Rds'))
freshwater_temps <- readRDS(here("processed-data", "freshwater_temperatures_my_points.RDS"))
##need to remove 2_0065, is in a bay / cod --=> marine
freshwater_temps <- freshwater_temps %>%
  filter(study_ID != "2_0065")
marine_sst <- readRDS(here("processed-data", "marine_sst_raw_temp.RDS"))

fitted_datasets <- fitted_datasets %>%
  mutate(land_or_sea = ifelse(land_or_sea == "terrestrial", "freshwater", "marine"))

#combine temperature data
#### need to do this in their intiial making files ####
marine_sst <- marine_sst %>%
  rename(temp_mean = sst_mean) %>%
  rename(temp_sd = sst_sd) %>%
  rename(temp_min = sst_min) %>%
  rename(temp_max = sst_max) %>%
  rename(temp_median = sst_median) %>%
  rename(temp_range = sst_range) %>%
  mutate(enviornment = "marine") %>%
  select(-(ID)) %>%
  rowwise() %>%
  mutate(
    q_low = quantile(c_across(`1982-01-01`:`2025-09-01`), probs = 0.025, na.rm = TRUE),
    q_high = quantile(c_across(`1982-01-01`:`2025-09-01`), probs = 0.975, na.rm = TRUE)
  ) %>%
  ungroup() %>%
    select(enviornment, latitude, longitude, study_ID, species_ID, distance, temp_mean, temp_median, temp_sd, temp_min, q_low, temp_max, q_high, temp_range)
freshwater_temps <- freshwater_temps %>%
  rename(q_low = temp_q_low) %>%
  rename(q_high = temp_q_high) %>%
  mutate(enviornment = "freshwater") %>%
  select(enviornment, latitude, longitude, study_ID, species_ID, distance, temp_mean, temp_median, temp_sd, temp_min, q_low, temp_max, q_high, temp_range)

temp_data_all <- rbind(freshwater_temps, marine_sst)
temp_data_all <- temp_data_all %>%
  inner_join(curves %>% select(curve_ID, latitude, longitude, study_ID, species_ID), join_by(latitude, longitude, study_ID, species_ID)) %>%
  distinct() %>%
  select(-(c(latitude, longitude, study_ID, species_ID)))

fits_with_temps <- fitted_datasets %>%
  left_join(temp_data_all, join_by(curve_ID))
  


#### H1: species thermal optima decreases with latitude ####
topt_lat <- ggplot(data = fitted_datasets %>%
         filter(topt_TF == TRUE) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = topt, color = land_or_sea)) +
  geom_abline(intercept = 40, slope = -1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(x = "Absolute Latitude", y = "Thermal Optima") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
topt_lat
ggsave("topt_lat_regression.pdf", plot = topt_lat, path = here("figures"), width = 5, height = 4)


topt_lat_realm <- lmer(topt ~ abs_latitude * land_or_sea + (1 | study_ID), 
                        data = fitted_datasets %>%
                          filter(topt_TF == TRUE,
                                 !is.na(abs_latitude)))

plot(residuals(topt_lat_realm))
qqnorm(resid(topt_lat_realm))
qqline(resid(topt_lat_realm))
hist(resid(topt_lat_realm))
summary(topt_lat_realm)
r2(topt_lat_realm)
Anova(topt_lat_realm) # affect of abs lat on topt is sig, but realm isnt?
## what about response type
# response types # 
res <- fitted_datasets %>%
  filter(topt_TF == TRUE) %>%
  group_by(response_type_group) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
#top groups are swimming, metabolism, and growth
ggplot(data = fitted_datasets %>%
         filter(topt_TF == TRUE) %>%
         filter(response_type_group %in% c("swimming", "metabolism", "growth")) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = topt, color = response_type_group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(x = "Absolute Latitude", y = "Thermal Optimum", title = "Scatter of Topt and latitude with response type")

resp_topt_lat <- lmer(topt ~ abs_latitude * response_type_group + (1 | study_ID),
                      data = fitted_datasets %>%
                        filter(response_type_group %in% c("swimming", "metabolism", "growth")) %>%
                        filter(topt_TF == TRUE,
                               !is.na(abs_latitude)))
plot(residuals(resp_topt_lat))
qqnorm(resid(resp_topt_lat))
qqline(resid(resp_topt_lat))
hist(resid(resp_topt_lat))
summary(resp_topt_lat)
r2(resp_topt_lat)
Anova(resp_topt_lat) 

# looking at acclim temp #
#curve type information
curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(same_acclim_temp = case_when(
    all(is.na(acclim_temp)) ~ NA,
    all(acclim_temp == acclim_temp[1], na.rm = TRUE) ~ TRUE,
    TRUE ~ FALSE),
    curve_acclim_temp = if_else(same_acclim_temp == TRUE, acclim_temp[1], NA)) %>%
  ungroup()
curves <- curves %>%
  mutate(curve_acclim_temp = ifelse(str_detect(curve_acclim_temp, "-"), # split by "-" and compute row-wise mean
                                    rowMeans(do.call(rbind, str_split(curve_acclim_temp, "-", simplify = FALSE)) %>% apply(2, as.numeric)),
                                    as.numeric(curve_acclim_temp)))     
fitted_datasets <- fitted_datasets %>%
  left_join(curves %>% select(curve_type, curve_ID, same_acclim_temp, curve_acclim_temp), join_by(curve_ID)) %>%
  distinct()

ggplot(data = fitted_datasets %>% 
         filter(topt_TF == TRUE) %>%
         filter(same_acclim_temp == TRUE),
       aes(x = curve_acclim_temp, y = topt)) +
  geom_point(alpha = 0.7) +
  labs(x = "aclim", y = "Thermal Optimum",
       title = "Scatter of Topt and aclim")

aclim_lm <- lm(topt ~ curve_acclim_temp, data = fitted_datasets %>% 
           filter(topt_TF == TRUE) %>%
           filter(same_acclim_temp == TRUE))

summary(aclim_lm)


#### topt and enviornmental temp ####

topt_mean_tm <- ggplot(data = fits_with_temps %>% 
         filter(topt_TF == TRUE),
       aes(x = temp_mean, y = topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "Average Water Temperature",
    y = "Thermal Optima") +
  theme_classic()

topt_mean_tm
ggsave("topt_mean_tmp_regression.pdf", plot = topt_mean_tm, path = here("figures"), width = 5, height = 4)

library(nlme)
mean_topt_model <- lme(topt ~ temp_mean * enviornment,
                          data = fits_with_temps %>%
                            filter(topt_TF == TRUE),
                          random = ~ 1|study_ID)

plot(residuals(mean_topt_model))
qqnorm(resid(mean_topt_model))
qqline(resid(mean_topt_model))
summary(mean_topt_model)
hist(resid(mean_topt_model))
r2(mean_topt_model)
Anova(mean_topt_model) #significant

#### topt and extremes ####
topt_ext_tm <- ggplot(data = fits_with_temps %>% 
                         filter(topt_TF == TRUE),
                       aes(x = q_high, y = topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "0.975 Quantile Temperature",
    y = "Thermal Optima") +
  theme_classic()

topt_ext_tm
ggsave("topt_extreme_tmp_regression.pdf", plot = topt_ext_tm, path = here("figures"), width = 5, height = 4)

library(nlme)
extreme_topt_model <- lme(topt ~ q_high * enviornment,
                       data = fits_with_temps %>%
                         filter(topt_TF == TRUE),
                       random = ~ 1|study_ID)
plot(residuals(extreme_topt_model))
qqnorm(resid(extreme_topt_model))
qqline(resid(extreme_topt_model))
summary(extreme_topt_model)
hist(resid(extreme_topt_model))



mean_topt_model <- lme(topt ~ temp_mean * enviornment,
                       data = fits_with_temps %>%
                         filter(topt_TF == TRUE),
                       random = ~ 1|study_ID)








#### performance breadth and tolerance breadth ####
tolerance <- ggplot(data = fits_with_temps %>% 
         filter(thermal_tolerance_TF == TRUE),
       aes(x = temp_sd, y = thermal_tolerance, color = enviornment)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal tolerance breadth") +
  theme_classic()
tolerance
ggsave("tolerance_var.pdf", plot = tolerance, path = here("figures"), width = 5, height = 4)

#performance breadth should increase with var
breadth <- ggplot(data = fits_with_temps %>% 
         filter(breadth_TF == TRUE),
       aes(x = temp_sd, y = breadth, color = enviornment)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal performance breadth") +
  theme_classic()
breadth
ggsave("breadth_var.pdf", plot = breadth, path = here("figures"), width = 5, height = 4)

#species in more variable enviornments should have larger performance breadths

mean_and_var_temp <- ggplot(data = fits_with_temps,
       aes(x = temp_mean, y = temp_sd, color = enviornment)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Average Temperature",
    y = "Variability (temp sd)") + 
  scale_color_manual(
  name = "Environment",
  values = c("marine" = "blue", "freshwater" = "lightgreen")
) +
  theme_classic()
ggsave("temp+mean+var.pdf", plot = mean_and_var_temp, path = here("figures"), width = 5, height = 4)

#show where breadths are in temperature space
library(nlme)
mean_breadth_model <- lme(breadth ~ temp_mean,
                     data = freshwater_temps %>%
                       filter(breadth_TF == TRUE),
                     random = ~ 1|study_ID)
plot(residuals(mean_breadth_model))
qqnorm(resid(mean_breadth_model))
qqline(resid(mean_breadth_model))
hist(resid(mean_breadth_model))
summary(mean_breadth_model)

var_breadth_model <- lme(breadth ~ temp_sd,
                          data = freshwater_temps %>%
                            filter(breadth_TF == TRUE),
                          random = ~ 1|study_ID)

plot(residuals(var_breadth_model))
qqnorm(resid(var_breadth_model))
qqline(resid(var_breadth_model))
hist(resid(var_breadth_model))
summary(var_breadth_model)
#tolerance breadth should increase with thermal variability





## deutsch warming tolerance - the difference between ctmax and mean env. temp
fits_with_temps <- fits_with_temps %>%
  mutate(warming_tolerance = ctmax - temp_mean) %>%
  mutate(thermal_safety_margin_duetsch = topt - temp_mean)

##does how close your topt is to your env temp depend on latitude???? ### 
fits_with_temps <- fits_with_temps %>%
  mutate(diff_max = q_high - topt) %>% 
  mutate(diff_mean = temp_mean - topt) 

###topt should be closer to mean water temp in the tropics (ie mag should decrease with abs. latitude), where temps are higher (out of the tropics hyp)
diff_mean <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = diff_mean, color = enviornment)) +
  geom_abline(intercept = 10, slope = -1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "mean temp - topt") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
ggsave("diff_mean.pdf", plot = diff_mean, path = here("figures"), width = 5, height = 4)

diff_extreme <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = diff_max, color = enviornment)) +
  geom_abline(intercept = 10, slope = -1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "extreme temp - topt") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
ggsave("diff_extreme.pdf", plot = diff_extreme, path = here("figures"), width = 5, height = 4)


##okay so how close topt is to envir. temp decreases with latitude in freshwater fish

# warming tolerance - if topt is closer to env mean in tropics, warming tolerance should increase with lat
warming_tol <- ggplot(data = fits_with_temps %>%
                         filter(thermal_max_TF == TRUE),
                       aes(x = abs_latitude, y = warming_tolerance, color = enviornment)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "warming tolerance (ctmax - temp_mean)") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
warming_tol

TSM <- ggplot(data = fits_with_temps %>%
                        filter(topt_TF == TRUE),
                      aes(x = abs_latitude, y = thermal_safety_margin_duetsch, color = enviornment)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "TSM (topt - temp_mean)") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
TSM
ggsave("TSM.pdf", plot = TSM, path = here("figures"), width = 5, height = 4)



#topt and enviornmental temp
dif <- fits_with_temps %>%
  pivot_longer(
    cols = c(diff_max, diff_mean),
    names_to = "diff_type",
    values_to = "diff_value"
  )
dif_top_en_his <- ggplot(dif %>% filter(topt_TF == TRUE)) +
  geom_boxplot(aes(x = diff_type, y = diff_value, color = enviornment)) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(x = "enviornmental temperature", y = "enviornmental temperature - thermal optima") +
  theme_classic()
ggsave("dif_his.pdf", plot = dif_top_en_his, path = here("figures"), width = 5, height = 4)


#topt will be more dif from sst max in enviornemnts with greater thermal variability -- ie mag will increase with sst var
#freshwater
fits_with_temps <- fits_with_temps %>%
  mutate(abs_dif_max = abs(diff_max))
freshwater_temps_topt <- freshwater_temps %>%
  filter(topt_TF == TRUE)
freshwater_temps_topt <- freshwater_temps_topt %>%
  mutate(abs_diff_max = abs(diff_max))
freshwater_temps_topt <- freshwater_temps_topt %>%
  mutate(abs_diff_mean = abs(diff_mean))
##predic

#topt is further above mean temp in more variabile enviorneents in marine systems
var_dif_mean_reg <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = temp_sd, y = diff_mean, color = enviornment)) +
  geom_abline(intercept = 0, slope = -1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "thermal variability (temp sd)",
    y = "mean temp - topt") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
ggsave("var_dif_mean_reg.pdf", plot = var_dif_mean_reg, path = here("figures"), width = 5, height = 4)

var_dif_max_reg <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = temp_sd, y = diff_max, color = enviornment)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "thermal variability (temp sd)",
    y = "extreme temp - topt") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
ggsave("var_dif_max_reg.pdf", plot = var_dif_max_reg, path = here("figures"), width = 5, height = 4)

extreme_mean_box <- ggplot(fits_with_temps, aes(x = enviornment, y = q_high - temp_mean, color = enviornment)) +
  geom_boxplot(alpha = 0.6, outlier.alpha = 0.4) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "Enviornment", 
    y = "Q-high - Mean Temp") + 
  theme_classic()
extreme_mean_box
ggsave("extreme-mean-box.pdf", plot = extreme_mean_box, path = here("figures"), width = 5, height = 4)



