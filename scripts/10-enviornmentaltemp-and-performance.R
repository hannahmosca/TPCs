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
  library(terra)

  #load data
  fitted_datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))
  fitted_datasets <- fitted_datasets %>%
    mutate(land_or_sea = ifelse(land_or_sea == "terrestrial", "freshwater", "marine"))
  curves <- readRDS(here('processed-data', 'wild-tpcs.Rds'))
  
  ##all temperatures
  #freshwater all
  all_freshwater_rast <- rast((here("processed-data", "freshwater_summarized_masked.nc"))) #average masked across months from 1982-2025 %>%
  names_temp <- c("mean", "sd", "min", "max", "q_low", "q_high")
  names(all_freshwater_rast) <- names_temp
  all_freshwater <- as.data.frame(all_freshwater_rast, xy = TRUE, na.rm = TRUE)
  all_freshwater <- all_freshwater %>%
    rename(longitude = x) %>%
    rename(latitude = y)
  #marine all
  all_marine_rast <- rast((here("processed-data", "sst_monthly_summarized.nc"))) 
  names(all_marine_rast) <- names_temp
  all_marine <- as.data.frame(all_marine_rast, xy = TRUE, na.rm = TRUE)
  all_marine <- all_marine %>%
    rename(longitude = x) %>%
    rename(latitude = y)
  
  ##point data
  freshwater_points <- readRDS(here("processed-data", "freshwater_temperatures_my_points.RDS"))
  marine_points <- readRDS(here("processed-data", "sst_temperatures_my_points.RDS"))

  ##### combine all temperature data ####
  all_freshwater <- all_freshwater %>%
    mutate(enviornment = "freshwater")
  freshwater_points <- freshwater_points %>%
    mutate(enviornment = "freshwater") %>%
    rename(q2.5 = q_low) %>%
    rename(q97.5 = q_high)
  all_marine <- all_marine %>%
   mutate(enviornment = "marine")
  marine_points <- marine_points %>%
    mutate(enviornment = "marine")
  
temp_data_all <- rbind(all_freshwater, all_marine)
point_data_all <- rbind(freshwater_points, marine_points) %>%
  select(latitude, longitude, everything())



fits_with_temps <- fitted_datasets %>%
  left_join(point_data_all, join_by(latitude, longitude))
  

#### H1: species thermal optima decreases with latitude ####
topt_lat <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = topt, color = land_or_sea)) +
  geom_point(alpha = 0.7) +
  labs(x = "Absolute Latitude", y = "Thermal Optima") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()

topt_lat_realm <- lmer(topt ~ abs_latitude * land_or_sea + (1 | study_ID), 
                        data = fits_with_temps %>%
                          filter(topt_TF == TRUE,
                                 !is.na(abs_latitude)))
#194 observations
#72 studies

plot(residuals(topt_lat_realm))
qqnorm(resid(topt_lat_realm))
qqline(resid(topt_lat_realm))
hist(resid(topt_lat_realm))
summary(topt_lat_realm)
## predict 
library(ggeffects)
library(ggplot2)
## want to make sure only predicting on range of data
lat_range <- fits_with_temps %>%
  filter(topt_TF == TRUE, !is.na(abs_latitude)) %>%
  group_by(land_or_sea) %>%
  summarise(
    min_lat = min(abs_latitude),
    max_lat = max(abs_latitude)
  )
lat_range
fresh_grid <- data.frame(
  abs_latitude = seq(lat_range$min_lat[lat_range$land_or_sea=="freshwater"],
                     lat_range$max_lat[lat_range$land_or_sea=="freshwater"],
                     length.out = 200),
  land_or_sea = "freshwater"
)

marine_grid <- data.frame(
  abs_latitude = seq(lat_range$min_lat[lat_range$land_or_sea=="marine"],
                     lat_range$max_lat[lat_range$land_or_sea=="marine"],
                     length.out = 200),
  land_or_sea = "marine"
)

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(topt_lat_realm, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(topt_lat_realm, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


topt_lat <- ggplot(data = pred_grid, aes(x = abs_latitude)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = abs_latitude, y = topt, color = land_or_sea), alpha = .6) +
  geom_line(aes(y = pred, color = land_or_sea)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = land_or_sea), alpha = 0.20) +
  labs(x = "Absolute Latitude", y = "Thermal Optima") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
topt_lat
ggsave("topt_lat_regression.jpeg", plot = topt_lat, path = here("figures"), width = 5, height = 4)



#### topt and enviornmental temp ####
topt_mean_tm <- ggplot(data = fits_with_temps %>% 
         filter(topt_TF == TRUE),
       aes(x = mean, y = topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "Average Water Temperature",
    y = "Thermal Optima") +
  theme_classic()


mean_topt_model <- lmer(topt ~ mean * enviornment + (1 | study_ID), 
                       data = fits_with_temps %>%
                         filter(topt_TF == TRUE))


plot(residuals(mean_topt_model))
qqnorm(resid(mean_topt_model))
qqline(resid(mean_topt_model))
summary(mean_topt_model)
hist(resid(mean_topt_model))
r2(mean_topt_model)
Anova(mean_topt_model) #significant


## want to make sure only predicting on range of data
temp_range <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(enviornment) %>%
  summarise(
    min_mean_temp = min(mean),
    max_max_temp = max(mean))
  temp_range
fresh_grid <- data.frame(
  mean = seq(temp_range$min_mean_temp[temp_range$enviornment=="freshwater"],
                  temp_range$max_max_temp[temp_range$enviornment=="freshwater"],
                     length.out = 200),
  enviornment = "freshwater")
marine_grid <- data.frame(
  mean = seq(temp_range$min_mean_temp[temp_range$enviornment=="marine"],
                     temp_range$max_max_temp[temp_range$enviornment=="marine"],
                     length.out = 200),
  enviornment = "marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(mean_topt_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(mean_topt_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


topt_meantemp <- ggplot(data = pred_grid, aes(x = mean)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = mean, y = topt, color = enviornment), alpha = .6) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Average Water Temperature", y = "Thermal Optima") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
topt_meantemp


ggsave("topt_mean_tmp_regression.pdf", plot = topt_meantemp, path = here("figures"), width = 5, height = 4)



#### topt and extremes ####
ggplot(data = fits_with_temps %>% 
                         filter(topt_TF == TRUE),
                       aes(x = q97.5, y = topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "Extreme Water Temperature",
    y = "Thermal Optima") +
  theme_classic()


extremes_topt_model <- lmer(topt ~ q97.5 * enviornment + (1 | study_ID), 
                        data = fits_with_temps %>%
                          filter(topt_TF == TRUE))


plot(residuals(extremes_topt_model))
qqnorm(resid(extremes_topt_model))
qqline(resid(extremes_topt_model))
summary(extremes_topt_model)
hist(resid(extremes_topt_model))

temp_range <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(enviornment) %>%
  summarise(
    min_upper_temp = min(q97.5),
    max_upper_temp = max(q97.5))
temp_range
fresh_grid <- data.frame(
  q97.5 = seq(temp_range$min_upper_temp[temp_range$enviornment=="freshwater"],
             temp_range$max_upper_temp[temp_range$enviornment=="freshwater"],
             length.out = 200),
  enviornment = "freshwater")
marine_grid <- data.frame(
  q97.5 = seq(temp_range$min_upper_temp[temp_range$enviornment=="marine"],
             temp_range$max_upper_temp[temp_range$enviornment=="marine"],
             length.out = 200),
  enviornment = "marine")
pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(extremes_topt_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(extremes_topt_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit
pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


topt_extremetemp <- ggplot(data = pred_grid, aes(x = q97.5)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = q97.5, y = topt, color = enviornment), alpha = .6) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Extreme Water Temperature", y = "Thermal Optima") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic() +
  theme(
    legend.position = "none")
topt_extremetemp

topt_meantemp


library(patchwork)
plot <- topt_meantemp + topt_extremetemp
plot
ggsave("topt_envtemp.jpeg", plot = plot, path = here("figures"), width = 10, height = 5)


##### title ####
##does how close your topt is to your env temp depend on how variable your en???? ### 
fits_with_temps <- fits_with_temps %>%
  mutate(diff_max = q97.5 - topt) %>% 
  mutate(diff_mean = mean - topt) 
#topt and enviornmental temp
dif <- fits_with_temps %>%
  pivot_longer(
    cols = c(diff_mean, diff_max),
    names_to = "diff_type",
    values_to = "diff_value"
  )
dif_top_en_his <- ggplot(dif %>% 
                           filter(topt_TF == TRUE) %>%
                           mutate(diff_type = factor(diff_type, 
                                                     levels = c("diff_mean", "diff_max"), 
                                                     labels = c("Mean", "Extreme")))) +
  geom_boxplot(aes(x = diff_type, y = diff_value, color = enviornment)) +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(x = "Environmental Temperature", y = "Etemp - Topt") +
  theme_classic()

dif_top_en_his
ggsave("dif_his.pdf", plot = dif_top_en_his, path = here("figures"), width = 5, height = 4)

#topt will be more dif from sst max in enviornemnts with greater thermal variability -- ie mag will increase with sst var
#topt is further above mean temp in more variable environments in marine systems

## mean diff (mean - topt)
var_dif_mean_reg <- ggplot(data = fits_with_temps %>%
                             filter(topt_TF == TRUE),
                           aes(x = sd, y = (mean-topt), color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()

var_dif_mean_reg

var_dif_mean_model <- lmer(diff_mean ~ sd*enviornment + (1 | study_ID),
                           data = fits_with_temps %>%
                             filter(topt_TF == TRUE))

plot(residuals(var_dif_mean_model))
qqnorm(resid(var_dif_mean_model))
qqline(resid(var_dif_mean_model))
hist(resid(var_dif_mean_model))
summary(var_dif_mean_model) #marine sig?

temp_range <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(enviornment) %>%
  summarise(
    min_sd_temp = min(sd),
    max_sd_temp = max(sd))
temp_range
fresh_grid <- data.frame(
  sd = seq(temp_range$min_sd_temp[temp_range$enviornment=="freshwater"],
             temp_range$max_sd_temp[temp_range$enviornment=="freshwater"],
             length.out = 200),
  enviornment = "freshwater")
marine_grid <- data.frame(
  sd = seq(temp_range$min_sd_temp[temp_range$enviornment=="marine"],
             temp_range$max_sd_temp[temp_range$enviornment=="marine"],
             length.out = 200),
  enviornment = "marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(var_dif_mean_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(var_dif_mean_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


var_dif_mean_reg <- ggplot(data = pred_grid, aes(x = sd)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = sd, y = diff_mean, color = enviornment), alpha = .6) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "SD water temp", y = "Mean water temp - thermal optimum") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
var_dif_mean_reg



## extreme diff (q97.5 - topt)
var_dif_extreme_reg <- ggplot(data = fits_with_temps %>%
                             filter(topt_TF == TRUE),
                           aes(x = sd, y = (q97.5-topt), color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()

var_dif_extreme_reg

var_dif_extreme_model <- lmer(diff_max ~ sd*enviornment + (1 | study_ID),
                           data = fits_with_temps %>%
                             filter(topt_TF == TRUE))

plot(residuals(var_dif_extreme_model))
qqnorm(resid(var_dif_extreme_model))
qqline(resid(var_dif_extreme_model))
hist(resid(var_dif_extreme_model))
summary(var_dif_extreme_model) #both sig

temp_range <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(enviornment) %>%
  summarise(
    min_sd_temp = min(sd),
    max_sd_temp = max(sd))
temp_range
fresh_grid <- data.frame(
  sd = seq(temp_range$min_sd_temp[temp_range$enviornment=="freshwater"],
           temp_range$max_sd_temp[temp_range$enviornment=="freshwater"],
           length.out = 200),
  enviornment = "freshwater")
marine_grid <- data.frame(
  sd = seq(temp_range$min_sd_temp[temp_range$enviornment=="marine"],
           temp_range$max_sd_temp[temp_range$enviornment=="marine"],
           length.out = 200),
  enviornment = "marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(var_dif_extreme_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(var_dif_extreme_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


var_dif_extreme_reg <- ggplot(data = pred_grid, aes(x = sd)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = sd, y = diff_max, color = enviornment), alpha = .6) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "SD water temp", y = "extremem water temp - thermal optimum") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )
var_dif_extreme_reg

both_plot <- var_dif_mean_reg + var_dif_extreme_reg

ggsave("topt_diff_envtemp.jpeg", plot = both_plot, path = here("figures"), width = 10, height = 5)
























#### performance breadth and tolerance breadth ####
tolerance <- ggplot(data = fits_with_temps %>% 
         filter(breadth_TF == TRUE),
       aes(x = sd, y = breadth, color = enviornment)) +
  geom_point(alpha = 0.7)  +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal breadth") +
  theme_classic()
tolerance
ggsave("tolerance_var.pdf", plot = tolerance, path = here("figures"), width = 5, height = 4)

#performance breadth should increase with var
breadth <- ggplot(data = fits_with_temps %>% 
         filter(breadth_TF == TRUE),
       aes(x = sd, y = breadth, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
breadth
ggsave("breadth_var.pdf", plot = breadth, path = here("figures"), width = 5, height = 4)

#species in more variable enviornments should have larger performance breadths

mean_and_var_temp <- ggplot(data = fits_with_temps,
       aes(x = mean, y = sd, color = enviornment)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Average Temperature",
    y = "Variability (temp sd)") + 
  scale_color_manual(
  name = "Environment",
  values = c("marine" = "blue", "freshwater" = "lightgreen")
) +
  theme_classic()
mean_and_var_temp
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

var_breadth_model <- lme(breadth ~ sd*enviornment,
                          data = fits_with_temps %>%
                            filter(breadth_TF == TRUE),
                           random = ~ 1|study_ID)

summary(var_breadth_model)
plot(residuals(var_breadth_model))
qqnorm(resid(var_breadth_model))
qqline(resid(var_breadth_model))
hist(resid(var_breadth_model))
summary(var_breadth_model)
#tolerance breadth should increase with thermal variability





## deutsch warming tolerance - the difference between ctmax and mean env. temp
fits_with_temps <- fits_with_temps %>%
  mutate(warming_tolerance = ctmax - mean) %>%
  mutate(thermal_safety_margin_duetsch = topt - mean)

##does how close your topt is to your env temp depend on latitude???? ### 
fits_with_temps <- fits_with_temps %>%
  mutate(diff_max = q97.5 - topt) %>% 
  mutate(diff_mean = mean - topt) 

###topt should be closer to mean water temp in the tropics (ie mag should decrease with abs. latitude), where temps are higher (out of the tropics hyp)
diff_mean <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = diff_mean)) +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  # scale_color_manual(
  #   name = "Environment",
  #   values = c("marine" = "blue", "freshwater" = "lightgreen")
  # ) +
  theme_classic()

diff_mean

model <- lme((topt-mean) ~ sd*enviornment,
                        data = fits_with_temps %>%
                          filter(topt_TF == TRUE),
                        random = ~ 1|study_ID)
summary(model)

diff_extreme <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = topt-q97.5, color = enviornment)) +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
diff_extreme
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
    cols = c(diff_mean, diff_max),
    names_to = "diff_type",
    values_to = "diff_value"
  )
dif_top_en_his <- ggplot(dif %>% 
                           filter(topt_TF == TRUE) %>%
                           mutate(diff_type = factor(diff_type, 
                                                     levels = c("diff_mean", "diff_max"), 
                                                     labels = c("Mean", "Extreme")))) +
  geom_boxplot(aes(x = diff_type, y = diff_value, color = enviornment)) +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(x = "Environmental Temperature", y = "Etemp - Topt") +
  theme_classic()

dif_top_en_his
ggsave("dif_his.pdf", plot = dif_top_en_his, path = here("figures"), width = 5, height = 4)

#topt will be more dif from sst max in enviornemnts with greater thermal variability -- ie mag will increase with sst var
#topt is further above mean temp in more variable environments in marine systems

## mean diff (mean - topt)
var_dif_mean_reg <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = sd, y = (mean-topt), color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()

var_dif_mean_reg

var_dif_mean_model <- lmer(diff_mean ~ sd*enviornment + (1 | study_ID),
             data = fits_with_temps %>%
               filter(topt_TF == TRUE))

plot(residuals(var_dif_mean_model))
qqnorm(resid(var_dif_mean_model))
qqline(resid(var_dif_mean_model))
hist(resid(var_dif_mean_model))
summary(var_dif_mean_model) #marine sig?






var_dif_max_reg <- ggplot(data = fits_with_temps %>%
                             filter(topt_TF == TRUE),
                           aes(x = sd, y = diff_max, color = enviornment)) +
  geom_point(alpha = 0.7) + 
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()

var_dif_max_reg


var_dif_max_model <- lmer((topt-q97.5) ~ sd*enviornment + (1 | study_ID),
                           data = fits_with_temps %>%
                             filter(topt_TF == TRUE))






plot(residuals(var_dif_max_model))
qqnorm(resid(var_dif_max_model))
qqline(resid(var_dif_max_model))
hist(resid(var_dif_max_model))
summary(var_dif_mean_model)











var_dif_mean_reg <- ggplot(data = fits_with_temps %>%
                             filter(topt_TF == TRUE),
                           aes(x = temp_sd, y = topt-temp_mean, color = enviornment)) +
  geom_abline(intercept = 0, slope = -1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "thermal variability (temp sd)",
    y = "topt- mean") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
var_dif_mean_reg

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

ggplot(data = fits_with_temps %>%
                             filter(topt_TF == TRUE),
                           aes(x = abs_latitude, y = temp_sd, color = enviornment)) +
  geom_abline(intercept = 0, slope = -1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "latitude",
    y = "thermal variability") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()


ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = temp_sd, color = enviornment)) +
  geom_abline(intercept = 0, slope = -1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "latitude",
    y = "thermal variability") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()


ggplot(data = fits_with_temps) +
  geom_point(aes(x = abs_latitude, y = q_low, color = enviornment)) +
  geom_point(aes(x = abs_latitude, y = q_high, color = enviornment), shape = 2)+
  labs(
    x = "abs latitude",
    y = "extremes") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()








#### dumping ####

## what about response type
# response types # 
res <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(response_type_group) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
#top groups are swimming, metabolism, and growth
ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE) %>%
         filter(response_type_group %in% c("swimming", "metabolism", "growth")) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = topt, color = response_type_group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(x = "Absolute Latitude", y = "Thermal Optimum", title = "Scatter of Topt and latitude with response type")

resp_topt_lat <- lmer(topt ~ abs_latitude * response_type_group + (1 | study_ID),
                      data = fits_with_temps %>%
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



fits_with_temps <- fits_with_temps %>%
  left_join(breadth_summary %>% dplyr::select(curve_ID, model, tmin_breadth, tmax_breadth, my_breadth), join_by(curve_ID, model))

ggplot() +
  geom_point(data = fits_with_temps %>% 
               filter(breadth_TF == TRUE) %>%
               filter(enviornment == "marine"), 
             aes(x = abs_latitude, y = topt), color = "red") +
  geom_point(data = fits_with_temps %>% 
               filter(breadth_TF == TRUE) %>%
               filter(enviornment == "freshwater"), 
             aes(x = abs_latitude, y = topt), color = "red") +
  geom_point(data = fits_with_temps %>% 
               filter(breadth_TF == TRUE),
             aes(x = abs_latitude, y = mean), color = "black") +
  geom_linerange(data = fits_with_temps %>% 
                   filter(breadth_TF == TRUE) %>%
                   filter(enviornment == "marine"), 
                 aes(x = abs_latitude, ymin = tmin_breadth, ymax = tmax_breadth), color = "darkblue", linewidth = 1, alpha = .3) +
  geom_linerange(data = fits_with_temps %>% 
                   filter(breadth_TF == TRUE) %>%
                   filter(enviornment == "freshwater"), 
                 aes(x = abs_latitude, ymin = tmin_breadth, ymax = tmax_breadth), color = "darkgreen", linewidth = 1, alpha = .3)




