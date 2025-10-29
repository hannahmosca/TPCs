
# load lmr stuff
library(lmerTest)
library(car)
install.packages("performance")
library(performance)
#### topt and enviornmental temp ####
ggplot(data = freshwater_unflagged %>% 
         filter(topt_TF == TRUE),
       aes(x = temp_mean, y = topt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "mean water temperature",
    y = "Topt",
    title = "topt and mean env temp"
  )
mean_topt_model <- lmer(topt ~ temp_mean + (1 | study_ID), 
                 data = freshwater_unflagged %>%
                   filter(topt_TF == TRUE))
plot(residuals(mean_topt_model))
qqnorm(resid(mean_topt_model))
qqline(resid(mean_topt_model))
summary(mean_topt_model)
r2(mean_topt_model)
Anova(mean_topt_model) #significant

ggplot(data = freshwater_unflagged %>% 
         filter(topt_TF == TRUE),
       aes(x = temp_max, y = topt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "max water temperature",
    y = "Topt",
    title = "topt and max env temp"
  )
max_topt_model <- lmer(topt ~ temp_max + (1 | study_ID), 
                        data = freshwater_unflagged %>%
                          filter(topt_TF == TRUE))
plot(residuals(max_topt_model))
qqnorm(resid(max_topt_model))
qqline(resid(max_topt_model))
summary(max_topt_model)
r2(max_topt_model)
Anova(max_topt_model)
AIC(mean_topt_model, max_topt_model) #max isnt?

#predictions
pred_mean <- freshwater_unflagged %>%
  filter(topt_TF == TRUE) %>%
  dplyr::select(temp_mean) %>%
  distinct() %>%
  mutate(pred_mean = predict(mean_topt_model, newdata = ., re.form = NA))
pred_max <- freshwater_unflagged %>%
  filter(topt_TF == TRUE) %>%
  dplyr::select(temp_max) %>%
  distinct() %>%
  mutate(pred_max = predict(max_topt_model, newdata = ., re.form = NA))

ggplot(freshwater_unflagged %>% filter(topt_TF == TRUE)) +
  geom_point(aes(x = temp_mean, y = topt), color = "blue", alpha = 0.5) +
  geom_line(data = pred_mean, aes(x = temp_mean, y = pred_mean), color = "blue", linewidth = 1.2) +
  geom_point(aes(x = temp_max, y = topt), color = "red", alpha = 0.5) +
  geom_line(data = pred_max, aes(x = temp_max, y = pred_max), color = "red", linewidth = 1.2) +
  labs(
    x = "Environmental Temperature",
    y = "Topt",
    title = "Comparison of Mean vs Maximum water temp correlated w/ Topt in marine fish",
    subtitle = "Blue = Mean temp, Red = Max temp"
  ) +
  theme_minimal()
##topt is closer to sst mean than sst max in fresh


#### performance breadth and tolerance breadth ####
ggplot(data = freshwater_unflagged %>% 
         filter(thermal_tolerance_TF == TRUE),
       aes(x = temp_sd, y = thermal_tolerance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal tolerance breadth",
    title = "tolerance breadth and Variability"
  )

ggplot(data = freshwater_unflagged %>% 
         filter(breadth_TF == TRUE),
       aes(x = temp_sd, y = breadth)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal performance breadth",
    title = "Performance Breadth and Variability"
  )
library(nlme)
breadth_model <- lme(breadth ~ temp_mean,
                     data = freshwater_unflagged %>%
                       filter(breadth_TF == TRUE),
                     random = ~ 1|study_ID)
plot(residuals(breadth_model))
qqnorm(resid(breadth_model))
qqline(resid(breadth_model))
hist(resid(breadth_model))
summary(breadth_model)
r2(breadth_model)
Anova(breadth_model)

## deutsch warming tolerance - the difference between ctmax and mean env. temp
freshwater_unflagged <- freshwater_unflagged %>%
  mutate(warming_tolerance = ctmax - temp_mean) %>%
  mutate(thermal_safety_margin_duetsch = topt - temp_mean)

ggplot(data = freshwater_unflagged %>%
         filter(thermal_max_TF == TRUE),
       aes(x = abs_latitude, y = warming_tolerance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "latitude",
    y = "warming tolerance",
    title = "Scatter"
  )
##does how close your topt is to your env temp depend on latitude???? ### 
freshwater_unflagged <- freshwater_unflagged %>%
  mutate(diff_max = temp_max - topt) %>% 
  mutate(diff_mean = temp_mean - topt) 
###topt should be closer to mean water temp in the tropics (ie mag should decrease with abs. latitude), where temps are higher
ggplot(data = freshwater_unflagged %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = diff_mean)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "abs latitude",
    y = "magnitude of difference between topt and mean water temp",
    title = "latitudinal trends in topt and mean water temp"
  )
dif_model <- lme(abs_latitude ~ diff_mean,
                 data = freshwater_unflagged %>%
                   filter(topt_TF == TRUE),
                 random = ~ 1|study_ID)
plot(residuals(dif_model))
qqnorm(resid(dif_model))
qqline(resid(dif_model))
summary(dif_model)
r2(dif_model)
Anova(dif_model) #significant

### tsm should increase with latitude (deutsch), because fish in tropics are already living close to their physiological optimum
ggplot(data = freshwater_unflagged %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = thermal_safety_margin_duetsch)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "latitude",
    y = "TSM",
    title = "Scatter"
  )
TSM_model <- lmer(abs_latitude ~ thermal_safety_margin_duetsch + (1 | study_ID), 
                  data = freshwater_unflagged %>%
                    filter(topt_TF == TRUE))
plot(residuals(TSM_model))
qqnorm(resid(TSM_model))
qqline(resid(TSM_model))
summary(TSM_model)
r2(TSM_model)
Anova(TSM_model) #significant



#topt and enviornmental temp
dif_fresh <- freshwater_unflagged %>%
  pivot_longer(
    cols = c(diff_max, diff_mean),
    names_to = "diff_type",
    values_to = "diff_value"
  )
ggplot(dif_fresh %>% filter(topt_TF == TRUE)) +
  geom_boxplot(aes(x = diff_type, y = diff_value))
#dif 
# qq
freshwater_unflagged_topt <- freshwater_unflagged %>%
  filter(topt_TF == TRUE)
qqnorm(freshwater_unflagged_topt$diff_mean); qqline(freshwater_unflagged_topt$diff_mean)
qqnorm(freshwater_unflagged_topt$diff_max); qqline(freshwater_unflagged_topt$diff_max)

#topt will be more dif from sst max in enviornemnts with greater thermal variability -- ie mag will increase with sst var
freshwater_unflagged_topt <- freshwater_unflagged_topt %>%
  mutate(abs_diff_max = abs(diff_max))
freshwater_unflagged_topt <- freshwater_unflagged_topt %>%
  mutate(abs_diff_mean = abs(diff_mean))

qqnorm(freshwater_unflagged_topt$abs_diff_max); qqline(freshwater_unflagged_topt$abs_diff_max)
freshwater_unflagged_topt <- freshwater_unflagged_topt %>%
  mutate(log_abs_diff_max = log1p(abs_diff_max))  # log1p handles 0s safely
qqnorm(freshwater_unflagged_topt$abs_diff_max); qqline(freshwater_unflagged_topt$abs_diff_max)

ggplot(data = freshwater_unflagged_topt,
       aes(x = temp_sd, y = abs_diff_mean)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "water temp sd)",
    y = "magnitude of dif between topt and mean temp (logged)",
    title = "thermal variation and topt in relation to mean temp"
  )

qqnorm(freshwater_unflagged_topt$abs_diff_mean); qqline(freshwater_unflagged_topt$abs_diff_mean)
freshwater_unflagged_topt <- freshwater_unflagged_topt %>%
  mutate(log_abs_diff_mean = log1p(abs_diff_mean))  # log1p handles 0s safely
qqnorm(freshwater_unflagged_topt$log_abs_diff_mean); qqline(freshwater_unflagged_topt$log_abs_diff_mean)

ggplot(data = freshwater_unflagged_topt, 
       aes(x = temp_sd, y = abs_diff_max)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "SST variability (SST sd)", 
    y = "magnitude of dif between topt and max temp (logged)",
    title = "thermal variation and topt in relation to max temp"
  )
#Magnitude-only test
varmean <- (lmer(abs_diff_mean ~ temp_sd + (1 | study_ID), data = freshwater_unflagged_topt)) #expect dif between sstmean and topt to scale with thermal variation
varmax <- (lmer(abs_diff_max ~ temp_sd + (1 | study_ID), data = freshwater_unflagged_topt))
hist(resid(varmax))
plot(residuals(varmean))
qqnorm(resid(varmean))
qqline(resid(varmean))
summary(varmean)
r2(varmean)
Anova(varmean) #significant

plot(residuals(varmax))
qqnorm(resid(varmax))
qqline(resid(varmax))
summary(varmax)
r2(varmax)
Anova(varmax) #

#dif between mean sst and topt is more related to thermal variability than dif between max sst and topt
AIC(varmean, varmax) 
anova(varmean, varmax)

library(ggeffects)
# Predict Topt offset from mean SST over sst_sd range
pred_dif_temp_sd_mean <- ggpredict(varmean, terms = "temp_sd")
pred_dif_temp_sd_max <- ggpredict(varmax, terms = "temp_sd")
pred_dif_temp_sd_mean <- as.data.frame(pred_dif_temp_sd_mean)
pred_dif_temp_sd_max <- as.data.frame(pred_dif_temp_sd_max)


ggplot(freshwater_unflagged_topt) +
  geom_point(aes(x = temp_sd, y = log_abs_diff_mean), color = "blue", alpha = 0.5) +
  geom_line(data = pred_dif_temp_sd_mean, aes(x = x, y = predicted), color = "blue", linewidth = 1.2) + 
  geom_ribbon(data = pred_dif_temp_sd_mean, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) +
  geom_point(aes(x = temp_sd, y = log_abs_diff_max), color = "red", alpha = 0.5) +
  geom_line(data = pred_dif_temp_sd_max, aes(x = x, y = predicted), color = "red", linewidth = 1.2) + 
  geom_ribbon(data = pred_dif_temp_sd_max, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "red", alpha = 0.2) +
  labs(
    x = "SST_SD",
    y = "Difference between topt and sst",
    title = "Thermal variability and magnitude of difference \n between topt and SST",
    subtitle = "Blue = Mean SST, Red = Max SST"
  ) +
  theme_minimal()

