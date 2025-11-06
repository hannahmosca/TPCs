### this script is for testing hypoths ###
#packages
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
marine_sst <- readRDS(here("processed-data", "marine_sst_raw_temp"))

#combine temperature data

marine_sst <- marine_sst %>%
  rename(temp_mean = sst_mean) %>%
  rename(temp_sd = sst_sd) %>%
  rename(temp_min = sst_min) %>%
  rename(temp_max = sst_max) %>%
  rename(temp_median = sst_median) %>%
  rename(temp_range = sst_range) %>%
  mutate(water_temp_type = "sst")

freshwater_temps <- freshwater_temps %>%
  mutate(water_temp_type = "fresh_temp")

temp_data_all <- rbind(freshwater_temps, marine_sst)



#### H1: species thermal optima decreases with latitude ####
ggplot(data = fitted_datasets %>%
         filter(topt_TF == TRUE) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = topt, color = land_or_sea)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(x = "Absolute Latitude", y = "Thermal Optimum", 
       title = "Scatter of Topt and latitude")
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

ggplot(data = temp_data_all %>% 
         filter(topt_TF == TRUE),
       aes(x = temp_mean, y = topt, colour = water_temp_type)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(
    x = "Average Water Temperature",
    y = "Thermal Optimum",
    title = "topt and mean env temp"
  )
mean_topt_model <- lmer(topt ~ temp_mean * water_temp_type + (1 | study_ID), 
                        data = temp_data_all %>%
                          filter(topt_TF == TRUE))
plot(residuals(mean_topt_model))
qqnorm(resid(mean_topt_model))
qqline(resid(mean_topt_model))
summary(mean_topt_model)
hist(resid(mean_topt_model))
r2(mean_topt_model)
Anova(mean_topt_model) #significant








# freshwater 

ggplot(data = freshwater_temps %>% 
         filter(topt_TF == TRUE),
       aes(x = temp_mean, y = topt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "mean water temperature",
    y = "Topt",
    title = "topt and mean env temp"
  )
mean_topt_model <- lmer(topt ~ temp_mean + (1 | study_ID), 
                 data = freshwater_temps %>%
                   filter(topt_TF == TRUE))
plot(residuals(mean_topt_model))
qqnorm(resid(mean_topt_model))
qqline(resid(mean_topt_model))
summary(mean_topt_model)
hist(resid(mean_topt_model))
r2(mean_topt_model)
Anova(mean_topt_model) #significant

ggplot(data = freshwater_temps %>% 
         filter(topt_TF == TRUE),
       aes(x = temp_max, y = topt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "max water temperature",
    y = "Topt",
    title = "topt and max env temp"
  )
max_topt_model <- lmer(topt ~ temp_max + (1 | study_ID), 
                        data = freshwater_temps %>%
                          filter(topt_TF == TRUE))
plot(residuals(max_topt_model))
qqnorm(resid(max_topt_model))
qqline(resid(max_topt_model))
summary(max_topt_model)
r2(max_topt_model)
Anova(max_topt_model) #max isnt sig? 
AIC(mean_topt_model, max_topt_model) #mean water temp for freshwater better correlates with topt

#predictions
pred_mean <- freshwater_temps %>%
  filter(topt_TF == TRUE) %>%
  dplyr::select(temp_mean) %>%
  distinct() %>%
  mutate(pred_mean = predict(mean_topt_model, newdata = ., re.form = NA))
pred_max <- freshwater_temps %>%
  filter(topt_TF == TRUE) %>%
  dplyr::select(temp_max) %>%
  distinct() %>%
  mutate(pred_max = predict(max_topt_model, newdata = ., re.form = NA))

fresh_mean_max <- ggplot(freshwater_temps %>% filter(topt_TF == TRUE)) +
  geom_point(aes(x = temp_mean, y = topt), color = "blue", alpha = 0.5) +
  geom_line(data = pred_mean, aes(x = temp_mean, y = pred_mean), color = "blue", linewidth = 1.2) +
  geom_point(aes(x = temp_max, y = topt), color = "red", alpha = 0.5) +
  geom_line(data = pred_max, aes(x = temp_max, y = pred_max), color = "red", linewidth = 1.2) +
  labs(
    x = "Environmental Temperature",
    y = "Topt",
    title = "freshwater fish",
    subtitle = "Blue = Mean temp, Red = Max temp"
  ) +
  theme_minimal()
fresh_mean_max
##topt is closer to sst mean than sst max in fresh

# marine
ggplot(data = marine_sst %>% 
         filter(topt_TF == TRUE),
       aes(x = sst_mean, y = topt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "mean SST",
    y = "Topt",
    title = "mean SST and fish thermal optima"
  )

m_mean <- lmer(topt ~ sst_mean + (1 | study_ID), 
               data = marine_sst %>% filter(topt_TF == TRUE))
plot(residuals(m_mean))
qqnorm(resid(m_mean))
qqline(resid(m_mean))
summary(m_mean)
r2(m_mean)
Anova(m_mean) #significant

ggplot(data = marine_sst %>% 
         filter(topt_TF == TRUE),
       aes(x = sst_max, y = topt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "max SST",
    y = "Topt",
    title = "max SST and fish thermal optima"
  )
m_max  <- lmer(topt ~ sst_max + (1 | study_ID), 
               data = marine_sst %>% filter(topt_TF == TRUE))
plot(residuals(m_max))
qqnorm(resid(m_max))
qqline(resid(m_max))
summary(m_max)
r2(m_max)
Anova(m_max) #significant
AIC(m_mean, m_max) #max temp better correlates with topt
anova(m_mean, m_max)

#predictions
pred_mean <- marine_sst %>%
  filter(topt_TF == TRUE) %>%
  dplyr::select(sst_mean) %>%
  distinct() %>%
  mutate(pred_mean = predict(m_mean, newdata = ., re.form = NA))
pred_max <- marine_sst %>%
  filter(topt_TF == TRUE) %>%
  dplyr::select(sst_max) %>%
  distinct() %>%
  mutate(pred_max = predict(m_max, newdata = ., re.form = NA))

marine_mean_max <- ggplot(marine_sst %>% filter(topt_TF == TRUE)) +
  geom_point(aes(x = sst_mean, y = topt), color = "blue", alpha = 0.5) +
  geom_line(data = pred_mean, aes(x = sst_mean, y = pred_mean), color = "blue", linewidth = 1.2) +
  geom_point(aes(x = sst_max, y = topt), color = "red", alpha = 0.5) +
  geom_line(data = pred_max, aes(x = sst_max, y = pred_max), color = "red", linewidth = 1.2) +
  labs(
    x = "Environmental Temperature",
    y = "Topt",
    title = "marine fish",
    subtitle = "Blue = Mean SST, Red = Max SST"
  ) +
  theme_minimal()
marine_mean_max
library(patchwork)
marine_mean_max + fresh_mean_max

##topt is aligns more with sst max than sst mean in marine fish?##


#### performance breadth and tolerance breadth ####
ggplot(data = freshwater_temps %>% 
         filter(thermal_tolerance_TF == TRUE),
       aes(x = temp_sd, y = thermal_tolerance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal tolerance breadth",
    title = "freshwater tolerance breadth and Variability"
  )
#performance breadth should decrease with temperature (because it increases with latitude)
ggplot(data = freshwater_temps %>% 
         filter(breadth_TF == TRUE),
       aes(x = temp_mean, y = breadth)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "mean enviornmental temperature",
    y = "thermal performance breadth",
    title = "freshwater performance breadth and enviornmental temperature"
  )

#species in more variable enviornments should have larger performance breadths
ggplot(data = freshwater_temps %>% 
         filter(breadth_TF == TRUE),
       aes(x = temp_sd, y = breadth)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "Thermal variability (temp sd)",
    y = "thermal performance breadth",
    title = "freshwater performance Breadth and Variability"
  )
ggplot(data = freshwater_temps,
       aes(x = temp_mean, y = temp_sd)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "mean_temp",
    y = "ssd",
    title = "thermal variability and mean temp"
  )
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
ggplot(data = marine_sst %>% 
         filter(thermal_tolerance_TF == TRUE),
       aes(x = sst_sd, y = thermal_tolerance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal tolerance breadth",
    title = "marine tolerance breadth and variability"
  )
###the data really isn't normally distributed
var_Tbreadth_model <- lme(thermal_tolerance ~ sst_sd,
                         data = marine_sst %>%
                           filter(thermal_tolerance_TF == TRUE),
                         random = ~ 1|study_ID)
plot(residuals(var_Tbreadth_model))
qqnorm(resid(var_Tbreadth_model))
qqline(resid(var_Tbreadth_model))
hist(resid(var_Tbreadth_model))
summary(var_Tbreadth_model)

#performance breadth should increase as variability increases
ggplot(data = marine_sst %>% 
         filter(breadth_TF == TRUE),
       aes(x = sst_sd, y = breadth)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "Thermal variability (temp sd)",
    y = "thermal performance breadth",
    title = "marine performance Breadth and Variability"
  )

var_breadth_model <- lme(breadth ~ sst_sd,
                         data = marine_sst %>%
                           filter(breadth_TF == TRUE),
                         random = ~ 1|study_ID)
#resids have long right tail, also not sig
plot(residuals(var_breadth_model))
qqnorm(resid(var_breadth_model))
qqline(resid(var_breadth_model))
hist(resid(var_breadth_model))
summary(var_breadth_model)



## deutsch warming tolerance - the difference between ctmax and mean env. temp
freshwater_temps <- freshwater_temps %>%
  mutate(warming_tolerance = ctmax - temp_mean) %>%
  mutate(thermal_safety_margin_duetsch = topt - temp_mean)

marine_sst <- marine_sst %>%
  mutate(warming_tolerance = ctmax - sst_mean) %>%
  mutate(thermal_safety_margin_duetsch = topt - sst_mean)

##does how close your topt is to your env temp depend on latitude???? ### 
freshwater_temps <- freshwater_temps %>%
  mutate(diff_max = temp_max - topt) %>% 
  mutate(diff_mean = temp_mean - topt) 
marine_sst <- marine_sst %>%
  mutate(diff_max = sst_max - topt) %>% 
  mutate(diff_mean = sst_mean - topt) 

###topt should be closer to mean water temp in the tropics (ie mag should decrease with abs. latitude), where temps are higher (out of the tropics hyp)
ggplot(data = freshwater_temps %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = diff_mean)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "mean env temp - topt",
    title = "freshwater: relationship between topt and mean temp with latitude"
  )
dif_model <- lme(abs_latitude ~ diff_mean,
                 data = freshwater_temps %>%
                   filter(topt_TF == TRUE),
                 random = ~ 1|study_ID)
plot(residuals(dif_model))
qqnorm(resid(dif_model))
qqline(resid(dif_model))
summary(dif_model) #significant

ggplot(data = marine_sst %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = diff_mean)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "mean env temp - topt",
    title = "marine: relationship between topt and mean temp with latitude"
  )
dif_model <- lme(abs_latitude ~ diff_mean,
                 data = marine_sst %>%
                   filter(topt_TF == TRUE),
                 random = ~ 1|study_ID)
plot(residuals(dif_model))
qqnorm(resid(dif_model))
qqline(resid(dif_model))
summary(dif_model) #not rlly in marine, but yes in freshwater

ggplot(data = marine_sst %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = diff_max)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "max env temp - topt",
    title = "marine: relationship between topt and max temp with latitude"
  )
dif_max_model <- lme(abs_latitude ~ diff_max,
                 data = marine_sst %>%
                   filter(topt_TF == TRUE),
                 random = ~ 1|study_ID)
plot(residuals(dif_max_model))
qqnorm(resid(dif_max_model))
qqline(resid(dif_max_model))
summary(dif_max_model) #no

##okay so how close topt is to envir. temp decreases with latitude in freshwater fish

# warming tolerance - if topt is closer to env mean in tropics, warming tolerance should increase with lat
ggplot(data = freshwater_temps %>%
         filter(thermal_max_TF == TRUE),
       aes(x = abs_latitude, y = warming_tolerance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "warming tolerance",
    title = "freshwater: warming tolerance and latitude"
  )
ggplot(data = marine_sst %>%
         filter(thermal_max_TF == TRUE),
       aes(x = abs_latitude, y = warming_tolerance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "warming tolerance",
    title = "marine: warming tolerance and latitude"
  )

### tsm should increase with latitude (deutsch), because fish in tropics are already living close to their physiological optimum
ggplot(data = freshwater_temps %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = thermal_safety_margin_duetsch)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "TSM",
    title = "freshwater: thermal safety margin and absolute latitude"
  )


TSM_model <- lme(abs_latitude ~ thermal_safety_margin_duetsch,
                     data = freshwater_temps %>%
                       filter(topt_TF == TRUE),
                     random = ~ 1|study_ID)

plot(residuals(TSM_model))
qqnorm(resid(TSM_model))
qqline(resid(TSM_model))
summary(TSM_model)
r2(TSM_model)
Anova(TSM_model) #significant

#topt and enviornmental temp
dif_fresh <- freshwater_temps %>%
  pivot_longer(
    cols = c(diff_max, diff_mean),
    names_to = "diff_type",
    values_to = "diff_value"
  )
ggplot(dif_fresh %>% filter(topt_TF == TRUE)) +
  geom_boxplot(aes(x = diff_type, y = diff_value))

dif_marine <- marine_sst %>%
  pivot_longer(
    cols = c(diff_max, diff_mean),
    names_to = "diff_type",
    values_to = "diff_value"
  )
ggplot(dif_marine %>% filter(topt_TF == TRUE)) +
  geom_boxplot(aes(x = diff_type, y = diff_value))


#topt will be more dif from sst max in enviornemnts with greater thermal variability -- ie mag will increase with sst var
#freshwater
freshwater_temps_topt <- freshwater_temps %>%
  filter(topt_TF == TRUE)
freshwater_temps_topt <- freshwater_temps_topt %>%
  mutate(abs_diff_max = abs(diff_max))
freshwater_temps_topt <- freshwater_temps_topt %>%
  mutate(abs_diff_mean = abs(diff_mean))
##predic
ggplot(data = freshwater_temps_topt,
       aes(x = temp_sd, y = abs_diff_mean)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "thermal variability (temp sd)",
    y = "abs (mean temp - topt)",
    title = "thermal variation magnitude of dif between topt and mean temp"
  )
ggplot(data = freshwater_temps_topt,
       aes(x = temp_sd, y = abs_diff_max)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "thermal variability (temp sd)",
    y = "abs (mean max - topt)",
    title = "thermal variation magnitude of dif between topt and max temp"
  )
sd_dif_max <- lme(temp_sd ~ abs_diff_max,
                 data = freshwater_temps_topt %>%
                   filter(topt_TF == TRUE),
                 random = ~ 1|study_ID)
plot(residuals(sd_dif_max))
qqnorm(resid(sd_dif_max))
qqline(resid(sd_dif_max))
hist(resid(sd_dif_max)) ## not normal
summary(sd_dif_max)

#marine
marine_sst_topt <- marine_sst %>%
  filter(topt_TF == TRUE)
marine_sst_topt <- marine_sst_topt %>%
  mutate(abs_diff_max = abs(diff_max))
marine_sst_topt <- marine_sst_topt %>%
  mutate(abs_diff_mean = abs(diff_mean))

#topt will be more dif from sst max in enviornemnts with greater thermal variability -- ie mag will increase with sst var
##in more thermally stable env. topt will be closer to max temp to maximize performance and in more variable enviornments, topt will be less than max temp because suboptimal opt   
ggplot(data = marine_sst_topt,
       aes(x = sst_sd, y = diff_max)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "SST variability (SST sd)",
    y = "max temp - topt",
    title = "marine: thermal variation and topt in relation to max sst"
  )

ggplot(data = marine_sst_topt, 
       aes(x = sst_sd, y = diff_mean)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "SST variability (SST sd)", 
    y = "mean temp - topt",
    title = "marine: thermal variation and topt in relation to mean sst"
  )
#direction
varmean <- (lmer(diff_mean ~ sst_sd + (1 | study_ID), data = marine_sst_topt)) #expect dif between sstmean and topt to scale with thermal variation
varmax <- (lmer(diff_max ~ sst_sd + (1 | study_ID), data = marine_sst_topt))

plot(residuals(varmean))
qqnorm(resid(varmean))
qqline(resid(varmean))
hist(resid(varmean))
summary(varmean)
r2(varmean)
Anova(varmean) #significant

plot(residuals(varmax))
qqnorm(resid(varmax))
qqline(resid(varmax))
hist(resid(varmax))
summary(varmax)
r2(varmax)
Anova(varmax) #significant

#dif between mean sst and topt is more related to thermal variability than dif between max sst and topt
AIC(varmean, varmax) 
anova(varmean, varmax)
##when i do abs value of dif it is more significant...

library(ggeffects)
# Predict Topt offset from mean SST over sst_sd range
pred_dif_sstsd_mean <- ggpredict(varmean, terms = "sst_sd")
pred_dif_sstsd_max <- ggpredict(varmax, terms = "sst_sd")
pred_dif_sstsd_mean <- as.data.frame(pred_dif_sstsd_mean)
pred_dif_sstsd_max <- as.data.frame(pred_dif_sstsd_max)


ggplot(marine_sst_topt) +
  geom_point(aes(x = sst_sd, y = diff_mean), color = "blue", alpha = 0.5) +
  geom_line(data = pred_dif_sstsd_mean, aes(x = x, y = predicted), color = "blue", linewidth = 1.2) + 
  geom_ribbon(data = pred_dif_sstsd_mean, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) +
  geom_point(aes(x = sst_sd, y = diff_max), color = "red", alpha = 0.5) +
  geom_line(data = pred_dif_sstsd_max, aes(x = x, y = predicted), color = "red", linewidth = 1.2) + 
  geom_ribbon(data = pred_dif_sstsd_max, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "red", alpha = 0.2) +
  labs(
    x = "SST_SD",
    y = "SST- topt",
    title = "Thermal variability and magnitude of difference \n between topt and SST",
    subtitle = "Blue = Mean SST, Red = Max SST"
  ) +
  theme_minimal()




