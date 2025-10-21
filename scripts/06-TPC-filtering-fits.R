#### script for filtering models and datasets for analysis ####
#### load packages and data ####
####have not done this script yet, left off with the fitted valid data that i have not figured out how to filter yet ###
rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(ggforce)
library(tidyverse)
curves <- readRDS(here('processed-data', 'wild_tpcs_data_coverage_sorted.RdS'))
model_preds <- readRDS(here('processed-data', 'all_model_predictions_01_10_25.RDS'))
params <- readRDS(here('processed-data', 'all_model_params_01_10_25.RdS'))
model_evaluations <- readRDS(here('processed-data', 'model_fit_evaluations_01_10_25.RDS'))
length(unique(model_preds$curve_ID)) #422

curves_sd <- curves %>%
  group_by(curve_ID) %>%
  mutate(sd_response = sd(response_value, na.rm = TRUE),
         min_1sd = min(response_value, na.rm = TRUE) - sd_response,
         max_1sd = max(response_value, na.rm. = TRUE) + sd_response,
         min_temp = min(test_temp, na.rm = TRUE),
         max_temp = max(test_temp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(curve_ID = as.numeric(curve_ID)) %>%
  select(curve_ID, response_value, test_temp, sd_response, max_1sd, min_1sd, min_temp, max_temp, dataset_type, thermal_min_TF, thermal_max_TF) 
#Attach bounds to fitted data ###
model_preds_with_bounds <- model_preds %>%
  left_join(
    curves_sd %>% distinct(curve_ID, response_value, test_temp, sd_response, max_1sd, min_1sd, min_temp, max_temp, dataset_type, thermal_min_TF, thermal_max_TF),
    by = "curve_ID"
  )
#filter valid models within 1 SD of raw data and get valid models/preds ###
valid_models <- model_preds_with_bounds %>%
  group_by(curve_ID, model) %>%
  summarise(valid = all(.fitted >= min_1sd & .fitted <= max_1sd), .groups = "drop") %>%
  filter(valid) %>%
  select(-valid) %>%
  filter(model != "ratkowsky") # this model is behaving weirdly

###also want a filter that is if the model predicts ctmin or ctmax to be more than 5 degrees on the x away from the min temp tested and max temp tested
# both for full curve ones
#for ones with ctmin  - just ctmin needs to be within 5
#for ones with a ctmax - jsut ctmax needs to be within 5
valid_models <- valid_models %>%
  left_join(
    params %>%
      distinct(curve_ID, model, ctmin, ctmax),  # assuming these columns exist
    by = c("curve_ID", "model")
  ) %>%
  left_join(
    curves_sd %>% distinct(curve_ID, min_temp, max_temp, dataset_type, thermal_min_TF, thermal_max_TF),
    by = "curve_ID"
  ) %>%
  filter(
    # Keep models where:
    (
      # if only thermal_min == TRUE
      thermal_min_TF == TRUE & thermal_max_TF != TRUE & ctmin >= (min_temp - 5)
    ) |
      (
        # if only thermal_max == TRUE
        thermal_max_TF == TRUE & thermal_min_TF != TRUE & ctmax <= (max_temp + 5)
      ) |
      (
        # if both are TRUE
        thermal_min_TF == TRUE & thermal_max_TF == TRUE &
          ctmin >= (min_temp - 5) & ctmax <= (max_temp + 5)
      ) |
      (
        # if neither are TRUE, keep everything (don’t filter)
        thermal_min_TF != TRUE & thermal_max_TF != TRUE
      )
  )
### we lost some curveIDs
length(unique(valid_models$curve_ID)) # go from #421 to #414 datasets when i filter out the SD
not_curves <- curves %>%
  select(curve_ID) %>%
  filter(!(curve_ID %in% valid_models$curve_ID)) %>%
  distinct() # lost = 51  62 125  26  25  33  70
valid_preds <- model_preds %>%
  semi_join(valid_models, by = c("curve_ID", "model"))
rm(model_preds)
rm(model_preds_with_bounds)
valid_model_evaluations <- model_evaluations %>%
  inner_join(valid_models, by = c("curve_ID", "model"))
valid_params <- params %>%
  inner_join(valid_models %>% select(model, curve_ID), by = c("curve_ID", "model"))
rm(model_evaluations)
rm(params)
#### get top 2 models for each dataset? ####
top_models <- valid_model_evaluations %>%
  group_by(curve_ID) %>%
  arrange(AIC, .by_group = TRUE) %>%  
  slice_head(n = 2) %>%          
  ungroup()

top_model <- top_models %>%
  group_by(curve_ID) %>%
  arrange(AIC, .by_group = TRUE) %>%  
  slice_head(n = 1) %>%          
  ungroup()
second_top_model <- top_models %>%
  group_by(curve_ID) %>%
  arrange(-AIC, .by_group = TRUE) %>%  
  slice_head(n = 1) %>%          
  ungroup()
top_model_preds <- valid_preds %>%
  inner_join(top_model %>% select(curve_ID, model), by = c("curve_ID", "model")) %>%
  left_join(curves %>% select(curve_ID, dataset_type), join_by(curve_ID)) %>%
  distinct()
second_top_model_preds <- valid_preds %>%
  inner_join(second_top_model %>% select(curve_ID, model), by = c("curve_ID", "model")) %>%
  left_join(curves %>% select(curve_ID, dataset_type), join_by(curve_ID)) %>%
  distinct()
top_params <- valid_params %>%
  inner_join(top_models %>% select(curve_ID, model), by = c("curve_ID", "model")) %>%
  left_join(curves %>% select(curve_ID, dataset_type), join_by(curve_ID)) %>%
  distinct()
best_param <- top_params %>%
  inner_join(top_model %>% select(curve_ID, model), by = c("curve_ID", "model"))

##breadth##
breadth_curves <- curves %>%
  filter(dataset_type == "topt") %>%
  left_join(best_param %>% select(curve_ID, topt, y_value_topt), by = "curve_ID") %>%
  group_by(curve_ID) %>%
  mutate(
    thresh_80 = 0.8 * y_value_topt,
    below_topt = test_temp < topt,
    above_topt = test_temp > topt,
    has_below = any(response_value[below_topt] < unique(thresh_80), na.rm = TRUE),
    has_above = any(response_value[above_topt] < unique(thresh_80), na.rm = TRUE),
    usable_for_breadth = has_below & has_above
  ) %>%
  ungroup() %>%
  filter(usable_for_breadth)
breadth_topt <- unique(breadth_curves$curve_ID) #64 of the topt curves can be used to get breadth to, while 59 of them cannot
###adding cols to curves ###
curves <- curves %>%
  mutate(
    thermal_tolerance_TF = dataset_type == "full_curve",
    thermal_safety_margin_TF = dataset_type %in% c("full_curve", "right_bound_withopt"),
    breadth_TF = curve_ID %in% breadth_topt | dataset_type == "full_curve"
  )
## add these cols to the other dfs
top_model_preds <- top_model_preds %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF), join_by(curve_ID)) %>%
  distinct()
second_top_model_preds <- second_top_model_preds %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF), join_by(curve_ID)) %>%
  distinct()
top_params <- top_params %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF), join_by(curve_ID)) %>%
  distinct()
best_param <- best_param %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF), join_by(curve_ID)) %>%
  distinct()





responses <- curves %>%
  select(curve_ID, response_type, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(response_type, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()

#### TOPT ####
ggplot() +
  geom_point(data = curves %>%
               filter(dataset_type == "right_bound_withopt"),
             aes(x = test_temp, y = response_value)) +
  geom_point(data = best_param %>%
               filter(dataset_type == "right_bound_withopt"),
             aes(x = topt, y = y_value_topt, color = model)) +
   # geom_point(data = best_param %>%
   #             filter(thermal_tolerance_TF == TRUE),
   #           aes(x = ctmin, y = y_value_ctmin, color = model)) +
   # geom_point(data = best_param %>%
   #              filter(thermal_tolerance_TF == TRUE),
   #           aes(x = ctmax, y = y_value_ctmax, color = model)) +
  geom_line(data = top_model_preds %>%
              filter(dataset_type == "right_bound_withopt"),
            aes(x = test_temp, y = .fitted, color = model), linewidth = 1) + geom_line(data = second_top_model_preds %>%                        
                                                                                         filter(dataset_type == "right_bound_withopt"),
            aes(x = test_temp, y = .fitted, color = model), linewidth = .5) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
                      labeller = labeller(curve_ID = curve_labels)) +
  scale_color_manual(
    values = c(
      "johnsonlewin" = "slateblue", 
      "lactin2" = "#4DAF4A",  
      "oneill"= "magenta", 
      "ratkowsky" = "yellow",  
      "rezende" = "#A65628",  
      "spain" = "royalblue3",  
      "thomas" = "#999999",  
      "weibull" = "black"  ,
      "hinshelwood" = "aquamarine",
      "briere" = "lightblue", 
      "gaussian" = "maroon",
      "quadratic" = "green"
    )
  ) +
  theme_minimal() +
  labs(x = "Test Temperature", y = "Response", color = "Model")


data_types_his <- best_param %>%
  select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF)  %>%
  left_join(curves %>% select(curve_ID, n_unique_temps)) %>%
  distinct()
data_types_his <- data_types_his %>%
  mutate(n_unique_temps = case_when(
    n_unique_temps >= 7 ~ "7+",
    TRUE ~ as.character(n_unique_temps)  # keep the original value otherwise
  ))
data_types_his <- data_types_his %>%
  mutate(none_TF = !if_any(thermal_min_TF:thermal_safety_margin_TF, ~ .x))

data_types_long <- data_types_his %>%
  pivot_longer(
    cols = c(thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF,
             thermal_tolerance_TF, thermal_safety_margin_TF, none_TF),
    names_to = "parameter",
    values_to = "has_param"
  )
param_counts_summary <- data_types_long %>%
  filter(has_param == TRUE) %>%
  count(parameter, n_unique_temps)


### start here, sorted data by what param curve covers ###

ggplot(param_counts_summary, aes(x = reorder(parameter, -n), y = n, fill = as.factor(n_unique_temps))) +
  geom_col(position = "stack") +
  labs(
    x = "Parameter",
    y = "Datasets",
    fill = "Unique Temps",
    title = "Count of fitted datasets with Each Thermal Parameter"
  ) +
  theme_minimal() +
  scale_x_discrete(
    labels = c(
      "topt_TF" = "Topt",
      "none_TF" = "No curve params",
      "thermal_max_TF" = "Tmax",
      "breadth_TF" = "Thermal Breadth",
      "thermal_min_TF" = "Tmin",
      "thermal_safety_margin_TF" = "Thermal Safety Margin",
      "thermal_tolerance_TF" = "Thermal Tolerance"
    )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##independent treatments
#### parameter analysis #### don't run this again
params_with_curve_info <- best_param %>%
  left_join(curves %>% select(curve_ID, study_ID, habitat_water, habitat, abs_latitude, latitude, longitude, response_type, response_unit, response_type_group, land_or_sea, treatment_1_group), join_by(curve_ID)) %>%
  distinct()
params_with_curve_info$response_type_group <- as.factor(params_with_curve_info$response_type_group)
library(car)
install.packages("performance")
library(performance)
#### does topt increase with latitude?? #### 
topt <- params_with_curve_info %>%
  filter(topt_TF == TRUE) %>%
  filter(!(is.na(abs_latitude)))
length(unique(topt$study_ID))

#is my topt equally distributed?
hist((params_with_curve_info %>% 
       filter(topt_TF == TRUE) %>%
       filter(!(is.na(abs_latitude))))$topt)
# scatter of topt and lat
ggplot(data = params_with_curve_info %>% 
         filter(topt_TF == TRUE) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = topt, color = land_or_sea)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Absolute Latitude",
    y = "Thermal Optimum",
    title = "Scatter of Topt and latitude"
  )

#linear mixed effects model with study_ID as random effect
lm_model <- lmer(topt ~ abs_latitude + (1 | study_ID), 
                  data = params_with_curve_info %>%
                    filter(topt_TF == TRUE, !is.na(abs_latitude)))
install.packages("lmerTest")
library(lmerTest)
summary(lm_model)
r2(lm_model)

plot(residuals(lm_model))
qqnorm(resid(lm_model))
qqline(resid(lm_model))
#try releveling, take in dif orders so one of three major ones is base
lm_model2 <- lmer(topt ~ abs_latitude * land_or_sea + (1 | study_ID), 
                 data = params_with_curve_info %>%
                   filter(topt_TF == TRUE, !is.na(abs_latitude)))
summary(lm_model2)
r2(lm_model2)
plot(residuals(lm_model2))
qqnorm(resid(lm_model2))
qqline(resid(lm_model2))

preds <- ggpredict(lm_model, terms = "abs_latitude")
head(preds)
ggplot() +
  geom_point(data = params_with_curve_info %>%
               filter(topt_TF == TRUE, !is.na(abs_latitude)),
             aes(x = abs_latitude, y = topt),
             alpha = 0.5) +
  geom_line(data = preds, aes(x = x, y = predicted), color = "blue", size = 1.2) +
  geom_ribbon(data = preds,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "blue", alpha = 0.2) +
  labs(x = "Absolute latitude (°)", y = "Thermal optimum (°C)",
       title = "Effect of latitude on Topt (random intercept by study)") +
  theme_minimal(base_size = 14)


#does max temp change with study, does temp range 
##is there still association between lat and topt after we control for variation of median, min, and max test tested
mixed.lmer <- lmer(topt ~ abs_latitude + (1|max_temp), data = topt)
summary(mixed.lmer)
plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!


#### what about breadth #### 
# scatter of topt and lat
ggplot(data = params_with_curve_info %>% 
         filter(breadth_TF == TRUE) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = breadth, color = land_or_sea)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "Absolute Latitude",
    y = "Thermal breadth",
    title = "Scatter of Topt and latitude"
  )


ggplot(data = params_with_curve_info %>% 
         filter(thermal_tolerance_TF == TRUE) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = thermal_tolerance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "Absolute Latitude",
    y = "Thermal tolerance",
    title = "Scatter of tolerance and latitude"
  )

saveRDS(params_with_curve_info, file = here("processed-data", "sorted_datasets_withparams.RDS"))
