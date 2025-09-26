#### 
#sort - after join with valid preds -- group by curve_id and within that sort ot find best model
rss_5_param_models <- rss_5_param_models %>%
  mutate(best_model = pmap_chr(select(., deutsch_rss:weibull_rss),
                               ~ names(list(...))[which.min(c(...))]))
best_models <- rss_5_param_models %>%
  select(curve_ID, best_model) %>%
  mutate(best_model = gsub("_rss$", "", best_model))


#top 3 models for each curve
rss_top3_long <- rss_5_param_models %>%
  pivot_longer(cols = deutsch_rss:weibull_rss,
               names_to = "model",
               values_to = "rss") %>%
  group_by(curve_ID) %>%
  arrange(rss, .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  mutate(curve_ID = as.numeric(curve_ID))
rss_top3_long <- rss_top3_long %>%
  mutate(model = gsub("_rss$", "", model))





all_preds_top3 <- all_preds_long %>%
  inner_join(rss_top3_long %>% select(curve_ID, model), by = c("curve_ID", "model"))
all_preds_long_best <- all_preds_long %>%
  semi_join(best_models, by = c("curve_ID", "model" = "best_model"))
all_preds_top_3_metabolism <- all_preds_top3 %>%
  filter(response_type_group == "metabolism")
length(unique(high_res_ds_metabolism$curve_ID)) #44
high_res_ds_metabolism <- high_res_ds %>%
  filter(response_type_group == "metabolism")

all_preds_top_3_growth <- all_preds_top3 %>%
  filter(response_type_group == "growth")
length(unique(high_res_ds_growth$curve_ID)) #63
high_res_ds_growth <- high_res_ds %>% 
  filter(response_type_group == "growth")

responses <- curves %>%
  select(curve_ID, response_type, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(response_type, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()
library(ggforce)
#split into response types 
ggplot() +
  geom_point(data = high_res_ds %>% filter(response_type_group == "reproduction"), aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds_top3 %>% filter(response_type_group == "reproduction"), aes(x = test_temp, y = .fitted, color = model), linewidth = .5) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 3, page =2,
                      labeller = labeller(curve_ID = curve_labels)) +
  scale_color_manual(
    values = c(
      "deutsch" = "#E41A1C",  # red
      "johnsonlewin" = "slateblue",  # blue
      "lactin2" = "#4DAF4A",  
      "modifiedgaussian" = "#FF7F00",  # orange
      "oneill"= "magenta",  # purple
      "ratkowsky" = "yellow",  
      "rezende" = "#A65628",  
      "spain" = "royalblue3",  
      "thomas" = "#999999",  
      "weibull" = "black"  ,
      "hinshelwood" = "aquamarine",
      "briere" = "lightblue"
    )
  ) +
  theme_minimal() +
  labs(x = "Test Temperature", y = "Response", color = "Model")


ggplot() +
  geom_point(data = high_res_ds %>% filter(curve_ID == "318"), aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds_top3 %>% filter(curve_ID == "318") %>% filter(model != "ratkowsky" ), aes(x = test_temp, y = .fitted, color = model), linewidth = .5) +
  scale_color_manual(
    values = c(
      "deutsch" = "#E41A1C",  # red
      "johnsonlewin" = "slateblue",  # blue
      "lactin2" = "#4DAF4A",  
      "modifiedgaussian" = "#FF7F00",  # orange
      "oneill"= "magenta",  # purple
      "ratkowsky" = "yellow",  
      "rezende" = "#A65628",  
      "spain" = "royalblue3",  
      "thomas" = "#999999",  
      "weibull" = "black"  ,
      "hinshelwood" = "aquamarine",
      "briere" = "lightblue"
    )
  ) +
  theme_minimal() +
  labs(x = "Test Temperature", y = "Response", color = "Model")



####filtering datasets to how much of a curve they cover (curve coverage?), using topt####
#add topt to highrescurvedf so we can filter with it
topt <- all_params_deutsch_2008_highres %>%
  select(topt, curve_ID)
high_res_ds_fitted <- left_join(high_res_ds, topt, join_by(curve_ID))
#classify what side of the curve a point is on
high_res_ds_fitted <- high_res_ds_fitted %>%
  group_by(curve_ID) %>%
  mutate(
    side = case_when(
      test_temp < topt ~ "increasing",
      test_temp > topt ~ "decreasing",
      TRUE ~ "opt"
    ),
    n_increasing = sum(side == "increasing"),
    n_decreasing = sum(side == "decreasing"),
    curve_symmetry = case_when(
      n_increasing >= 2 & n_decreasing >= 2 ~ "full curve",
      n_increasing >= 1 & n_decreasing >= 1 ~ "full curve_ish",
      n_increasing > 0 & n_decreasing == 0 ~ "increasing only",
      n_increasing == 0 & n_decreasing > 0 ~ "decreasing only",
      TRUE ~ "incomplete"
    )
  ) %>%
  ungroup()

increasing_side <- high_res_ds_fitted %>%
  filter(curve_symmetry == "increasing only")
length(unique(increasing_side$curve_ID)) #27
decreasing_side <- high_res_ds_fitted %>%
  filter(curve_symmetry == "decreasing only")
print(unique(decreasing_side$curve_ID)) #5
full_curves <- high_res_ds_fitted %>%
  filter(curve_symmetry == "full curve")
print(unique(full_curves$curve_ID)) #120
full_curves_ish <- high_res_ds_fitted %>%
  filter(curve_symmetry == "full curve_ish")
length(unique(full_curves_ish$curve_ID)) #16
#### plot some with raw data, fitted data, and params to see ####
ggplot() + 
  geom_point(
    data = high_res_ds_fitted %>% filter(curve_ID == "332"), 
    aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds_deutsch_2008_highres %>% filter(curve_ID == "332"),
            aes(x = test_temp, y = .fitted), 
            linewidth = 1) +
  geom_point(data = all_param_points_deutsch_2008_highres %>% filter(curve_ID == "332"),
             aes(x = test_temp, y = y_value),
             size = 3, color = "red")


#### overlaying some quality deutsch curves ####
#filter to those where RSS is less than 0.01 (ask jenn about this, not sure if this is arbitrary)
good_fit_deutsch <- rss_deutsch_2008_highres %>%
  filter(RSS <= 0.01) %>%
  filter(!curve_ID %in% c("223", "210", "89", "332")) %>% #datasets where deutsch brings the curve down / fits really weird because the equation needs a ctmax
  pull(curve_ID) #25


good_fits_deutsch_preds <- all_preds_deutsch_2008_highres %>%
  filter(curve_ID %in% good_fit_deutsch)
length(unique(good_fits_deutsch_preds$curve_ID)) #25
topt_deutsch_2008 <- all_params_deutsch_2008_highres %>%
  select(topt, curve_ID)
good_fits_deutsch_preds <- left_join(good_fits_deutsch_preds, topt_deutsch_2008, join_by(curve_ID))
length(unique(good_fits_deutsch_preds$curve_ID)) #25
info <- curves %>%
  select(curve_ID, curve_type, latitude, longitude, response_type_group, response_type)
good_fits_deutsch_preds <- left_join(good_fits_deutsch_preds, info, join_by(curve_ID))
#normalize resp
good_fits_deutsch_preds <- good_fits_deutsch_preds %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = .fitted / max(.fitted)) %>%
  ungroup()
length(unique(good_fits_deutsch_preds$curve_ID)) #25
#plot
ggplot(good_fits_preds, aes(x = test_temp, y = response_scaled, color = response_type_group, group = curve_ID)) +
  geom_line(linewidth = .5) +
  theme_minimal() +
  labs(x = "Temperature (C)",
       y = "Normalized Response",
       title = "deutsch 5+ good RSS curves") +
  theme(legend.position = "bottom")
#25 curves (out of the 200...)

#### 3 parameter models for datasets with 4 points ####
#any with 4 points will be attempted with as many models on rtpc that fit 4 points 
# only 3 models have 3 parameters -- flinn_1991, gaussian_1989, and quadratic_2008
curve_ids <- unique(low_res_ds$curve_ID)
#### first trying flinn_1991 ####
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- low_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'flinn_1991')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 1
  start_upper <- sv + 1
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'flinn_1991')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'flinn_1991')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ flinn_1991(temp = test_temp, a, b, c),
      data = curve_data,
      iter = c(4,4,4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    
    params_list[[i]] <- tibble()
    preds_list[[i]] <- tibble()
    param_points_list[[i]] <- tibble()
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  

all_params_flinn_1991_lowres <- bind_rows(params_list, .id = "list_id")
all_preds_flinn_1991_lowres <- bind_rows(preds_list, .id = "list_id")
all_param_points_flinn_1991_lowres <- bind_rows(param_points_list, .id = "list_id")
##evaluating flinn_1991 model fit##
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_flinn_1991_lowres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
rss_flinn_1991_lowres <- rss_flinn_1991_lowres %>%
  rename(RSS_flinn = RSS)

#### gaussian_1984 ####
curve_ids <- unique(low_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- low_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'gaussian_1987')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'gaussian_1987')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'gaussian_1987')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ gaussian_1987(temp = test_temp, rmax, topt, a),
      data = curve_data,
      iter = c(4,4,4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    
    params_list[[i]] <- tibble()
    preds_list[[i]] <- tibble()
    param_points_list[[i]] <- tibble()
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  

all_params_gaussian_1987_lowres <- bind_rows(params_list, .id = "list_id")
all_preds_gaussian_1987_lowres <- bind_rows(preds_list, .id = "list_id")
all_param_points_gaussian_1987_lowres <- bind_rows(param_points_list, .id = "list_id")
##evaluating gaussian_1987 model fit##
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_gaussian_1987_lowres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list)
rss_gaussian_1987_lowres <- rss_gaussian_1987_lowres %>%
  rename(RSS_gaussian = RSS)
#### fit with quadratic_2008####
curve_ids <- unique(low_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- low_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'quadratic_2008')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'quadratic_2008')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'quadratic_2008')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ gaussian_1987(temp = test_temp, a, b, c),
      data = curve_data,
      iter = c(4,4,4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    
    params_list[[i]] <- tibble()
    preds_list[[i]] <- tibble()
    param_points_list[[i]] <- tibble()
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #1 failed fit

all_params_quadratic_2008_lowres <- bind_rows(params_list, .id = "list_id")
all_preds_quadratic_2008_lowres <- bind_rows(preds_list, .id = "list_id")
all_param_points_quadratic_2008_lowres <- bind_rows(param_points_list, .id = "list_id")
##evaluating gaussian_1987 model fit##
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_quadratic_2008_lowres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
) %>%
  rename(RSS_quadratic = RSS)









ggplot() + 
  geom_point(
    data = low_res_ds %>% filter(curve_ID == "371"), 
    aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds_flinn_1991_lowres %>% filter(curve_ID == "371"),
            aes(x = test_temp, y = .fitted), 
            linewidth = 1) +
  geom_point(data = all_param_points_flinn_1991_lowres %>% filter(curve_ID == "371"),
             aes(x = test_temp, y = y_value),
             size = 3, color = "red")
#### combine RSS for 3 parameter models to see which model is best for each curve ####
rss_4_param_models <- left_join(rss_flinn_1991_lowres, rss_quadratic_2008_lowres, by = "curve_ID")
rss_4_param_models <- left_join(rss_4_param_models, rss_gaussian_1987_lowres, by = "curve_ID")
rss_4_param_models <- rss_4_param_models %>%
  mutate(best_model = names(select(., RSS_flinn:RSS_gaussian))[max.col(-select(., RSS_flinn:RSS_gaussian))])
flinn_bestrss <- rss_4_param_models %>%
  filter(best_model == "RSS_flinn")
gaussian_bestrss <- rss_4_param_models %>%
  filter(best_model == "RSS_gaussian")
quadratic_bestrss <- rss_4_param_models %>%
  filter(best_model == "RSS_quadratic")
print(unique(quadratic_bestrss$curve_ID))

ggplot() + 
  geom_point(
    data = curves %>% filter(curve_ID == "11"), 
    aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds_oneill_1972_highres %>% filter(curve_ID == "11"),
            aes(x = test_temp, y = .fitted), 
            linewidth = 1)


good_fit_deutsch_curve_IDs <- c("351")
good_fit_briere2_curve_IDs <- "311"
good_fit_quadratic_curve_IDs <- c("206", "208", "198", "236","269","390", '392', "392")
good_fit_gaussian_curve_IDs <- c("285","288", "289", "246", "393", "194","397", "377", "391", "33", "267", "276", "249","229", "180","179","174", "362","361","205", "370", "130", "173", "371", "18", "163", "99", "69", "67", "98", "108", "96", "106", "104", "37")
good_fit_flinn_curve_IDs <- c("319", "324", "344", "372", "15", "16", "44", "45", "47", "42", "43", "97", "102", "94", "114", "109", "110", "129", "147", "151", "204", "203", "196", "195", "200", "197", "201", "31", "216", "237", "235", "233", "279", "272", "274", "296", "394", "395", "413")
not_opt_datasets <- curves %>%
  filter(!(curve_ID %in% c(
    good_fit_quadratic_curve_IDs,
    good_fit_gaussian_curve_IDs,
    good_fit_flinn_curve_IDs,
    good_fit_deutsch
  )))
###flinn###
good_fits_flinn_1991_preds <- all_preds_flinn_1991_lowres %>%
  filter(curve_ID %in% good_fit_flinn_curve_IDs)
length(unique(good_fits_flinn_1991_preds$curve_ID)) #39
topt_flinn <- all_params_flinn_1991_lowres %>%
  select(topt, curve_ID)
good_fits_flinn_1991_preds <- left_join(good_fits_flinn_1991_preds, topt_flinn, join_by(curve_ID))
info <- curves %>%
  select(curve_ID, curve_type, latitude, longitude, response_type_group, response_type)
good_fits_flinn_1991_preds <- left_join(good_fits_flinn_1991_preds, info, join_by(curve_ID))
good_fits_flinn_1991_preds <- good_fits_flinn_1991_preds %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = .fitted / max(.fitted)) %>%
  ungroup()
length(unique(good_fits_flinn_1991_preds$curve_ID)) #39

###gaus###
good_fits_gaussian_preds <- all_preds_gaussian_1987_lowres %>%
  filter(curve_ID %in% good_fit_gaussian_curve_IDs)
length(unique(good_fits_gaussian_preds$curve_ID)) #35
topt_gaus <- all_params_gaussian_1987_lowres %>%
  select(topt, curve_ID)
good_fits_gaussian_preds <- left_join(good_fits_gaussian_preds, topt_gaus, join_by(curve_ID))
info <- curves %>%
  select(curve_ID, curve_type, latitude, longitude, response_type_group, response_type)
good_fits_gaussian_preds <- left_join(good_fits_gaussian_preds, info, join_by(curve_ID))
good_fits_gaussian_preds <- good_fits_gaussian_preds %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = .fitted / max(.fitted)) %>%
  ungroup()
length(unique(good_fits_gaussian_preds$curve_ID)) #35

###quadratic###
good_fits_quadratic_preds <- all_preds_quadratic_2008_lowres %>%
  filter(curve_ID %in% good_fit_quadratic_curve_IDs)
topt_quad <- all_params_quadratic_2008_lowres %>%
  select(topt, curve_ID)
good_fits_quadratic_preds <- left_join(good_fits_quadratic_preds, topt_quad, join_by(curve_ID))
info <- curves %>%
  select(curve_ID, curve_type, latitude, longitude, response_type_group, response_type)
good_fits_quadratic_preds <- left_join(good_fits_quadratic_preds, info, join_by(curve_ID))
good_fits_quadratic_preds <- good_fits_quadratic_preds %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = .fitted / max(.fitted)) %>%
  ungroup()
length(unique(good_fits_quadratic_preds$curve_ID)) #7

ggplot(good_fits_gaussian_preds, aes(x = test_temp, y = response_scaled, color = response_type_group, group = curve_ID)) +
  geom_line(linewidth = .5) +
  theme_minimal() +
  labs(x = "Temperature (C)",
       y = "Normalized Response",
       title = "3 param gaus") +
  theme(legend.position = "bottom")



ggplot(good_fits_quadratic_preds, aes(x = test_temp, y = response_scaled, color = response_type_group, group = curve_ID)) +
  geom_line(linewidth = .5) +
  theme_minimal() +
  labs(x = "Temperature (C)",
       y = "Normalized Response",
       title = "3 param quad") +
  theme(legend.position = "bottom")

good_fits_deutsch_preds <- good_fits_preds %>%
  mutate(model_name = "deutsch")
good_fits_flinn_1991_preds <- good_fits_flinn_1991_preds %>%
  mutate(model_name = "flinn")
good_fits_gaussian_preds <- good_fits_gaussian_preds %>%
  mutate(model_name = "gaussian")
good_fits_quadratic_preds <- good_fits_quadratic_preds %>%
  mutate(model_name = "quadratic")
all_preds <- rbind(good_fits_quadratic_preds, good_fits_deutsch_preds, good_fits_flinn_1991_preds, good_fits_gaussian_preds)

ggplot(all_preds, aes(x = test_temp, y = response_scaled, color = response_type_group, group = curve_ID)) +
  geom_line(linewidth = .35) +
  theme_minimal() +
  labs(x = "Temperature (C)",
       y = "Normalized Response",
       title = "Overlaid TPCs") +
  scale_color_manual(
    values = c(
      "metabolism" = "orange",        
      "feeding" = "lightpink",   
      "growth" = "springgreen3", 
      "swimming"  = "royalblue",
      "reproduction" = "purple3",
      "survival" = "red",
      "predation" = "black"
    )
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom")

length(unique(all_preds$curve_ID)) #106
#make some sort of codified way to indicate if a dataset has min, max, opt, breadth, etc to indicate curve fullness
fits <- all_preds %>%
  select(curve_ID, topt, curve_type, latitude, longitude, response_type_group, response_type, model_name ) %>%
  distinct() %>%
  mutate(abs_latitude = abs(latitude))
ggplot(fits, aes(x = abs_latitude, y = topt, color = response_type_group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Absolute Latitude",
    y = "Thermal Optimum",
    title = "Scatter of Topt and Absolute Latitude"
  ) +
  scale_color_manual(
    values = c(
      "metabolism" = "orange",        
      "feeding" = "lightpink",   
      "growth" = "springgreen3", 
      "swimming"  = "royalblue",
      "reproduction" = "purple3",
      "survival" = "red",
      "predation" = "black"
    )
  )
theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


regression <- lm(topt ~ abs_latitude, data = fits)
summary(regression)
summary(regression)$r.squared


####violin plot of tpc topt on y and dif response types on x ####
library(hrbrthemes)
install.packages("viridis")
library(viridis)
sample <- fits_unique %>% group_by(response_type_group) %>% summarize(num=n())
fits_unique %>%
  left_join(sample) %>%
  mutate(myaxis = paste0(response_type_group, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = topt, fill = response_type_group)) +
  geom_violin(width = 1.4) + 
  geom_boxplot(width = .1, color = "grey", alpha = .2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)) +
  ggtitle("Violin plot of thermal optimums for each response group") +
  xlab("")

#only feeding, growth, metabolism, and swimming
fits_filtered <- fits_unique %>%
  filter(response_type_group %in% c("feeding", "growth", "metabolism", "swimming"))
sample <- fits_filtered %>% group_by(response_type_group) %>% summarize(num=n())
fits_filtered %>%
  left_join(sample) %>%
  mutate(myaxis = paste0(response_type_group, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = topt, fill = response_type_group)) +
  geom_violin(width = 1.4) + 
  geom_boxplot(width = .1, color = "grey", alpha = .2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)) +
  ggtitle("Violin plot of thermal optimums for each response group") +
  xlab("")

sample <- fits_filtered %>% group_by(response_type_group) %>% summarize(num=n())
fits_filtered %>%
  left_join(sample) %>%
  mutate(
    response_type_group = factor(response_type_group,
                                 levels = c("metabolism", "feeding", "growth", "swimming")),
    myaxis = paste0(response_type_group, "\n", "n=", num)
  ) %>%
  ggplot(aes(x = myaxis, y = topt, fill = response_type_group)) +
  geom_violin(trim = FALSE, width = 0.9, alpha = 0.8, color = "black") +
  geom_boxplot(width = 0.15, color = "black", outlier.shape = NA, alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1, color = "black") +
  
  scale_fill_manual(values = c("metabolism" = "#F4D03F",
                               "feeding" = "#1ABC9C",
                               "growth" = "#7D3C98",
                               "swimming" = "blue")) +
  
  labs(
    x = "Response type",
    y = expression(T[opt]~"(°C)"),
    title = "Thermal optima across physiological performance"
  ) +
  theme_ipsum(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )






habitat <- curves %>%
  select(curve_ID, habitat, habitat_water)

fits <- fits %>%
  left_join(habitat, join_by(curve_ID))
fits_unique <- fits %>%
  group_by(curve_ID) %>%
  distinct()
sample <- fits_unique %>% group_by(habitat_water) %>% summarize(num=n())
fits_unique %>%
  left_join(sample) %>%
  mutate(
    habitat_water = factor(habitat_water,
                           levels = c("marine", "freshwater", "brackish")),
    myaxis = paste0(habitat_water, "\n", "n=", num)
  ) %>%
  ggplot(aes(x = myaxis, y = topt, fill = habitat_water)) +
  geom_violin(trim = FALSE, width = 0.9, alpha = 0.8, color = "black") +
  geom_boxplot(width = 0.15, color = "black", outlier.shape = NA, alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1, color = "black") +
  
  scale_fill_manual(values = c("marine" = "#F4D03F",
                               "freshwater" = "#1ABC9C",
                               "brackish" = "#7D3C98")) +
  
  labs(
    x = "Habitat type",
    y = expression(T[opt]~"(°C)"),
    title = "Thermal optima across aquatic habitats"
  ) +
  theme_ipsum(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
print(unique(fits_unique$habitat))
sample_hab <- fits_unique %>% group_by(habitat) %>% summarize(num=n())

###can i test if the relationsihp is mostly latitudinally driver? or if also by water type?