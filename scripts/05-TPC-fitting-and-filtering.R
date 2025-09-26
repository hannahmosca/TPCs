#### script info #### 
#title: TPC-fitting.R
#author: Hannah Mosca
#this script is for fitting TPCs with rtpc package and filtering the data so we know what part of the curve data for, if we can fit all of the datasets with curves, etc.

#### 1. loading and installing packages and data ####
install.packages('rTPC')
# load packages
library(rTPC)
library(nls.multstart)
install.packages("nls.multstart")
library(broom)
library(tidyverse)
library(dplyr)
#load the data
d <- readRDS(here("processed-data","wild-tpcs.RdS")) # this was made in script 01

# classifying datasets by how much data is in them//how many test temps
# TP datasets with 5+ temperatures, calling them 'high res'
high_res_ds <- d %>%
  group_by(curve_ID) %>%
  mutate(
    temp_bin = round(test_temp),
    n_unique_temps = n_distinct(temp_bin)
  ) %>%
  filter(n_unique_temps >= 5) %>%
  ungroup() %>%
  select(-temp_bin, -n_unique_temps)
length(unique(high_res_ds$curve_ID)) #200

## TP datasets with 4 temperatures, calling them 'low res'
low_res_ds <- d %>%
  anti_join(high_res_ds, by = "curve_ID") 
length(unique(low_res_ds$curve_ID)) #222

#### 2. Fitting high res curves with all 4 parameter models in rtpc, first try to fit with Deutsche 2008 ####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'deutsch_2008')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'deutsch_2008')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'deutsch_2008')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ deutsch_2008(temp = test_temp, rmax, topt, ctmax, a),
      data = curve_data,
      iter = c(4,4,4,4),
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
    
    # redictions
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
print(length(failed_fits)) #32 datasets 

all_params_deutsch_2008_highres <- bind_rows(params_list, .id = "list_id")
all_preds_deutsch_2008_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_deutsch_2008_highres <- bind_rows(param_points_list, .id = "list_id")
#how good is model
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_deutsch_2008_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 3. bierre2_1999 ####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'briere2_1999')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'briere2_1999')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'briere2_1999')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ briere2_1999(temp = test_temp, tmin, tmax, a, b),
      data = curve_data,
      iter = c(4,4,4,4),
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
print(length(failed_fits))  #0

all_params_briere2_1999_highres <- bind_rows(params_list, .id = "list_id")
all_preds_briere2_1999_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_briere2_1999_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_briere2_1999_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)

#### 4. hinshelwood_1947####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'hinshelwood_1947')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 1
  start_upper <- sv + 1
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'hinshelwood_1947')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'hinshelwood_1947')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ hinshelwood_1947(temp = test_temp, a, e, b, eh),
      data = curve_data,
      iter = c(5, 5, 5, 5),
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
print(length(failed_fits))  #0

all_params_hinshelwood_1947_highres <- bind_rows(params_list, .id = "list_id")
all_preds_hinshelwood_1947_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_hinshelwood_1947_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_hinshelwood_1947_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 5. johnson_lewin_1946####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'johnsonlewin_1946')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 1
  start_upper <- sv + 1
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'johnsonlewin_1946')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'johnsonlewin_1946')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ johnsonlewin_1946(temp = test_temp, r0, e, eh, topt),
      data = curve_data,
      iter = c(5, 5, 5, 5),
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
print(length(failed_fits))  #61

all_params_johnsonlewin_1946_highres <- bind_rows(params_list, .id = "list_id")
all_preds_johnsonlewin_1946_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_johnsonlewin_1946_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_johnsonlewin_1946_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 6. lactin2_1995####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ lactin2_1995(temp = test_temp, a, b, tmax, delta_t),
      data = curve_data,
      iter = c(3, 3, 3, 3),
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
print(length(failed_fits))  #61

all_params_lactin2_1995_highres <- bind_rows(params_list, .id = "list_id")
all_preds_lactin2_1995_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_lactin2_1995_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_lactin2_1995_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 7. lrf_1991####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'lrf_1991')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lrf_1991')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lrf_1991')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ lrf_1991(temp = test_temp, rmax, topt, tmin, tmax),
      data = curve_data,
      iter = c(3, 3, 3, 3),
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
print(length(failed_fits))  #

all_params_lrf_1991_highres <- bind_rows(params_list, .id = "list_id")
all_preds_lrf_1991_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_lrf_1991_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_lrf_1991_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 8. modified_guassian_2006####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'modifiedgaussian_2006')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'modifiedgaussian_2006')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'modifiedgaussian_2006')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ modifiedgaussian_2006(temp = test_temp, rmax, topt, a, b),
      data = curve_data,
      iter = c(3, 3, 3, 3),
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
print(length(failed_fits))  #

all_params_modifiedgaussian_2006_highres <- bind_rows(params_list, .id = "list_id")
all_preds_modifiedgaussian_2006_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_modifiedgaussian_2006_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_modifiedgaussian_2006_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 9. oneil_1972####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'oneill_1972')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'oneill_1972')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'oneill_1972')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ oneill_1972(temp = test_temp, rmax, ctmax, topt, q10),
      data = curve_data,
      iter = c(4, 4, 4, 4),
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
print(length(failed_fits))  #

all_params_oneill_1972_highres <- bind_rows(params_list, .id = "list_id")
all_preds_oneill_1972_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_oneill_1972_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_oneill_1972_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 10. ratkowsky_1983####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'ratkowsky_1983')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'ratkowsky_1983')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'ratkowsky_1983')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ ratkowsky_1983(temp = test_temp, tmin, tmax, a, b),
      data = curve_data,
      iter = c(4, 4, 4, 4),
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
print(length(failed_fits))  #

all_params_ratkowsky_1983_highres <- bind_rows(params_list, .id = "list_id")
all_preds_ratkowsky_1983_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_ratkowsky_1983_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_ratkowsky_1983_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 11. rezende_2019 ####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'rezende_2019')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'rezende_2019')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'rezende_2019')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ rezende_2019(temp = test_temp, q10, a, b, c),
      data = curve_data,
      iter = c(4, 4, 4, 4),
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
print(length(failed_fits)) #14

all_params_rezende_2019_highres <- bind_rows(params_list, .id = "list_id")
all_preds_rezende_2019_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_rezende_2019_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_rezende_2019_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 12. spain_1982####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'spain_1982')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 1
  start_upper <- sv + 1
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'spain_1982')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'spain_1982')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ spain_1982(temp = test_temp, a, b, c, r0),
      data = curve_data,
      iter = c(3, 3, 3, 3),
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
print(length(failed_fits)) #0

all_params_spain_1982_highres <- bind_rows(params_list, .id = "list_id")
all_preds_spain_1982_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_spain_1982_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_spain_1982_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 13. thomas_2012####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'thomas_2012')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 1
  start_upper <- sv + 2
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'thomas_2012')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'thomas_2012')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ thomas_2012(temp = test_temp, a, b, c, topt),
      data = curve_data,
      iter = c(4, 4, 4, 4),
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
print(length(failed_fits)) #0

all_params_thomas_2012_highres <- bind_rows(params_list, .id = "list_id")
all_preds_thomas_2012_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_thomas_2012_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_thomas_2012_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#### 14. weibull_1995####
curve_ids <- unique(high_res_ds$curve_ID)
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'weibull_1995')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'weibull_1995')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'weibull_1995')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ weibull_1995(temp = test_temp, a, topt, b, c),
      data = curve_data,
      iter = c(4, 4, 4, 4),
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
print(length(failed_fits)) #4

all_params_weibull_1995_highres <- bind_rows(params_list, .id = "list_id")
all_preds_weibull_1995_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_weibull_1995_highres <- bind_rows(param_points_list, .id = "list_id")
# how good is moodel
rss_list <- sapply(fits_list, function(fit) {
  if (inherits(fit, "try-error")) return(NA)
  deviance(fit)
}) #NA if failed to fit
# make df
rss_weibull_1995_highres <- data.frame(
  curve_ID = names(fits_list),
  RSS = rss_list
)
#

#### 15. fitting low rest curves with 3 parameter models in rtpc ####
#### 3 parameter models for datasets with 4 points ####
#any with 4 points will be attempted with as many models on rtpc that fit 4 points 
# only 3 models have 3 parameters -- flinn_1991, gaussian_1989, and quadratic_2008
curve_ids <- unique(low_res_ds$curve_ID)
#### flinn_1991 ####
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


#### 16. join all predicted values ####
###here we got rid of lrf model because the fit was really weird###
# for the high res
high_res_pred_list <- list(
  "spain" = all_preds_spain_1982_highres,
  "weibull" = all_preds_weibull_1995_highres,
  "thomas" = all_preds_thomas_2012_highres,
  "rezende" = all_preds_rezende_2019_highres,
  "ratkowsky" = all_preds_ratkowsky_1983_highres,
  "oneill" = all_preds_oneill_1972_highres,
  "modifiedgaussian" = all_preds_modifiedgaussian_2006_highres,
  "lactin2" = all_preds_lactin2_1995_highres,
  "johnsonlewin" = all_preds_johnsonlewin_1946_highres,
  "hinshelwood" = all_preds_hinshelwood_1947_highres,
  "briere" = all_preds_briere2_1999_highres,
  "deutsch" = all_preds_deutsch_2008_highres
)
# for the low res
low_res_preds_list <- list(
  "flinn" = all_preds_flinn_1991_lowres,
  "gaussian" = all_preds_gaussian_1987_lowres,
  "quadratic" = all_preds_quadratic_2008_lowres
)

# Add a model column and bind all rows
all_preds_long_high <- imap_dfr(high_res_pred_list, ~ .x %>% mutate(model = .y))
all_preds_long_low <- imap_dfr(low_res_preds_list, ~ .x %>% mutate(model = .y))
all_preds <- rbind(all_preds_long_high, all_preds_long_low)

curves_sd <- curves %>%
  group_by(curve_ID) %>%
  mutate(sd_response = sd(response_value, na.rm = TRUE),
         min_1sd = min(response_value, na.rm = TRUE) - sd_response,
         max_1sd = max(response_value, na.rm. = TRUE) + sd_response) %>%
  ungroup() %>%
  mutate(curve_ID = as.numeric(curve_ID)) %>%
  select(curve_ID, response_value, test_temp, sd_response, max_1sd, min_1sd) 


### Attach bounds to fitted data ###
all_preds_with_bounds <- all_preds %>%
  left_join(
    curves_sd %>% distinct(curve_ID, response_value, test_temp, sd_response, max_1sd, min_1sd),
    by = "curve_ID"
  )

### Filter valid models within 1 SD of raw data and get valid models/preds ###
valid_models <- all_preds_with_bounds %>%
  group_by(curve_ID, model) %>%
  summarise(valid = all(.fitted >= min_1sd & .fitted <= max_1sd), .groups = "drop") %>%
  filter(valid) %>%
  select(-valid)
valid_preds <- all_preds %>%
  semi_join(valid_models, by = c("curve_ID", "model"))
info <- curves %>%
  select(curve_ID, response_type_group, latitude, longitude) %>%
  mutate(curve_ID = as.numeric(curve_ID))
valid_preds <- valid_preds %>%
  left_join(info, join_by(curve_ID))



#### . concatanate the  RSS for models to determine best ####
#first need to rename rss to model_rss in high res
rss_deutsch_2008_highres <- rss_deutsch_2008_highres %>%
  rename(deutsch_rss = RSS)
rss_briere2_1999_highres <- rss_briere2_1999_highres %>%
  rename(briere_rss = RSS)
rss_hinshelwood_1947_highres <- rss_hinshelwood_1947_highres %>%
  rename(hinshelwood_rss = RSS)
rss_johnsonlewin_1946_highres <- rss_johnsonlewin_1946_highres %>%
  rename(johnsonlewin_rss = RSS)
rss_lactin2_1995_highres <- rss_lactin2_1995_highres %>%
  rename(lactin2_rss = RSS)
rss_lrf_1991_highres <- rss_lrf_1991_highres %>%
  rename(lrf_rss = RSS)
rss_modifiedgaussian_2006_highres <- rss_modifiedgaussian_2006_highres %>%
  rename(modifiedgaussian_rss = RSS)
rss_oneill_1972_highres <- rss_oneill_1972_highres %>%
  rename(oneill_rss = RSS)
rss_ratkowsky_1983_highres <- rss_ratkowsky_1983_highres %>%
  rename(ratkowsky_rss = RSS)
rss_rezende_2019_highres <- rss_rezende_2019_highres %>%
  rename(rezende_rss = RSS)
rss_spain_1982_highres <- rss_spain_1982_highres %>%
  rename(spain_rss = RSS)
rss_thomas_2012_highres <- rss_thomas_2012_highres %>%
  rename(thomas_rss = RSS)
rss_weibull_1995_highres <- rss_weibull_1995_highres %>%
  rename(weibull_rss = RSS)
# join dfs
rss_list_high <- list(
  rss_deutsch_2008_highres,
  rss_briere2_1999_highres, 
  rss_hinshelwood_1947_highres,
  rss_johnsonlewin_1946_highres,
  rss_lactin2_1995_highres,
  rss_modifiedgaussian_2006_highres,
  rss_oneill_1972_highres,
  rss_ratkowsky_1983_highres,
  rss_rezende_2019_highres,
  rss_spain_1982_highres,
  rss_thomas_2012_highres,
  rss_weibull_1995_highres
)
rss_list_low <- list(
  rss_flinn_1991_lowres,
  rss_quadratic_2008_lowres,
  rss_gaussian_1987_lowres
)

# join them all by curve_ID
rss_5_param_models <- reduce(rss_list_high, left_join, by = "curve_ID") %>%
  mutate(curve_ID = as.numeric(curve_ID))
rss_4_param_models <- reduce(rss_list_low, left_join, by = "curve_ID") %>%
  mutate(curve_ID = as.numeric(curve_ID))
rss_5_long <- rss_5_param_models %>%
  pivot_longer(
    cols = -curve_ID,
    names_to = "model",
    values_to = "rss"
  )

rss_4_long <- rss_4_param_models %>%
  pivot_longer(
    cols = -curve_ID,
    names_to = "model",
    values_to = "rss"
  )

rss_all_long <- bind_rows(rss_5_long, rss_4_long) %>%
  mutate(model = model %>%
           str_remove("_rss$"))
         
valid_preds_with_rss <- valid_preds %>%
  left_join(rss_all_long, by = c("curve_ID", "model"))

all_model_predictions <- valid_preds_with_rss 
saveRDS(all_model_predictions, file = here('processed-data', "all_model_predictions.RDS"))
