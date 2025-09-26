#### script info #### 
#title: TPC-rtpc.R
#author: Hannah Mosca
#this script 

#### loading and installing packages ####
install.packages('rTPC')
# load packages
library(rTPC)
library(nls.multstart)
install.packages("nls.multstart")

library(broom)
library(tidyverse)
####this is where we fit two models to many datasets####
#the two models being fit in this example are gaussian_1987() and sharpeschoolhigh_1981()

#load the data
d <- readRDS(here("processed-data","wild-tpcs.RdS"))


# fiting 2 models in rtpc #


d <- filter(d, curve_ID == "20")
fit <- nls_multstart(response_value~gaussian_1987(temp = test_temp, rmax, topt, a),
                     data = d,
                     iter = c(3,3,3),
                     start_lower = get_start_vals(d$test_temp, d$response_value, model_name = 'gaussian_1987') - 10,
                     start_upper = get_start_vals(d$test_temp, d$response_value, model_name = 'gaussian_1987') + 10,
                     lower = get_lower_lims(d$test_temp, d$response_value, model_name = 'gaussian_1987'),
                     upper = get_upper_lims(d$test_temp, d$response_value, model_name = 'gaussian_1987'),
                     supp_errors = 'Y',
                     convergence_count = FALSE)
fit

# get unique curve IDs
curve_ids <- unique(d$curve_ID)

# empty containers
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids

params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- d %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'gaussian_1987')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'gaussian_1987')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'gaussian_1987')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit Gaussian model
  fit <- try(
    nls_multstart(
      response_value ~ gaussian_1987(temp = test_temp, rmax, topt, a),
      data = curve_data,
      iter = c(3,3,3),
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

# combine results
all_params <- bind_rows(params_list, .id = "list_id")
all_preds <- bind_rows(preds_list, .id = "list_id")
all_param_points <- bind_rows(param_points_list, .id = "list_id")

ggplot() + 
  geom_point(
    data = curves %>% filter(curve_ID == "419"), 
    aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds %>% filter(curve_ID == "419"),
            aes(x = test_temp, y = .fitted), 
            linewidth = 1) +
  geom_point(data = all_param_points %>% filter(curve_ID == "419"),
             aes(x = test_temp, y = y_value),
             size = 3)

print(unique(full_curves_ish$curve_ID)) #67
[1]  35 307 322 328 329 333 332 336 337 339 344 345 346 347 370 371 372  15  16  19  44  47  43  48 107 106 105 120  23  37
[31]  66  67  64  69 109 110 127 130 128 146 143 147 140 134 151 158 160 170 169 176 192 204 203 196 195 200  33  31 212 250
[61] 249 274 289 284 285 287 288
#322 did such a bad job at fitting...i think this one should be a curve? try with different model?
#curves in fullcurveish that should be in full cruve
##329, 336, 344, 347, 370, 371, 15, 19, 44, 37
#11  
#18
#19
#20
#22
#31--at least 1 point after topt but it cant be like right next to topt, has to pull it down?
#need to have at least 1(or 2?) points after the predicted topt and also before 
 #we have some curves that dont really have a curve shape / response doesnt go up and come back down a lot
  #some curves that show the only data points decreasing half
  #some curves that show the only data points on the increasing half
  #some full curves with at least 2 points before the predicted thermal optimum and two points after
  
  ##data within 4 degrees of the max // close to max
  
#classify curves as just increasing, just decreasing, or full curve
#might need to first add topt to each curve id...
#some curves predict with ctmax?
##joining topt df to the curve df
topt <- all_params %>%
  select(topt, curve_ID)
curves <- left_join(d, topt, join_by(curve_ID))

#classify what side of the curve a point is on
curves <- curves %>%
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

increasing_side <- curves %>%
  filter(curve_symmetry == "increasing only")
print(unique(increasing_side$curve_ID)) #137
decreasing_side <- curves %>%
  filter(curve_symmetry == "decreasing only")
length(unique(decreasing_side$curve_ID)) #40
full_curves <- curves %>%
  filter(curve_symmetry == "full curve")
print(unique(full_curves$curve_ID)) #178
full_curves_ish <- curves %>%
  filter(curve_symmetry == "full curve_ish")
print(unique(full_curves_ish$curve_ID)) #67


#### try with a different model ####
curve_ids <- unique(d$curve_ID)

# empty containers
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids

params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()

# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- d %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'deutsch_2008')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'deutsch_2008')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'deutsch_2008')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit Gaussian model
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
print(failed_fits) #101 failed fits for deutsch ( i think because you need more than 4 data points to fit)
# combine results
all_params_deutsch_2008 <- bind_rows(params_list, .id = "list_id")
all_preds_deutsch_2008 <- bind_rows(preds_list, .id = "list_id")
all_param_points_deutsch_2008 <- bind_rows(param_points_list, .id = "list_id")

topt <- all_params_deutsch_2008 %>%
  select(topt, curve_ID)
curvesdeutsch_2008 <- left_join(d, topt, join_by(curve_ID))

#classify what side of the curve a point is on
curvesdeutsch_2008 <- curvesdeutsch_2008 %>%
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

increasing_side <- curvesdeutsch_2008 %>%
  filter(curve_symmetry == "increasing only")
length(unique(increasing_side$curve_ID)) #55
decreasing_side <- curvesdeutsch_2008 %>%
  filter(curve_symmetry == "decreasing only")
length(unique(decreasing_side$curve_ID)) #31
full_curves <- curvesdeutsch_2008 %>%
  filter(curve_symmetry == "full curve")
print(unique(full_curves$curve_ID)) #193
full_curves_ish <- curvesdeutsch_2008 %>%
  filter(curve_symmetry == "full curve_ish")
length(unique(full_curves_ish$curve_ID)) #42


ggplot() + 
  geom_point(
    data = curves %>% filter(curve_ID == "419"), 
    aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds_deutsch_2008 %>% filter(curve_ID == "419"),
            aes(x = test_temp, y = .fitted), 
            linewidth = 1) +
  geom_point(data = all_param_points_deutsch_2008 %>% filter(curve_ID == "419"),
             aes(x = test_temp, y = y_value),
             size = 3)

#121 is no good


#### quantify the difference between gaussian and deutsch ####
#make dataframe that includes parameters from both models and then calculate the difference for each parameter
all_params_deutsch_2008 <- all_params_deutsch_2008 %>%
  rename(d_rmax = rmax) %>%
  rename(d_topt = topt) %>%
  rename(d_ctmin = ctmin) %>%
  rename(d_ctmax = ctmax) %>%
  rename(d_e = e) %>%
  rename(d_eh = eh) %>%
  rename(d_q10 = q10) %>%
  rename(d_thermal_safety_margin = thermal_safety_margin) %>%
  rename(d_thermal_tolerance = thermal_tolerance) %>%
  rename(d_breadth = breadth) %>%
  rename(d_skewenss = skewness) %>%
  select(-(list_id))
all_params_deutsch_2008 <- all_params_deutsch_2008 %>%
  select(curve_ID, everything())
all_params_gaussian <- all_params %>%
  rename(g_rmax = rmax) %>%
  rename(g_topt = topt) %>%
  rename(g_ctmin = ctmin) %>%
  rename(g_ctmax = ctmax) %>%
  rename(g_e = e) %>%
  rename(g_eh = eh) %>%
  rename(g_q10 = q10) %>%
  rename(g_thermal_safety_margin = thermal_safety_margin) %>%
  rename(g_thermal_tolerance = thermal_tolerance) %>%
  rename(g_breadth = breadth) %>%
  rename(g_skewenss = skewness) %>%
  select(-(list_id))
  select(curve_ID, everything())
  
both_model_paramaters <- left_join(all_params_deutsch_2008, all_params_gaussian, by = "curve_ID")
  
##there are also a number of curves that deutsch didn't predict for because i think it requires one more starting param than gaussian
#want to make it NA when the is a prediciton for one but not the other


both_model_paramaters_dif <- both_model_paramaters %>%
  mutate(
    delta_rmax = abs(g_rmax - d_rmax),
    rel_rmax   = (delta_rmax / g_rmax) * 100,
    delta_topt = abs(g_topt - d_topt),
    rel_topt   = (delta_rmax / g_rmax) * 100,
    delta_ctmin = abs(g_ctmin - d_ctmin),
    rel_ctmin   = (delta_ctmin / g_ctmin) * 100,
    delta_ctmax = abs(g_ctmax - d_ctmax),
    rel_ctmax   = (delta_ctmax / g_ctmax) * 100,
    delta_breadth = abs(g_breadth - d_breadth),
    rel_breadth   = (delta_breadth / g_breadth) * 100)


library(ggplot2)

#gaussian vs deutsch

ggplot(both_model_paramaters_dif, aes(x = delta_topt)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  labs(x = "Difference in topt", y = "Count")

