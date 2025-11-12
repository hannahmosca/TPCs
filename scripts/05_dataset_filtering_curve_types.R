# Hannah Mosca # 
# this script is to subset the cleaned performance datasets into curve types #
rm(list=ls())
library(conflicted)
library(dplyr)

conflict_prefer("select", "dplyr")
#read in the data
curves <- readRDS(here('processed-data', 'wild-tpcs.RdS'))
#### 01.normalize all of the datasets so can work with scaled values ####
data_scaled <- curves %>%
  select(curve_ID, test_temp, response_value, response_type, response_unit) %>%
  group_by(curve_ID, test_temp) %>%
  mutate(mean_response = mean(response_value, na.rm = TRUE)) %>%  # mean at each temp, handles ind response curves
  ungroup() %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = mean_response / max(mean_response, na.rm = TRUE)) %>%  # scale within curve
  ungroup() %>%
  distinct(curve_ID, test_temp, response_type, mean_response, response_scaled, response_unit)
#### 02. add columns for datasets that are left bounded, right bounded, and reach an optimum ####

#optimum: curves that have a max response sandwiched by responses that are less on both sides ...ie go up and come down

# The values rise before the peak.
# The values fall after the peak.
# The peak is not at the edges - ie the first point.

optimum_check <- data_scaled %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>% #order data by temp
  summarize(
    peak_pos = which.max(response_scaled), #finds position of max response/opt
    has_optimum = peak_pos > 1 & peak_pos < n() & #peak is not the first or last point
      all(diff(response_scaled[1:peak_pos]) >= 0) &  #response values rise up to peak
      all(diff(response_scaled[peak_pos:n()]) <= 0)  #respose values fall below peak
  )
data_scaled <- left_join(data_scaled, optimum_check, by = "curve_ID")

optimum_curves <- optimum_check %>%
  filter(has_optimum == TRUE)

opt_list <- unique(optimum_curves$curve_ID) #checked these, and am removing '156' and putting it in no-opt
opt_list <- opt_list[!opt_list %in% 156] ###OPT CURVE LIST###
adding_to_topt <- c(80, 22, 87, 226, 228, 232, 234, 265, 304, 314, 313, 325, 401, 312, 416, 230, 74, 91, 164, 199, 222, 223, 227, 231, 233, 242, 243, 253, 261, 263, 266, 311, 383, 386, 389, 397, 405, 408, 412, 71, 63) # these are ones i got from ctmin and ctmax and unbounded_NO
opt_list <- c(opt_list, adding_to_topt)
opt_list <- unique(opt_list)

####03. Handling datasets without an optimum ####
non_opt <- curves %>%
  filter(!(curve_ID %in% opt_list))
non_opt_list <- unique(non_opt$curve_ID)

# Compute left and right bounds
non_opt <- data_scaled %>%
  filter(curve_ID %in% non_opt_list) %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>%
  mutate(
    first_temp = first(test_temp),
    first_response = first(response_scaled),
    left_bound  = ifelse(first_response <= 0.10, "yes", "no"),
    last_temp = last(test_temp),
    last_response = last(response_scaled),
    right_bound = ifelse(last_response <= 0.10, "yes", "no")
  ) %>%
  ungroup()

#CTMIN only datasets
ctmin <- non_opt %>%
  filter(left_bound == "yes")
###after this check, adding 22, 87, 226, 228, 232, 234, 265, 304, 314, 313, 325, 401, 312, 416 to topt dataframe list
###78 is really confusing, going to a confusing curve ID list
ctmin_only <- unique(ctmin$curve_ID) ##checking ctmin only
#FINAL CTMIN ONLY LIST#
ctmin_only_list <- ctmin_only[!ctmin_only %in% c(22, 87, 226, 228, 232, 234, 265, 304, 314, 313, 325, 401, 312, 416, 78)] 
adding_to_ctmin <- c(150, 400)
ctmin_only_list <- c(ctmin_only_list, adding_to_ctmin)

#CTMAX only datasets
ctmax <- non_opt %>%
  filter(right_bound == "yes")
ctmax_only <- unique(ctmax$curve_ID) ##checking ctmin only
#FINAL CTMAX ONLY LIST#
ctmax_only_list <- ctmax_only[!ctmax_only %in% c(36, 55, 56, 185, 186, 226, 230, 416)] #ones i am removing from ctmax

#niether CTMIN or CTMAX
unbounded_NO <- non_opt %>%
  filter(left_bound == "no") %>%
  filter(right_bound == "no")
unbounded_NO <- unique(unbounded_NO$curve_ID) 
unbounded_NO_list <- unbounded_NO[!unbounded_NO %in% c(80, 7, 8, 17, 23, 33, 38, 39, 40, 54, 60, 61, 63, 70, 71, 84, 93, 111, 125, 145, 152, 172, 202, 205, 209, 210, 215, 224, 228, 277, 308, 306, 297, 315, 316, 320, 322, 323, 324, 338, 341, 343, 345, 350, 373, 388, 399, 404, 406, 407, 423, 25, 74, 91, 164, 199, 222, 223, 227, 231, 233, 242, 243, 253, 261, 263, 266, 267, 311, 383, 386, 389, 397, 405, 408, 412, 150)]

confusing_datasets <- c(78, 36, 55, 56, 7, 8, 17, 23, 33, 38, 39, 40, 54, 60, 61, 70, 84, 93, 111, 125, 145, 152, 172, 202, 205, 209, 210, 215, 224, 277, 308, 306, 297, 315, 316, 320, 322, 323, 324, 338, 341, 343, 345, 350, 373, 388, 399, 404, 406, 407, 423, 176, 278, 307, 12, 34, 66, 25, 267)

#### WORKING WITH OPT DATASETS ####
## first sort by boundedness ## i made the closeness to 0 further for this ones....
opt <- data_scaled %>%
  filter(curve_ID %in% opt_list) %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>%
  mutate(
    first_temp = first(test_temp),
    first_response = first(response_scaled),
    left_bound  = ifelse(first_response <= 0.20, "yes", "no"),
    last_temp = last(test_temp),
    last_response = last(response_scaled),
    right_bound = ifelse(last_response <= 0.20, "yes", "no")
  ) %>%
  ungroup()

##first bounded ones##
#ctmin with topt datasets
ctmin_topt <- opt %>%
  filter(left_bound == "yes") %>%
  filter(right_bound == "no")
ctmin_topt_list <- unique(ctmin_topt$curve_ID)
ctmin_topt_list <- ctmin_topt_list[!ctmin_topt_list %in% c(253, 80, 309, 25, 157, 228, 232, 234, 265, 401, 402, 403, 20, 22, 76, 366, 367, 409, 34, 66, 400)]
#ctmax with topt datasets
ctmax_topt <- opt %>%
  filter(left_bound == "no") %>%
  filter(right_bound == "yes")
ctmax_topt_list <- unique(ctmax_topt$curve_ID)
ctmax_topt_list <- ctmax_topt_list[!ctmax_topt_list %in% c(18, 59, 225, 230, 270, 173, 361, 362)]
adding_to_ctmax_topt <- c(185, 186)
ctmax_topt_list <- c(ctmax_topt_list, adding_to_ctmax_topt)


full <- c(309,253, 80, 173, 361, 362, 157, 228, 232, 234, 401, 402, 403, 18, 59, 225, 230, 270, 20, 22, 76, 366, 367, 409) #curves i think are full that i got from ctmax opt and ctmin opt

#ctmin+ctmax+topt full curves#
breadth <- opt %>%
  filter(left_bound == "yes") %>%
  filter(right_bound == "yes")
breadth_list <- unique(breadth$curve_ID)
breadth_list <- c(breadth_list, full)
breadth_list <- unique(breadth_list)
breadth_list <- breadth_list[!breadth_list %in% c(12)]


#unbounded curves
topt_only <- opt %>%
  filter(left_bound == "no") %>%
  filter(right_bound == "no")
topt_only <- unique(topt_only$curve_ID)
topt_only <- topt_only[!topt_only %in% c(176, 278, 307, 271)]


### WORKING with unbounded no opt ###

library(dplyr)

unbounded_curve_direction <- curves %>%
  group_by(curve_ID) %>%
  filter(curve_ID %in% unbounded_NO_list) %>%
  summarize(
    slope = lm(response_value ~ test_temp)$coefficients[2],  # slope of linear trend
    direction = case_when(
      slope > 0 ~ "increasing",
      slope < 0 ~ "decreasing",
      TRUE ~ "flat"
    )
  )
increasing_unbounded <- unbounded_curve_direction %>%
  filter(direction == "increasing")
inc_unbounded_NO_list <- unique(increasing_unbounded$curve_ID)
add_to_inc <- c(141, 156, 271)
inc_unbounded_NO_list <- c(inc_unbounded_NO_list, add_to_inc)
decreasing_unbounded <- unbounded_curve_direction %>%
  filter(direction == "decreasing")
dec_unbounded_NO_list <- unique(decreasing_unbounded$curve_ID)
dec_unbounded_NO_list <- dec_unbounded_NO_list[!dec_unbounded_NO_list %in% c(141, 156)]

# now i have these vectors that hold all of the curves sorted
all <- c(topt_only, ctmax_only_list, ctmin_only_list, inc_unbounded_NO_list, dec_unbounded_NO_list, confusing_datasets, ctmin_topt_list, ctmax_topt_list, breadth_list)
length(unique(all))

## 
distinct_curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(dataset_type = case_when(
    curve_ID %in% topt_only ~ "topt",
    curve_ID %in% ctmax_only_list ~ "right_bound",
    curve_ID %in% ctmin_only_list ~ "left_bound",
    curve_ID %in% inc_unbounded_NO_list ~ "unbounded_increasing",
    curve_ID %in% dec_unbounded_NO_list ~ "unbounded_decreasing",
    curve_ID %in% confusing_datasets ~ "irregular",
    curve_ID %in% ctmin_topt_list ~ "left_bound_withopt",
    curve_ID %in% ctmax_topt_list ~ "right_bound_withopt",
    curve_ID %in% breadth_list ~ "full_curve",
    TRUE ~ NA_character_
  ))

dataset_types <- distinct_curves %>%
  group_by(curve_ID) %>%
  select(curve_ID, dataset_type, habitat_water) %>%
  distinct() %>%
  mutate(
    topt_TF = case_when(curve_ID %in% c(topt_only, ctmin_topt_list, ctmax_topt_list, breadth_list) ~ TRUE, TRUE ~ FALSE),
    thermal_min_TF = case_when(curve_ID %in% c(ctmin_topt_list, ctmin_only_list, breadth_list) ~ TRUE, TRUE ~ FALSE),
    thermal_max_TF = case_when(curve_ID %in% c(ctmax_topt_list, ctmax_only_list, breadth_list) ~ TRUE, TRUE ~ FALSE),
    breadth_TF = case_when(curve_ID %in% breadth_list ~ TRUE, TRUE ~ FALSE),
    increasing_side_TF = case_when(curve_ID %in% c(ctmin_topt_list, ctmin_only_list, breadth_list, inc_unbounded_NO_list) ~ TRUE, TRUE ~ FALSE),
    decreasing_side_TF = case_when(curve_ID %in% c(ctmax_topt_list, ctmax_only_list, breadth_list, dec_unbounded_NO_list) ~ TRUE, TRUE ~ FALSE)) %>%
  ungroup()

# #make long
# long_data <- dataset_types %>%
#   select(habitat_water, topt, thermal_min, thermal_max, breadth, increasing_side, decreasing_side) %>%
#   pivot_longer(
#     cols = c(topt, thermal_min, thermal_max, breadth, increasing_side, decreasing_side),
#     names_to = "parameter",
#     values_to = "is_true"
#   )
# summary_counts <- long_data %>%
#   group_by(habitat_water, parameter) %>%
#   summarise(n_true = sum(is_true, na.rm = TRUE), .groups = "drop")


b <- ggplot(summary_counts, aes(x = reorder(parameter, -n_true), y = n_true, fill = habitat_water)) +
  geom_col(position = "stack") +  
  scale_fill_manual(
    values = c(
      "marine" = "navy",
      "freshwater" = "darkgreen",
      "brackish" = "gold"
    )
  ) +
  labs(
    title = "Count of datasets per parameter",
    x = "Thermal Parameter",
    y = "Count",
    fill = "Habitat Type"
  ) +
  scale_x_discrete(
    labels = c(
      "topt" = "Thermal Optimum",
      "thermal_min" = "Thermal Minimum",
      "thermal_max" = "Thermal Maximum",
      "breadth" = "Thermal Breadth",
      "increasing_side" = "Performance Rise",
      "decreasing_side" = "Performance fall"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
b
ggsave("dataset_type_by_param_his.pdf", plot = b, path = here("figures"), width = 7, height = 4)


distinct_curves <- distinct_curves %>%
  mutate(dataset_type = factor(
    dataset_type,
    levels = c(
      "full_curve",
      "left_bound_withopt",
      "right_bound_withopt",
      "topt",
      "left_bound",
      "right_bound",
      "unbounded_increasing",
      "unbounded_decreasing",
      "irregular"
    )
  ))

a <- ggplot(
  data = distinct_curves %>%
    group_by(curve_ID) %>%
    slice(1),
  aes(x = params_we_can_get, fill = habitat_water)
) +
  geom_bar(position = "stack") +
  theme_minimal() +
  xlab("Parameter") +
  ylab("Count of Curves") +
  ggtitle("Distribution of Curve Types by Habitat") +
  labs(fill = "Habitat Type") +
  scale_fill_manual(
    values = c(
      "marine" = "darkblue",     
      "freshwater" = "darkgreen",  
      "brackish" = "gold")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

a
c



###OUTPUT###
curves <- curves %>%
  left_join(dataset_types %>% select(-(habitat_water)), join_by(curve_ID))
saveRDS(curves, file = here('processed-data', "wild_tpcs_data_coverage_sorted.RDS"))
