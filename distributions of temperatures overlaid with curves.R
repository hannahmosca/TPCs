#### distributions of temperatures overlaid with curves ####
library(here)
library(dplyr)
library(tidyverse)
library(terra)

#load data
marine <- readRDS(here("processed-data", "marine_sst_all_temporal_mypoints.RDS"))
model_preds <- readRDS(here("processed-data", "top_model_predictions.RDS"))
#load data
fitted_datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))
fitted_datasets <- fitted_datasets %>%
  mutate(land_or_sea = ifelse(land_or_sea == "terrestrial", "freshwater", "marine"))

marine_full_curve <- fitted_datasets %>%
  filter(land_or_sea == "marine") %>%
  filter(dataset_type == "full_curve")

marine_topt <- fitted_datasets %>%
  filter(land_or_sea == "marine") %>%
  filter(dataset_type == "topt")

marine_full_curve <- marine_full_curve %>%
  left_join(marine, join_by(latitude, longitude))

temperatures <- marine_full_curve %>%
  select(-(c(1:27))) %>%
  select(-(c(3:7)))

library(dplyr)
library(tidyr)

temperatures_long <- temperatures %>%
  pivot_longer(
    cols = -c(latitude, longitude), 
    names_to = "date",
    values_to = "temperature"
  ) %>%
  mutate(date = as.Date(date))

temperatures_long <- temperatures_long %>%
  left_join(marine_full_curve %>% select(curve_ID, latitude, longitude), join_by(latitude, longitude))

temperatures_long_topt <- temperatures_long %>%
  left_join(marine_topt %>% select(curve_ID, latitude, longitude), join_by(latitude, longitude))

library(ggforce)
responses <- curves %>%
  select(curve_ID, response_type, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(response_type, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()

marine_full_curves <- unique(marine_full_curve$curve_ID)
marine_topt_curves <- unique(marine_topt$curve_ID)

ggplot() +
  geom_point(data = marine_topt,
             aes(x = topt, y = y_value_topt)) +
  geom_point(data = marine_topt,
             aes(x = ctmin, y = y_value_ctmin)) +
  geom_point(data = marine_topt,
            aes(x = ctmax, y = y_value_ctmax, color = model)) +
  geom_line(data = model_preds %>%
              filter(curve_ID %in% marine_topt_curves),
            aes(x = test_temp, y = .fitted, color = model), linewidth = 1) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
                      labeller = labeller(curve_ID = curve_labels)) +
  theme_minimal()

ggplot() +
geom_histogram(data = temperatures_long, aes(x = temperature), bins = 30,
  fill = "grey80", color = NA, alpha = 0.6) +
  facet_wrap_paginate( ~ curve_ID, scales = "free_y", ncol = 4, nrow = 4, page = 1 )

###try just free_y


ggplot() +
  geom_point(data = marine_topt,
             aes(x = topt, y = y_value_topt)) +
  geom_line(data = model_preds %>%
              filter(curve_ID %in% marine_topt_curves),
            aes(x = test_temp, y = .fitted, color = model), linewidth = 1) +
  geom_histogram(
    data = temperatures_long_topt,
    aes(x = temperature, y = after_stat(..count..) / 50),
    fill = "grey80", alpha = 0.6, bins = 20
  ) +
  scale_y_continuous(
    name = "Curve response",
    sec.axis = sec_axis(~ . * 50, name = "Temp count")
  ) +
  facet_wrap_paginate(
    ~curve_ID, scales = "free",
    ncol = 4, nrow = 4, page = 1,
    labeller = labeller(curve_ID = curve_labels)
  ) +
  theme_minimal()

  