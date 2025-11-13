  #### curve coverage figures ####
  
  #want to make 2 kinds -- one updates to the one I already have with the visuals about coverage, and another that is a hist of #params
  
  rm(list=ls())
  coverage_sorted <- readRDS(here('processed-data', "wild_tpcs_data_coverage_sorted.RDS"))
  
  ## filter out some things so only working with unique coverage information
  coverage_sorted <- coverage_sorted %>%
    select(n_unique_temps, curve_ID, study_ID, species_ID, response_type, response_type_group, curve_type, land_or_sea, abs_latitude, habitat_water, dataset_type, topt_TF, thermal_min_TF, thermal_max_TF, breadth_TF, increasing_side_TF, decreasing_side_TF, thermal_tolerance_TF) %>%
    distinct() %>%
    mutate(n_unique_temps = ifelse(n_unique_temps >= 7, "7+", n_unique_temps))

  ## want to order it
  coverage_sorted <- coverage_sorted %>%
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
    )) %>%
    mutate(n_unique_temps = factor(n_unique_temps,
                                   levels = c("4", "5", "6", "7+")))
  
  
a <- ggplot(data = coverage_sorted, aes(x = dataset_type, fill = (n_unique_temps))) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("4" = "olivedrab3", "5" = "plum4", "6" = "sienna2","7+" = "lightblue")) +
  xlab("Dataset Coverage") +
  ylab("Count") +
  scale_x_discrete(
    labels = c(
      "full_curve" = "Full curve",
      "left_bound_withopt" = "T-minimum + optimum",
      "right_bound_withopt" = "T-maximum + optimum", 
      "topt" = "T-optimum only",
      "left_bound" = "T-minimum only",
      "right_bound" = "T-maximum only",
      "unbounded_increasing" = "Unbounded, increasing",
      "unbounded_decreasing" = "Unbounded, decreasing",
      "irregular" = "Irregular"
      )) +
  labs(fill = "Distinct temperatures tested") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
ggsave("dataset_type_his_temp.pdf", plot = a, path = here("figures"), width = 7, height = 4)


a <- ggplot(data = coverage_sorted, aes(x = dataset_type, fill = (habitat_water))) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("marine" = "navy", "freshwater" = "olivedrab3", "brackish" = "gold2"))+
  xlab("Dataset Coverage") +
  ylab("Count") +
  scale_x_discrete(
    labels = c(
      "full_curve" = "Full curve",
      "left_bound_withopt" = "T-minimum + optimum",
      "right_bound_withopt" = "T-maximum + optimum", 
      "topt" = "T-optimum only",
      "left_bound" = "T-minimum only",
      "right_bound" = "T-maximum only",
      "unbounded_increasing" = "Unbounded, increasing",
      "unbounded_decreasing" = "Unbounded, decreasing",
      "irregular" = "Irregular"
    )) +
  labs(fill = "Realm") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
a
ggsave("dataset_type_his_water.pdf", plot = a, path = here("figures"), width = 7, height = 4)

#### figure about curve / parameter coverage ####
