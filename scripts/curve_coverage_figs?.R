  #### curve coverage figures ####
  
  #want to make 2 kinds -- one updates to the one I already have with the visuals about coverage, and another that is a hist of #params
  rm(list=ls())
  coverage_sorted <- readRDS(here('processed-data', "wild_tpcs_data_coverage_sorted.RDS"))
  
  
  ## filter out some things so only working with unique coverage information
  coverage_sorted <- coverage_sorted %>%
    select(n_unique_temps, curve_ID, study_ID, species_ID, response_type, response_type_group, curve_type, land_or_sea, abs_latitude, habitat_water, dataset_type, topt_TF, thermal_min_TF, thermal_max_TF, thermal_tolerance_TF, increasing_side_TF, decreasing_side_TF) %>%
    distinct() %>%
    mutate(n_unique_temps_capped = ifelse(n_unique_temps >= 7, "7+", n_unique_temps))
  
temps_his <- ggplot(data = coverage_sorted, aes(x = n_unique_temps, fill = n_unique_temps_capped)) +
    geom_histogram(binwidth = 1, alpha = 0.9, color = "black") +
    scale_fill_manual(values = c("4" = "mediumslateblue", 
                                 "5" = "darkgreen", 
                                 "6" = "navy",
                                 "7+" = "cadetblue2")) +
  scale_y_continuous(breaks = seq(0,220,25)) +
  theme_minimal() + 
    labs(x = "Distinct Temperatures", y = "Number of datasets") + 
    theme(
      text = element_text(size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    labs(fill = "Temperature\nManipulations")
temps_his
  
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
    mutate(n_unique_temps_capped = factor(n_unique_temps_capped,
                                   levels = c("4", "5", "6", "7+")))
  
  
a <- ggplot(data = coverage_sorted, aes(x = dataset_type, fill = (n_unique_temps_capped))) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("4" = "mediumslateblue", "5" = "darkgreen", "6" = "navy","7+" = "cadetblue2")) +
  xlab("Coverage") +
  ylab("Number of datasets") +
  scale_y_continuous(breaks = seq(0,150,25)) +
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
    theme_classic() +
    theme(
      legend.position = "blank",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
a
ggsave("dataset_type_his_temp.pdf", plot = a, path = here("figures"), width = 7, height = 4)

library(patchwork)

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

temps <- curves %>%
  select(curve_ID, study_ID, test_temp) %>%
  group_by(curve_ID, study_ID) %>%
  summarise(
    min_temp = min(test_temp, na.rm = TRUE),
    max_temp = max(test_temp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  distinct(study_ID, min_temp, max_temp, .keep_all = TRUE) %>%
  group_by(study_ID) %>%
  mutate(line_num = row_number()) %>%  # give each range within a study a unique line position
  ungroup() %>%
  mutate(study_line = paste(study_ID, line_num, sep = "_"))  # unique ID for plotting

range <- ggplot(temps, aes(y = reorder(study_line, min_temp))) +
  geom_segment(aes(x = min_temp, xend = max_temp, yend = study_line)) +
  geom_point(aes(x = min_temp, y = study_line), color = "blue", size = 2) +
  geom_point(aes(x = max_temp, y = study_line), color = "red", size = 2) +
  labs(x = "Temperature Range Tested", y = "Study") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())


range

library(patchwork)
left_column <- temps_his / a + 
  plot_layout(ncol = 1, heights = c(1, 1))  
left_column
right_column <- range + plot_layout(widths = 2)
final_plot <- left_column | right_column  +           
  plot_annotation(tag_levels = "A")
final_plot
ggsave(
  filename = here("figures", "temp_coverage.png"),  # Corrected filename placement
  plot = final_plot, 
  width = 9, 
  height = 6, 
  dpi = 300, 
  device = "png"
)
#hi
#### figure about curve / parameter coverage ####
