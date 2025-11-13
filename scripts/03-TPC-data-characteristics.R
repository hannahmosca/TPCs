#### script info #### 
#title: TPC-data-characteristics.R
#author: Hannah Mosca
#this script ...wha
rm(list=ls())
#### 1. load packages ####
library(tidyverse)
library(here)
library(dplyr)
library(stringr)

#### 2. load TPCs and species taxonomy ####
curves <- readRDS(here("processed-data","wild-tpcs.RdS"))
taxa <- readRDS(here("processed-data", "taxonomy_up_to_date.RDS"))
#how many curves?
length(unique(curves$curve_ID)) #421
#how many different studies?
length(unique(curves$study_ID)) #95
#how many unique species?
length(unique(taxa$species_ID)) #91
##so i can do a breakdown of the datsets##
curves_unique <- curves %>%
  group_by(curve_ID) %>%
  slice(1) %>%
  group_by(study_ID) %>%
  mutate(datasets_per_study = n()) %>%
  ungroup

install.packages("maps")
library(maps)
world_map <- map_data("world")
#### latitude and longitude #### want to make the dots sized by how many datasets in each study
## also could be cool to plot it by the median temperature tested
map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "white") +
  geom_point(data = curves_unique %>%
               group_by(latitude, longitude) %>%
               slice(1),
             aes(x = longitude, y = latitude, colour = land_or_sea),
             size = 1.8, alpha = .7,
             position = position_jitter(width = 0.1, height = 0.1)) + 
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),  
    axis.line = element_blank(),  
    text = element_text(family = "Times New Roman", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),  
    plot.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none") +
  scale_color_manual(values = c("oceanic" = "blue3",   
                                "terrestrial" = "palegreen4"))
map
curves_unique <- curves_unique %>%
  mutate(response_type_group = as.factor(response_type_group))
#### response groups
library(forcats)
response <- ggplot(curves_unique, aes(x = fct_infreq(response_type_group))) +
  geom_bar(fill = "black", color = "lightgrey", alpha = 0.9) +
  xlab("Response") +
  ylab("Count") +
  theme_minimal() +
  scale_x_discrete(
    labels = c(
      "growth" = "Somatic Growth",
      "swimming" = "Swimming",
      "metabolism" = "Metabolism", 
      "feeding" = "Feeding",
      "reproduction" = "Reproduction",
      "predation" = "Predation",
      "survival" = "Survival")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

response


### histogram of # of test temperatures 
a <- ggplot(curves_unique, aes(x = n_unique_temps)) +
  geom_histogram(binwidth=1, fill="black", color="lightgrey", alpha=0.9) +
  theme_minimal() + 
  labs(x = "Number of Temperatures", y = "Number of datasets") + 
  theme(
    text = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
a
ggsave("histogram_test_temps.pdf", plot = a, path = here("figures"), width = 5, height = 4)

#median # of test temps?
median(curves_unique$n_unique_temps) #4
mean(curves_unique$n_unique_temps) #5.11
# % above 4, % above 5
# percentage above 4
mean(curves_unique$n_unique_temps > 4) * 100 #47.51
mean(curves_unique$n_unique_temps == 4) *100 #52.49 %
# percentage above 5
mean(curves_unique$n_unique_temps > 5) * 100 #24.94


## species
curves_unique_with_taxa <- curves_unique %>%
  select(curve_ID, study_ID, species_ID, response_type, habitat) %>%
  left_join(taxa, join_by(species_ID)) %>%
  group_by(family) %>%
  mutate(datasets_per_family = n()) %>%
  select(datasets_per_family, family) %>%
  distinct()

family <- ggplot(data = curves_unique_with_taxa, aes(x = reorder(family, datasets_per_family), y = datasets_per_family)) +
  geom_bar(stat = "identity", fill = "darkblue", color = "lightgrey", alpha = 0.9, width = .9) +
  coord_flip() +
  xlab("Family") + 
  ylab("Extracted Datasets") +  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    text = element_text(size = 11)) +
  scale_y_continuous(expand = c(0, 0))
family
ggsave("histogram_families.pdf", plot = family, path = here("figures"), width = 5, height = 6)

#### combine family, response, and map ####
library(patchwork)
final_plot <- (map / (family + response + plot_layout(widths = c(1, 1.3)))) + 
  plot_layout(heights = c(1.2, 1.2)) +
  plot_annotation(tag_levels = "A",
                  theme = theme())
final_plot
ggsave(
  filename = here("figures", "extracted_summary.png"),  # Corrected filename placement
  plot = final_plot, 
  width = 9, 
  height = 9, 
  dpi = 300, 
  device = "png"
)
#histogram of habitat types and want 3 bars split into proportions have specific habitats




## some sort of way to visualize the temperature ranges tested ##

temps <- curves %>%
  select(curve_ID, study_ID, test_temp, latitude, longitude) %>%
  group_by(curve_ID) %>%
  mutate(temp_range = (max(test_temp) - min(test_temp))) %>%
  mutate(min_temp = min(test_temp)) %>%
  mutate(max_temp = max(test_temp)) %>%
  slice(1) %>%
  group_by(study_ID, temp_range) %>%
  mutate(count = n())



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
    axis.ticks.y = element_blank(),
    plot.background = element_rect(color = "black", linewidth = 1)) 


range
ggsave("thermal_range_tested.pdf", plot = range, path = here("figures"), width = 5, height = 8)

##how many times is a test temp used across this dataset -- counts as 1 for if its in a curve_ID
temps_2 <- curves %>%
  distinct(curve_ID, test_temp)

##also a histogram of all the temperatures tested??
freq_tested <- ggplot(data = temps_2, aes(x = test_temp)) +
  geom_histogram(binwidth=1.5, fill="darkblue", color="lightgrey", alpha=0.9) +
  theme_minimal() + 
  labs(x = "Temperature", y = "# of Datasets") + 
  ggtitle("Frequency of tested temperatures across data") +
  theme(
    text = element_text(size = 8),
    plot.background = element_rect(color = "black", linewidth = 1)) 
freq_tested
ggsave("temp_Freq_tested_hist.pdf", plot = freq_tested, path = here("figures"), width = 5, height = 4)
