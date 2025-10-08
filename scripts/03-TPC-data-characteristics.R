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
length(unique(curves$curve_ID)) #422
#how many different studies?
length(unique(curves$study_ID)) #95
#how many unique species?
length(unique(taxa$species_ID)) #91
##so i can do a breakdown of the datsets##
curves_unique <- curves %>%
  group_by(curve_ID) %>%
  slice(1)


install.packages("maps")
library(maps)
world_map <- map_data("world")
#### latitude and longitude ####
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "white") +
  geom_point(data = curves_unique, 
             aes(x = longitude, y = latitude, colour = habitat_water),
             size = 2,
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
  scale_color_manual(values = c("marine" = "blue3",   
                                "freshwater" = "palegreen4",  
                                "brackish" = "#E69F00"))

library(ggplot2)

###this is how the datasets distribute across abs. latitude###
ggplot(curves_unique, aes(x = abs_latitude)) +
  geom_histogram(binwidth = 3, fill = "red", color = "white") +
  facet_wrap(~response_type_group) +
  labs(x = "Latitude (°)", y = "# of datasets") +
  theme_minimal()



