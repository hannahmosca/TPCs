#### script info #### 
#title: TPC-data-characteristics.R
#author: Hannah Mosca
#this script ...wha

#### 1. load packages ####
library(tidyverse)
library(here)
library(dplyr)
library(stringr)

#### 2. load TPCs and species taxonomy ####
curves <- readRDS(here("processed-data","wild-tpcs-09-09-2025.RdS"))
#how many curves?
length(unique(curves$curve_ID)) #389
#how many different studies?
length(unique(curves$study_ID)) #89
#how many unique species?
length(unique(curves$species_ID)) #87


