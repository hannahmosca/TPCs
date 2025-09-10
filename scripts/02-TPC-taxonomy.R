#### script info #### 
# title: TPC-taxonomy.R
# author: Hannah Mosca
# description: This script loads species data, filters it to match the TPC dataset,
#              and retrieves taxonomic classifications using ITIS.

#### 1. load packages ####
library(tidyverse)
library(here)
library(dplyr)
library(stringr)

# installing taxize from GitHub 
install.packages("remotes")
remotes::install_github("ropensci/bold")
remotes::install_github("ropensci/taxize")
library(taxize)

#### 2. load most up-to-date extracted species_ID sheet ####
filename <- "data_extraction_species_ID_10_09_2025.csv"
# load species data and remove empty entries
species <- read.csv(here("raw-data", filename)) %>%
  filter(species != "")

# load labeled TPC curves and get unique species IDs
tpc_dataset_species <- readRDS(here("processed-data", "wild-tpcs.RdS")) %>%
  pull(species_ID) %>%
  unique()

# filter species to only those present in the TPC dataset
species_filtered <- species %>%
  filter(species_ID %in% tpc_dataset_species)

# create a new column with full species name
species_filtered <- species_filtered %>%
  mutate(species_name = paste(genus, species))

# extract unique species names
species_name <- unique(species_filtered$species_name)

# get taxonomic classification for each species using ITIS
df1 <- classification(species_name, db = 'itis')


###stopped edited here
# get_wormsid_ gives back null and length 0 elements
## first remove with compact and discard then map_dfr

# make and rotate dataframe 
taxonmy <- map_dfr(.x = df1, ~ data.frame(.x), .id = 'fish') %>%
  pivot_wider(id_cols = fish, names_from = rank, values_from = c(name, id)) %>%
  rename_with(~ str_replace(.x, 'name_', '')) %>%
  rename_with(~ str_replace(.x, 'id_', 'wormsid_')) %>%
  janitor::clean_names()