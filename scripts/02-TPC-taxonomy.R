#### script info #### 
# title: TPC-taxonomy.R
# author: Hannah Mosca
# description: This script loads species data, filters it to match the TPC dataset, and gets taxonomic classifications using ITIS.

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




####yulia_worms_function.r####
library(dplyr)

# Function to fetch WoRMS records for a vector of species names using lapply
get_wm_records <- function(species_names,marine_only = TRUE,fuzzy = TRUE) {
  # Use lapply to apply the worrms::wm_records_names function to each species name
  all_records <- lapply(species_names, function(species_name ) {
    tryCatch({
      worrms::wm_records_name(species_name,fuzzy = fuzzy,marine_only=marine_only)
    }, error = function(e) {
      message("\n", e)
      return(NULL)
    })
  })
  
  if(!is.null(all_records[[1]])){
    # Set the names of the list to match the species names
    names(all_records) <- species_names
    all_records<- tibble::enframe(all_records) %>%
      tidyr::unnest(value) |> 
      select(rank,valid_name,name,status,scientificname,match_type, everything()) |> 
      group_by(name) |> 
      #Keep only one accepted row per group, if available, and the accepted row should come from the first row if available.
      #If there is no accepted row in the group, just keep the first row of the group.
      slice(if (any(status == "accepted")) which(status == "accepted")[1] else 1) |> 
      ungroup()
    return(all_records)
  }
}
# dta<-get_wm_records("Tanystylum grossifemora")


# Function to get the rank for each species
get_taxon_rank <- function(species_name) {
  # Search for the species name in WoRMS
  species_info <- wm_records_name(name = species_name)
  
  # Check if any results were returned
  if (nrow(species_info) > 0) {
    # Get the AphiaID for the first match
    aphia_id <- species_info$AphiaID[1]
    
    # Retrieve the detailed taxonomic information using AphiaID
    taxon_details <- worrms::wm_record(aphia_id)
    
    # Return the rank
    return(taxon_details$rank)
  } else {
    # Return NA if no match was found
    return(NA)
  }
}

# Function to get the taxonomic rank for each AphiaID
get_rank_from_aphia <- function(aphia_id) {
  # Retrieve taxonomic details using the AphiaID
  taxon_details <- worrms::wm_record(aphia_id)
  
  # Return the rank if available, otherwise NA
  if (!is.null(taxon_details)) {
    return(taxon_details$rank)
  } else {
    return(NA)
  }
}
