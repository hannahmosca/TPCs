#### script info #### 
# title: TPC-taxonomy.R
# author: Hannah Mosca
# description: This script loads species data, filters it to match the TPC dataset, and gets taxonomic classifications using ITIS.
rm(list=ls())
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
filename <- "data_extraction_species_ID_07_10_2025.csv"
# load species data and remove empty entries
species <- read.csv(here("raw-data", filename)) %>%
  filter(species != "")
##filter species to only those we have temp datasets on
curves <- readRDS(here("processed-data", "wild-tpcs.RdS"))
species_IDs <- unique(curves$species_ID)
species_filtered <- species %>%
  filter(species_ID %in% species_IDs)


# create a new column with full species name
species_filtered <- species_filtered %>%
  mutate(species_name = paste(genus, species))
species_filtered <- species_filtered %>%
  mutate(species_name = case_when(
    species_name == "Austrolebias wolterstorff" ~ "Megalebias wolterstorffi",
    species_name == "gambusia holbrooki" ~ "Gambusia affinis",
    species_name == "Salvelinus alpinus" ~ "Salvelinus alpinus",  # redundant but fine
    species_name == "Channa striatus" ~ "Channa striata",
    species_name == "Centropristis  striata" ~ "Centropristis striata",
    species_name == "Chromis  atripectoralis" ~ "Chromis atripectoralis",
    TRUE ~ species_name   
  ))
# extract unique species names
species_name <- unique(species_filtered$species_name) #one that is duplicated, fundilitis heterolitus, but one of them is a sub-species

# get taxonomic classification for each species using ITIS
df1 <- classification(species_name, db = 'itis')
#83 found, 4 not found 

###stopped edited here
# get_wormsid_ gives back null and length 0 elements
## first remove with compact and discard then map_dfr

# make and rotate dataframe 
taxonmy <- map_dfr(.x = df1, ~ data.frame(.x), .id = 'species_name') %>%
  pivot_wider(id_cols = species_name, names_from = rank, values_from = c(name, id)) %>%
  rename_with(~ str_replace(.x, 'name_', '')) %>%
  rename_with(~ str_replace(.x, 'id_', 'wormsid_')) %>%
  janitor::clean_names()
taxonmy <- taxonmy %>%
  left_join(species_filtered %>% select(species_ID, species_name), join_by(species_name))

write.csv(taxonmy, here('processed-data', 'taxonomy.csv')) ## updated in raw the ones that i coudlnt get from itls

taxonomy_updated <- read.csv(here('processed-data', 'taxonomy-updated.csv'))

install.packages("worms")
library(worrms)

codes <- taxonomy_updated %>%
  rename(gen_spp = species_name)

source(here("scripts", "yulia_worms_function.R"))


# Step 3. loop through species list -----------------------------------------------
# create empty list to store outputs
out <- vector(mode = "list",
              length = nrow(codes))

# loop through species list to run the function
for(i in 1:length(out)){
  
  # save function output in list element
  out[[i]] <- get_wm_records(codes$gen_spp[i])
  # clock
  print(i)
  
}
# note that most outputs will probably have 1 row (this is ideal)
# but some might have zero if it doens't find your species, and 
# some might have multiple if there are mulitple matches

# view to see how many rows each has. If they are all 1 row, that's good. 
lapply(out, nrow) %>% unlist() %>% table()
# if there are some with different than one row, investigate those
which(lapply(out, nrow) %>% unlist() != 1)

# Step 4. bind outputs to original data frame -----------------------------
# Create new columns to save outputs
codes$after_wrms_name <- NA
codes$after_wrms_id <- NA

# loop again to add the name that worms finds and the AphiaID 
# to the original dataframe. 
for(i in 1:nrow(codes)){
  if(!is.null(out[[i]])){
    codes$after_wrms_name[i] <- out[[i]]$valid_name
    codes$after_wrms_id[i] <- out[[i]]$valid_AphiaID
  }
  print(i)
}
codes <- codes %>%
  relocate(after_wrms_name, .before = gen_spp) %>%
  relocate(after_wrms_id, .after = after_wrms_name)

write.csv(codes, here("processed-data", "alpha_not_updated.csv"))
taxo_alpha_updated <- read.csv(here("processed-data", "alpha_updated.csv")) %>%
  select(gen_spp, kingdom, subkingdom, infrakingdom, phylum, subphylum, infraphylum, superclass, class, superorder, order, suborder, family, subfamily, genus, species, AlphiaID, species_ID)

taxonomy <- taxo_alpha_updated %>%
  left_join(species_filtered %>% select(species_ID, subspecies, common_name, species_notes), join_by(species_ID))
saveRDS(taxonomy, here("processed-data", "taxonomy_up_to_date.RDS"))
