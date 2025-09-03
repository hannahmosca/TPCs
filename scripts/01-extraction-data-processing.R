#### script info #### 
#title: extraction-data-processing.R
#author: Hannah Mosca
#this script cleans and processes raw thermal performance data. It prepares raw trait data and experimental data extracted from the literature for TPC fitting. It filters for wild fish only, assigns unique curve Ids, standardizes variable names, categorizes trait types, and outputs cleaned data sets for mean and individual-level responses.

#### 1. load packages ####
library(tidyverse)
library(here)
library(stringr)

#### 2. load most up to date extraction datasheet ####
filename <- "data_extraction_09-03-2025.csv"
raw_data <- read.csv(here("raw-data", filename))

#### 3. initial data cleaning ####
data <- raw_data %>%
  filter(if_any(everything(), ~ . != "")) %>% #remove empty rows
  filter(origin == "wild")  #keep wild-origin data only
rm(raw_data) #save some space in env
#standardize curve type
data <- data %>%
  mutate(across(where(is.character), ~na_if(., "n/a"))) %>%
  mutate(curve_type = ifelse(curve_type == "accute-exposure",
                             "acute-change",
                             ifelse(curve_type == "acute",
                                    "acute-change",
                                    ifelse(curve_type == "batch-aclim",
                                           "batch-acclim",
                                           ifelse(is.na(curve_type) | curve_type == "",
                                                  NA,
                                                  ifelse(curve_type == "acute-exposure",
                                                         "acute-change",
                                                         curve_type))))))
#tidy up response types that are the same (there are likely more we can group)
data <- data %>% #handling response_type syns
  mutate(response_type = case_when(
    str_detect(response_type, "weight-sgr|SGR-weight|weight-growth-rate") ~ "standard-growth-rate-weight",
    response_type == "max-metabolism" ~ "maximum-metabolic-rate",
    response_type == "CTmin" ~ "ctmin",
    TRUE ~ response_type  
  ))
#keep rows with at least one valid response
data <- data %>%
  filter(if_any(c(response_mean, response_ind, response_mode, response_median, min_response, max_response), ~ !is.na(.)))
#remove problematic studies
data <- data %>%
  filter(!study_ID %in% c("1_0017", "1_0020"))

#### 4. categorize responses into response/trait groups ####
data <- data %>%
  mutate(response_type_group = case_when(
    # somatic growth-related categories
    response_type %in% c("growth-energy-as-a-percentage-of-food-energy","decrease-in-body-weight","length-sgr","SGR","otolith-sgr","growth-efficiency","growth-rate-between-blastopore-closure-and-maximum-tissue-wet-mass","fish-water-composition", "fish-protein-composition","fish-fat-composition","final-growth-rate","relative-daily-growth","standardised-growth-rate","tank-specific-growth-rate","growth efficiency","specific-growth-rate","standardised-energy-intake", "growth-rate", "specific-growth-rate-in-wet-weight", 
                         "specific-growth-rate-in-dry-weight", "specific-growth-rate-in-protein", "total-length", "relative-growth-rate", "daily-increment-in-total-length",
                         "specific-growth-rate-in-energy", "length-growth-rate", "WGR", "SGR-standard-length", "embyronic-growth-rate", "rate-of-normal-developing-larvae", 
                         "LGR", "growth-change-in-mass", "growth-change-in-length", "standardized growth", "instantaneous-rate-of-biomass-gain", "daily-weight-gain", "standardized growth", "growth-rate-body-weight", "specific-growth-rate-weight",
                         "instantaneous-growth-rate-weight", "standard-length","instantaneous-growth-rate-length", "body-comp-moisture-content", "body-comp-protein-content", "body-comp-lipid-conten","body-comp-ash-content", "daily-growth-rate", "standard-growth-rate-weight", "mean-standardised-growth-rate","weight-gain","individual-specific-growth-rate", "growth-rate-length") ~ "growth",
    
    # metabolism-related categories
    response_type %in% c("excretion-energy-and-metabolism-energy-as-a-percentage-of-food-energy","excess-post-excercise-oxygen-consumption-response","repeat-excess-post-excercise-oxygen-consumption-response","oxygen-uptake","scope-for-growth","consumption-rate","routine-metabolism","RMR","MMR_18h","MMR_1h","recMMR50","maximum ventilation rate", "resting ventilation rate", "metabolic-scope","oxygen-consumption-rate", "log-scope-for-activity", "standard-metabolic-rate", "maximum-metabolic-rate", 
                         "metabolic-rate","absolute-aerobic-scope", "aerobic scope", "whole-oxygen-embryo-consumption", "log-SMR", "mitochondrial-respiration", "log-active-metabolic-rate", "aerobic-scope", "routine-metabolic-rate", "%-maximum-metabolic-scope-of-activity", 
                         "aerobic_scope", "ctmax", "ctmin", "mass-adjusted-resting-metabolic-rate", "resting-oxygen-consumption", "maximum-oxygen-consumption", "active-metabolic-rate", 
                         "mass-adjusted-maximum-metabolic-rate", "mass-adjusted-absolute-aerobic-scope","oxygen-consumption", "CTmax") ~ "metabolism",
    
    # swimming-related categories
    response_type %in% c("%-active-fish","cstart-distance-traveled-in-100-ms","cstart-duration","cstart-turn-angle","total-swimming-time","cumulative-turning-angle","maximum-angular-velocity","activity","Ucrit","burst-swim-speed","u-gait", "distance-moved","recovery-ratio", "minimum-muscle-contraction-time","relative-critical-swimming-speed", "absolute-critical-swimming-speed", 
                         "critical-swimming-speed", "swimming-speed", "swimming-speed-critical-velocity", "U-crit", "critical swimming speed", "optimal swimming speed",
                         "maximum-swimming-speed", "relative-sustained-swimming-speed", "burst-swimming-speed" , 
                         "relative-routine-swimming-speed", "relative-maximum-swimming-speed", 
                         "maximum-burst-speed", "maximum-length-specific-velocity", "swim-up-rate", "maximum-swimming-velocity", 
                         "maximum-length-specific-acceleration", "maximum-undulatory-swimming-speed", 
                         "caudal-fin-beat-frequency-at-maximal-undulatory-swimming-speed", 
                         "maximum-labriform-swimming-speed", "pectoral-fin-beat-frequency-at-maximal-labriform-swimming-speed", 
                         "constant-acceleration-swimming-performance", 
                         "repeat-constant-acceleration-swimming-performance", "endurance",
                         "swimming-speed-critical-velocity", "tail-beat-frequency", "maximum-critical-swimming-speed", "max-acceleration", "max-velocity", "max-angular-velocity", "max-angular-acceleration", "u-crit", "spontaneous-swimming-speed") ~ "swimming",
    
    # reproduction-related categories
    response_type %in% c("spawning-interval","maximum-approach-to-female-speed","condition-factor","gonado-somatic-index","hatch-success","egg-production-per-pair-per-month", "spawns-per-pair-per-month", 
                         "gonadosomatic-index", "visceralsomatic-index", "fertilization-rate", 
                         "hatching-rate", "malformation-rate", "hatch-rate", "time-to-first-hatch", "days-till-hatch",
                         "time-to-50%-hatch", "time-to-100%-hatch", "duration-hatching-period", 
                         "survival-to-hatch", "batch-size", "spawn-duration", "time-to-maturity", 
                         "mating-success", "fecundity", "hatching-success", "larval-survival", 
                         "reproductive-success", "total-time-following-females", "number-mating-attempts-in-10-min","number-copulations-in-10-min","%-mating-efficiency","copulations/min-following-females") ~ "reproduction",
    
    # feeding-related categories
    response_type %in% c("energy-content","maximum-ration-level","faecal production","faeces-energy-as-a-percentage-of-food-energy","feeding rate","food-conversion-efficiency","feed-efficiency","feed-conversion-ratio", "feed-absorption-efficiency-in-dry-weight","ingestion-rate", "conversion-efficiency","absorption-efficiency", "absorption-rate", 
                         "feed-absorption-efficiency-in-protein", "feed-absorption-efficiency-in-energy", "daily-food-consumption", "net-conversion-efficiency", 
                         "feed-conversion-efficiency-in-wet-weight", "feed-conversion-efficiency-in-dry-weight", "feeding-rate", 
                         "feed-conversion-efficiency-in-protein", "feed-conversion-efficiency-in-energy", "gross-conversion-efficiency", 
                         "feeding-attempts-per-fish", "mean-daily-food-intake", "relative-daily-food-intake", "resting on the substrate",
                         "food-consumption-rate", "feed-intake", "feed-efficiency-ratio", "feeding-efficiency", 
                         "feed-absorption-efficiency-in-energy","daily-feeding-rate", "food-energy", "feed-conversion-efficiency","functional-feeding-rate", "total-food-consumed") ~ "feeding",
    
    # survival-related categories
    response_type %in% c("survival", "survival-rate", "mortality", "%-survival", 
                         "percent-mortality", "gonadsomatic-index") ~ "survival",
    # predation-related categories
    response_type %in% c("predation-index", "prey-capture-rate", "capture-manuever-time", "prey-capture-probability") ~ "predation",
    TRUE ~ response_type
  ))

#### 5. clean and classify habitats ####
# tidying habitat information
data <- data %>%
  mutate(habitat = if_else(habitat == "coral reef", "reef", habitat)) %>%
  mutate(habitat = if_else(habitat == "Marine", "marine", habitat)) %>%
  mutate(habitat = if_else(habitat == "n/a", NA, habitat)) %>%
  mutate(habitat = if_else(habitat == "", NA, habitat)) %>%
  mutate(habitat_water = case_when(
    habitat %in% c("ocean", "sound", "marine rockpools", "bay", "sea","marine", "coastal","marine estuary", "intertidal salt marshes",
                   "gulf", "salt-pond", "fjord", "reef", "intertidal", "harbour") ~ "marine",
    habitat %in% c("river", "lake", "swamp", "creek", "pond", "freshwater") ~ "freshwater",
    habitat %in% c("wetlands", "lagoon", "mixed", "estuary", "mangrove creek", "mixed") ~ "brackish",
    TRUE ~ NA  # for the NA ones, should get this information from species later on 
  ))

#### 6. generate curve IDs for mean response curves ####

#filtering out mean response tpcs
mean_tpcs <- data %>%
  filter(!is.na(response_mean)) %>%
  group_by(study_ID, species_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group, collection_site) %>%
  select(study_ID, species_ID, cohort_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group,
         acclim_temp, test_temp, response_mean, response_ind, everything()) %>%
  mutate(curve_ID = ifelse(length(unique(cohort_ID)) == n(),
                           as.character(cur_group_id()),
                           paste(cur_group_id(), cohort_ID, sep = "_"))) %>%
  mutate(response_curve_type = "mean") %>%
  select(cohort_ID, curve_ID, response_curve_type, everything()) %>%
  ungroup() %>%
  group_by(curve_ID) %>%
  filter(n() >= 4) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  select(-curve_ID) %>%
  rename(curve_ID = id) %>%
  mutate(across(c(response_mean, test_temp), as.numeric))

length(unique(mean_tpcs$curve_ID)) #219 unique curve ids ##now 300 ##now 324

#### 7. generate curve IDs for individual response curves ####
ind_tpcs <- data %>%
  filter(response_ind != "n/a")
#get where curve_id left off in means
start_id <- max(mean_tpcs$curve_ID, na.rm = TRUE) + 1
#assign individual curve_IDs starting from the next available number
ind_tpcs <- ind_tpcs %>%
  group_by(study_ID, species_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group) %>%
  select(study_ID, species_ID, cohort_ID, curve_type, response_type, sex, treatment_1_group, treatment_2_group,
         acclim_temp, test_temp, response_mean, response_ind, everything()) %>%
  mutate(curve_ID = as.numeric(cur_group_id() + start_id - 1)) %>%
  mutate(response_curve_type = "individual") %>%
  select(cohort_ID, curve_ID, response_curve_type, everything()) %>%
  ungroup() %>%
  group_by(curve_ID) %>%
  filter(n() >= 4) %>%
  ungroup() %>%
  mutate(across(c(response_mean, test_temp), as.numeric))

length(unique(ind_tpcs$curve_ID)) #28 ind tpcs ##now 40 #now 47


#### 8. generate curve IDs for other sample response curves ####
#filter datasets that report a median value
median_tpcs <- data %>%
  filter(!is.na(response_median)) %>%
  filter(!(response_median == "")) %>%
  filter(is.na(response_mean)) %>%
  filter(is.na(response_ind))
start_id2 <- max(ind_tpcs$curve_ID, na.rm = TRUE) + 1 #new start_ID
# curve_IDs for median tpcs
median_tpcs <- median_tpcs %>%
  group_by(study_ID, species_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group) %>%
  select(study_ID, species_ID, cohort_ID, curve_type, response_type, sex, treatment_1_group, treatment_2_group,
         acclim_temp, test_temp, response_median, response_mean, response_ind, everything()) %>%
  mutate(curve_ID = as.numeric(cur_group_id() + start_id2 - 1)) %>%
  mutate(response_curve_type = "median") %>%
  select(cohort_ID, curve_ID, response_curve_type, everything()) %>%
  ungroup() %>%
  group_by(curve_ID) %>%
  filter(n() >= 4) %>%
  ungroup() %>%
  mutate(across(c(response_median, test_temp), as.numeric))
length(unique(median_tpcs$curve_ID)) #3

#filter curves that report min and max value
min_max_tpcs <- data %>%
  filter(!is.na(max_response)) %>%
  filter(!is.na(min_response)) %>%
  filter(is.na(response_mean)) %>%
  filter(!(min_response == "")) %>%
  filter(!(max_response == "")) %>%
  filter(is.na(response_ind))
start_id3 <- max(median_tpcs$curve_ID, na.rm = TRUE) + 1 #new start_id
#curve_IDs for min and max tpcs
min_max_tpcs <- min_max_tpcs %>%
  group_by(study_ID, species_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group) %>%
  select(study_ID, species_ID, cohort_ID, curve_type, response_type, sex, treatment_1_group, treatment_2_group,
         acclim_temp, test_temp, max_response, min_response, response_mean, response_ind, everything()) %>%
  mutate(curve_ID = as.numeric(cur_group_id() + start_id3 - 1)) %>%
  mutate(response_curve_type = "min-max") %>%
  select(cohort_ID, curve_ID, response_curve_type, everything()) %>%
  ungroup() %>%
  group_by(curve_ID) %>%
  filter(n() >= 4) %>%
  ungroup() %>%
  mutate(across(c(min_response, max_response, test_temp), as.numeric))

length(unique(min_max_tpcs$curve_ID)) #1

####375 total curves#####

