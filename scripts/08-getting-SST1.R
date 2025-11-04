#load required packages
library(ncdf4)
library(terra)
library(here)
library(dplyr)
library(tidyverse)

filename <- "sst.mon.mean.nc" # sst data, 529 monthly means from 
r_temp = rast((here("raw-data", filename)), subds = "sst")
r_temp
nlyr(r_temp)
names(r_temp)[1:10]
crs(r_temp) #checks crs of raster
dim(r_temp) # 720 rows, 1440 columns, and 529
plot(r_temp[[12]]) ## plot a layer
res(r_temp)
#rename layers the dates so easier to work with
time_values <- time(r_temp)
layer_names <- format(as.Date(time_values), "%Y-%m-%d")
names(r_temp) <- layer_names
names(r_temp)

#rotate r_rempt so gets it to be -180 to 180 and -90to 90
r_temp <- rotate(r_temp)

#subset r_temp so starts where freshwater data does: 1982-01
r_temp1982_01to2025_09 <- subset(r_temp, 5:529)


## marine temp data for all locations monthly averages ##
df_marine <- as.data.frame(r_temp1982_01to2025_09, xy = TRUE, na.rm = FALSE)
saveRDS(df_marine, file = here("processed-data", "marine_sst_all_temp.RDS"))

df_marine <- df_marine %>%
  rowwise() %>%  # operate across columns for each row
  mutate(
    sst_mean   = mean(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_sd     = sd(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_median = median(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_min    = min(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_max    = max(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_range  = sst_max - sst_min
  ) %>%
  ungroup()
##save raw temp file
saveRDS(df_marine, file = here("processed-data", "marine_sst_all_temp.RDS"))




#my point data
datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))
curves <- readRDS(here('processed-data', 'wild-tpcs.Rds'))

marine <- datasets %>% #get dataset that i can get lat/longs for
  filter(land_or_sea == "oceanic") %>%
  filter(!(is.na(latitude))) %>%
  filter(!(is.na(longitude)))
# add treatment info
marine <- marine %>%
  left_join(curves %>% select(curve_ID, treatment_1_type, treatment_2_type, treatment_2_group, species_ID), join_by(curve_ID)) %>%
  distinct()
# at collection time info
study <- read.csv(here('raw-data', 'data_extraction_21_10_2025_ study_ID.csv')) %>%
  select(study_ID, data_collection_year, pub_year)
study <- study %>%
  filter(study_ID %in% marine$study_ID)
marine <- marine %>%
  left_join(study %>% select(study_ID, data_collection_year, pub_year), join_by(study_ID)) %>%
  distinct()
unique_lat_long <- marine %>%
  select(study_ID, latitude, longitude, species_ID, data_collection_year, pub_year) %>%
  distinct()
my_points <- unique_lat_long %>%
  select(longitude, latitude) 

new_my_points <- vect(unique_lat_long, geom = c("longitude", "latitude"), crs = crs(r_temp))
library(tidyterra)
ggplot() +
  geom_spatraster(data = r_temp1982_01to2025_09[[1]]) +
  geom_spatvector(data = new_my_points, color = "red")

sst_list <- vector("list", nlyr(r_temp1982_01to2025_09))

# Loop over each SST layer and extract values
for (i in seq_len(nlyr(r_temp1982_01to2025_09))) {
  message("Extracting layer: ", i)
  sst_vals <- terra::extract(
    r_temp1982_01to2025_09[[i]],
    new_my_points,
    method = "simple",
    search_radius = 30000
  )
  sst_vals$layer <- i
  sst_vals$date <- as.Date(names(r_temp1982_01to2025_09)[i])
  sst_list[[i]] <- sst_vals
}

sst_all <- dplyr::bind_rows(sst_list)
sst_wide <- sst_all %>%
  select(ID, distance, layer, matches("^\\d{4}-\\d{2}-\\d{2}$")) %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "var",
    values_to = "sst_value"
  ) %>%
  filter(!is.na(sst_value)) %>%
  # spread layers back out into columns 
  select(ID, var, distance, sst_value) %>% 
  pivot_wider(names_from = var, values_from = sst_value ) %>% 
  arrange(ID)

#join info back
sst_wide$study_ID = unique_lat_long$study_ID
sst_wide$species_ID = unique_lat_long$species_ID
sst_wide$latitude = unique_lat_long$latitude
sst_wide$longitude = unique_lat_long$longitude

sst_wide <- sst_wide %>%
  select(latitude, longitude, study_ID, species_ID, distance, everything())
sst_stats <- sst_wide %>%
  rowwise() %>%  # operate across columns for each row
  mutate(
    sst_mean   = mean(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_sd     = sd(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_median = median(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_min    = min(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_max    = max(c_across("1982-01-01":"2025-09-01"), na.rm = TRUE),
    sst_range  = sst_max - sst_min
  ) %>%
  ungroup()
##save raw temp file
saveRDS(sst_stats, file = here("processed-data", "marine_sst_raw_temp.RDS"))

sst_stats <- sst_stats %>%
  select(latitude, longitude, study_ID, species_ID, sst_mean, sst_sd, sst_median, sst_min, sst_max, sst_range, distance, everything())
marine <- marine %>%
  left_join(sst_stats %>% select(latitude, longitude, sst_mean, sst_sd, sst_median, sst_min, sst_max, sst_range), join_by(latitude, longitude)) %>%
  distinct()

saveRDS(marine, file = here("processed-data", "marine_sstemp_data.RDS"))
