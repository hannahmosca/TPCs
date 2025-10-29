### new script for working with future streams data ###
#required packages
rm(list=ls())
library(ncdf4)
library(terra)
library(here)
library(dplyr)
library(tidyverse)
#14 year file chunks, historical and present weekly, need to get them to 1 (monthly from 1982-2025)

## 1979 thr 1985
file1979thr1985 <- "waterTemp_weekAvg_output_E2O_hist_1979-01-07_to_1985-12-30.nc"
r_temp1979thr1985 <- rast((here("raw-data", file1979thr1985)), subds = "waterTemperature")
time_values <- time(r_temp1979thr1985)
layer_names <- format(as.Date(time_values), "%Y-%m-%d")
names(r_temp1979thr1985) <- layer_names
names(r_temp1979thr1985)
layer_1981 <- names(r_temp1979thr1985)[[157]]
r_temp1982thr1985 <- subset(r_temp1979thr1985, 157:364)
## now have 1982 to 1985
names(r_temp1982thr1985) #208 weeks: starting from 1982-01-07 to 1985-12-30

## 1986 thr 1995
file1986thr1995 <- "waterTemp_weekAvg_output_E2O_hist_1986-01-07_to_1995-12-30.nc"
r_temp1986thr1995 <- rast((here("raw-data", file1986thr1995)), subds = "waterTemperature")
time_values <- time(r_temp1986thr1995)
layer_names <- format(as.Date(time_values), "%Y-%m-%d")
names(r_temp1986thr1995) <- layer_names
names(r_temp1986thr1995) #520 weeks: starting from 1986-01-07 to 1995-12-30

## 1996 thr 2005
file1996thr2005 <- "waterTemp_weekAvg_output_E2O_hist_1996-01-07_to_2005-12-30.nc"
r_temp1996thr2005 <- rast((here("raw-data", file1996thr2005)), subds = "waterTemperature")
time_values <- time(r_temp1996thr2005)
layer_names <- format(as.Date(time_values), "%Y-%m-%d")
names(r_temp1996thr2005) <- layer_names
names(r_temp1996thr2005) #520 weeks: starting from 1996-01-07 to 2005-12-30


#merge all rasters
freshwater_r_temp_1982thr2005 <- c(r_temp1982thr1985, r_temp1986thr1995, r_temp1996thr2005)
names(freshwater_r_temp_1982thr2005)
crs(freshwater_r_temp_1982thr2005) 
ext(freshwater_r_temp_1982thr2005)
res(freshwater_r_temp_1982thr2005)     
ncell(freshwater_r_temp_1982thr2005)    
nlyr(freshwater_r_temp_1982thr2005)    

# #change resolution to match SST dataset resolution! wait to do this until i have all the data/its taking too long #
# res(freshwater_r_temp_1982thr1995) # = 0.08333333 0.08333334, and sst is  = 0.25 0.25
# #load sst raster
# filename2 <- "sst.mon.mean.nc" # sst data, 529 monthly means from 
# sst = rast((here("raw-data", filename2)), subds = "sst")
# res(sst)

# # Resample the finer freshwater raster to match the SST raster's resolution
# freshwater_r_temp_1982thr1995_resampled <- terra::resample(
#   freshwater_r_temp_1982thr1995,  # finer
#   sst,                            # coarser
#   method = "bilinear"           
# )
# 
# plot(freshwater_r_temp_1982thr1995_resampled[[1]])
  
#my point data
datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))
curves <- readRDS(here('processed-data', 'wild-tpcs.Rds'))

lat_long <- curves %>%
  select(study_ID, latitude, longitude, habitat) %>%
  distinct()
freshwater <- datasets %>% #get dataset that i can get lat/longs for
  filter(land_or_sea == "terrestrial") %>%
  filter(!(is.na(latitude))) %>%
  filter(!(is.na(longitude)))
# add treatment info
freshwater <- freshwater %>%
  left_join(curves %>% select(curve_ID, treatment_1_type, treatment_2_type, treatment_2_group, species_ID), join_by(curve_ID)) %>%
  distinct()
# add collection time info
study <- read.csv(here('raw-data', 'data_extraction_21_10_2025_ study_ID.csv')) %>%
  select(study_ID, data_collection_year, pub_year)
study <- study %>%
  filter(study_ID %in% freshwater$study_ID)

freshwater <- freshwater %>%
  left_join(study %>% select(study_ID, data_collection_year, pub_year), join_by(study_ID)) %>%
  distinct()
unique_lat_long <- freshwater %>%
  select(study_ID, latitude, longitude, species_ID, data_collection_year, pub_year) %>%
  distinct()
my_points <- unique_lat_long %>%
  select(longitude, latitude) 

#check where points fall, some estuaries to be dealt with
new_my_points <- vect(unique_lat_long, geom = c("longitude", "latitude"), crs = crs(freshwater_r_temp_1982thr1995))
#tidyterra needs to be loaded, but it messes with tidy
ggplot() +
  geom_spatraster(data = freshwater_r_temp_1982thr1995[[12]], aes(fill = 1982-03-25)) +
  geom_spatvector(data = new_my_points, color = "red")

###extract only temp values for my points
temp_list <- vector("list", nlyr(freshwater_r_temp_1982thr1995))

for (i in seq_len(nlyr(freshwater_r_temp_1982thr1995))) {
  message("Extracting layer: ", i)
  # extract temp values for each point
  temp_vals <- terra::extract(
    freshwater_r_temp_1982thr1995[[i]],
    new_my_points,
    method = "simple",
    search_radius = 30000
  ) #add layer identifier and date
  temp_vals$layer <- i
  temp_vals$date <- as.Date(names(freshwater_r_temp_1982thr1995)[i])
  
  temp_list[[i]] <- temp_vals
}
temp_all <- bind_rows(temp_list)
temp_wide <- temp_all %>%
  select(ID, distance, layer, matches("^\\d{4}-\\d{2}-\\d{2}$")) %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "var",
    values_to = "watertemp_value"
  ) %>%
  filter(!is.na(watertemp_value)) %>%
  # spread layers back out into columns 
  select(ID, var, distance, watertemp_value) %>% 
  pivot_wider(names_from = var, values_from = watertemp_value ) %>% 
  arrange(ID)
#add back details
temp_wide$study_ID = unique_lat_long$study_ID
temp_wide$species_ID = unique_lat_long$species_ID
temp_wide$latitude = unique_lat_long$latitude
temp_wide$longitude = unique_lat_long$longitude
temp_wide <- temp_wide %>%
  select(latitude, longitude, study_ID, species_ID, distance, everything())

date_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(temp_wide), value = TRUE)
date_months <- format(as.Date(date_cols), "%Y-%m")

##now make monthly instead of weekly
monthly_means <- sapply(unique(date_months), function(m) {
  cols <- date_cols[date_months == m]
  rowMeans(temp_wide[, cols], na.rm = TRUE)
})
temp_monthly <- cbind(
  temp_wide[, c("latitude", "longitude", "study_ID", "species_ID", "distance")],
  as.data.frame(monthly_means)
) %>%
  select(latitude, longitude, study_ID, species_ID, distance, everything())

##now go from kelvin to celius
temp_monthly <- temp_monthly %>%
  mutate(across(`1982-01`:`2005-12`, ~ .x - 273.15))

###for now, flagging 2_0047, 1_0019 needs to be in marine, 2_0093 is an estuary
temp_wide_unflagged <- temp_monthly %>%
  filter(!(study_ID %in% c("2_0093", "1_0019", "2_0047")))
freshwater_temperatures <- temp_wide_unflagged %>%
  rowwise() %>%  # operate across columns for each row
  mutate(
    temp_mean   = mean(c_across(`1982-01`:`2005-12`), na.rm = TRUE),
    temp_sd     = sd(c_across(`1982-01`:`2005-12`), na.rm = TRUE),
    temp_median = median(c_across(`1982-01`:`2005-12`), na.rm = TRUE),
    temp_min    = min(c_across(`1982-01`:`1995-12`), na.rm = TRUE),
    temp_max    = max(c_across(`1982-01`:`2005-12`), na.rm = TRUE),
    temp_range  = temp_max - temp_min
  ) %>%
  ungroup()
freshwater_temperatures <- freshwater_temperatures %>%
  select(latitude, longitude, study_ID, species_ID, temp_mean, temp_sd, temp_median, temp_min, temp_max, temp_range, distance, everything())

freshwater_unflagged <- freshwater %>%
  filter(study_ID %in% freshwater_temperatures$study_ID)
freshwater_unflagged <- freshwater_unflagged %>%
  left_join(freshwater_temperatures %>% select(latitude, longitude, temp_mean, temp_sd, temp_median, temp_min, temp_max, temp_range), join_by(latitude, longitude)) %>%
  distinct()
saveRDS(freshwater_unflagged, file = here("processed-data", "freshwater_temp_data.RDS"))

