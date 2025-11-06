### this is a script to get / work with freshwater temperature data from future streams ###

# first, I am merging 10-15 yr raster chunks of weekly temp data to get a raster of monthly averages from 1982-2025
# then, I am extracting point data for our collection lat and longs for the freshwater fish

#required packages
library(ncdf4)
library(terra)
library(here)
library(dplyr)
library(tidyverse)

#### getting temp from 1982-2025 ####
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

## 2006 thr 2019
file2006thr2019 <- "waterTemp_weekAvg_output_hadgem_rcp4p5_2006-01-07_to_2019-12-30.nc"
r_temp2006thr2019 <- rast((here("raw-data", file2006thr2019)), subds = "waterTemperature")
time_values <- time(r_temp2006thr2019)
layer_names <- format(as.Date(time_values), "%Y-%m-%d")
names(r_temp2006thr2019) <- layer_names
names(r_temp2006thr2019) #728 weeks: starting from 2006-01-07 to 2019-12-30


## 2020 thr 2029
file2020thr2029 <- "waterTemp_weekAvg_output_hadgem_rcp4p5_2020-01-07_to_2029-12-30.nc"
r_temp2020thr2029 <- rast((here("raw-data", file2020thr2029)), subds = "waterTemperature")
time_values <- time(r_temp2020thr2029)
layer_names <- format(as.Date(time_values), "%Y-%m-%d")
names(r_temp2020thr2029) <- layer_names
names(r_temp2020thr2029) # want only through sept 2025
r_temp2020thr2025 <- subset(r_temp2020thr2029, 1:299)
names(r_temp2020thr2025) #299 weeks: starting from 2020-01-07 to 2025-09-30

#merge all rasters
freshwater_r_temp <- c(r_temp1982thr1985, r_temp1986thr1995, r_temp1996thr2005, r_temp2006thr2019, r_temp2020thr2025)
names(freshwater_r_temp)
crs(freshwater_r_temp) 
ext(freshwater_r_temp)
res(freshwater_r_temp)     
ncell(freshwater_r_temp)    
nlyr(freshwater_r_temp)    

#make more space
rm(r_temp1979thr1985)
rm(r_temp1982thr1985)
rm(r_temp1986thr1995)
rm(r_temp1996thr2005)
rm(r_temp2006thr2019)
rm(r_temp2020thr2025)
rm(r_temp2020thr2029)

#### freshwater temp data for all locations monthly averages ####
#convert names to dates
dates <- as.Date(names(freshwater_r_temp))
month_group <- format(dates, "%Y-%m")
unique_month_group <- unique(month_group)
month <- as.Date(paste0(unique_month_group, "-01"))
#go from weekly to monthly
r_monthly <- tapp(freshwater_r_temp, month_group, mean)  
rm(freshwater_r_temp)
names(r_monthly)
head(r_monthly)
res(r_monthly)
#save file locally so don't have to do this computation again
writeCDF(r_monthly, filename = here("processed-data", "freshwater_monthly.nc"))


#### filtering out extremes for all locations freshwater ####
freshwater_monthly <- "freshwater_monthly.nc"
freshwater_monthly <- rast((here("raw-data", freshwater_monthly)), subds = "waterTemperature")
dim(freshwater_monthly) #before removing any values 2160 4320  525

threshold <- 350 #76.86 dg celcius
freshwater_monthly[freshwater_monthly > threshold] <- NA
freshwater_monthly_thr <- freshwater_monthly
names(freshwater_monthly_thr) <- month
#getting non thresholded data to compare
freshwater_monthly <- "freshwater_monthly.nc"
freshwater_monthly <- rast((here("raw-data", freshwater_monthly)), subds = "waterTemperature")
names(freshwater_monthly) <- month

#make coarser
freshwater_monthly_coarser <- aggregate(freshwater_monthly, fact = 2, fun = mean, na.rm = TRUE)
freshwater_monthly_thr_coarser <- aggregate(freshwater_monthly_thr, fact = 2, fun = mean, na.rm = TRUE)

res(freshwater_monthly_coarser)
plot(freshwater_monthly[[8]])
plot(freshwater_monthly_thr[[8]])
plot(freshwater_monthly_coarser[[8]])
plot(freshwater_monthly_thr_coarser[[8]])
plot(freshwater_monthly_thr[[8]])
#make dfs, one thresholded, one not
monthly_fresh_df <- as.data.frame(freshwater_monthly_coarser, xy = TRUE, cells = FALSE, na.rm = TRUE)
monthly_fresh_thresholded_df <- as.data.frame(freshwater_monthly_thr_coarser, xy = TRUE, cells = FALSE, na.rm = TRUE)

#change from kelvin to celcius
monthly_fresh_df <- monthly_fresh_df %>%
  mutate(across("1982-01-01":"2025-09-01", ~ .x - 273.15))
monthly_fresh_thresholded_df <- monthly_fresh_thresholded_df %>%
  mutate(across("1982-01-01":"2025-09-01", ~ .x - 273.15))
#rename to get lat and long
monthly_fresh_df <- monthly_fresh_df %>%
  rename(longitude = x) %>%
  rename(latitude = y)
monthly_fresh_thresholded_df <- monthly_fresh_thresholded_df %>%
  rename(longitude = x) %>%
  rename(latitude = y)

#### compute summary stats for all lat/long data ####
install.packages("matrixStats")
library(matrixStats)
#compute summary for not thresholded
temp_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(monthly_fresh_df))
temp_matrix <- as.matrix(monthly_fresh_df[, temp_cols])
monthly_fresh_df$temp_mean <- rowMeans(temp_matrix, na.rm = TRUE)
monthly_fresh_df$temp_sd <- rowSds(temp_matrix, na.rm = TRUE)
monthly_fresh_df$temp_median <- rowMedians(temp_matrix, na.rm = TRUE)
monthly_fresh_df$temp_min <- rowMins(temp_matrix, na.rm = TRUE)
monthly_fresh_df$temp_max <- rowMaxs(temp_matrix, na.rm = TRUE)
monthly_fresh_df$q_low <- rowQuantiles(temp_matrix, probs = 0.025, na.rm = TRUE)
monthly_fresh_df$q_high <- rowQuantiles(temp_matrix, probs = 0.975, na.rm = TRUE)
monthly_fresh_df$temp_range <- monthly_fresh_df$temp_max - monthly_fresh_df$temp_min
#compute summary for thresholded
temp_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(monthly_fresh_thresholded_df))
temp_matrix <- as.matrix(monthly_fresh_thresholded_df[, temp_cols])
monthly_fresh_thresholded_df$temp_mean <- rowMeans(temp_matrix, na.rm = TRUE)
monthly_fresh_thresholded_df$temp_sd <- rowSds(temp_matrix, na.rm = TRUE)
monthly_fresh_thresholded_df$temp_median <- rowMedians(temp_matrix, na.rm = TRUE)
monthly_fresh_thresholded_df$temp_min <- rowMins(temp_matrix, na.rm = TRUE)
monthly_fresh_thresholded_df$temp_max <- rowMaxs(temp_matrix, na.rm = TRUE)
monthly_fresh_thresholded_df$q_low <- rowQuantiles(temp_matrix, probs = 0.025, na.rm = TRUE)
monthly_fresh_thresholded_df$q_high <- rowQuantiles(temp_matrix, probs = 0.975, na.rm = TRUE)
monthly_fresh_thresholded_df$temp_range <- monthly_fresh_thresholded_df$temp_max - monthly_fresh_thresholded_df$temp_min


####save all lat and long thresholded and unthresholded data ####
saveRDS(monthly_fresh_df, here("processed-data", "freshwater_all_df_no_threshold.RDS"))
saveRDS(monthly_fresh_thresholded_df, here("processed-data", "freshwater_all_df_threshold.RDS"))


#### freshwater temp data for my lat and long points ####
#load my point data and curve data
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
#adding collection time info?
freshwater <- freshwater %>%
  left_join(study %>% select(study_ID, data_collection_year, pub_year), join_by(study_ID)) %>%
  distinct()
unique_lat_long <- freshwater %>%
  select(study_ID, latitude, longitude, species_ID, data_collection_year, pub_year) %>%
  distinct()
my_points <- unique_lat_long %>%
  select(longitude, latitude) 

#check where points fall, some estuaries to be dealt with
new_my_points <- vect(unique_lat_long, geom = c("longitude", "latitude"), crs = crs(freshwater_r_temp))
#tidyterra needs to be loaded, but it messes with tidy
ggplot() +
  geom_spatraster(data = freshwater_r_temp[[12]], aes(fill = 1982-03-25)) +
  geom_spatvector(data = new_my_points, color = "red")

###extract only temp values for my points
temp_list <- vector("list", nlyr(freshwater_r_temp))

for (i in seq_len(nlyr(freshwater_r_temp))) {
  message("Extracting layer: ", i)
  # extract temp values for each point
  temp_vals <- terra::extract(
    freshwater_r_temp[[i]],
    new_my_points,
    method = "simple",
    search_radius = 30000
  ) #add layer identifier and date
  temp_vals$layer <- i
  temp_vals$date <- as.Date(names(freshwater_r_temp)[i])
  
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
temp_wide <- temp_wide[-42,]
#add back details
temp_wide$study_ID = unique_lat_long$study_ID
temp_wide$species_ID = unique_lat_long$species_ID
temp_wide$latitude = unique_lat_long$latitude
temp_wide$longitude = unique_lat_long$longitude
temp_wide <- temp_wide %>%
  select(latitude, longitude, study_ID, species_ID, distance, everything())

date_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(temp_wide), value = TRUE)
date_months <- format(as.Date(date_cols), "%Y-%m")
##threshold the values
threshold <- 350 # 76.86°C
temp_wide_thresholded <- temp_wide %>%
  mutate(across(`1982-01-07`:`2025-09-30`,
                ~ ifelse(.x > threshold, NA, .x)))

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
## now for thresholded data
monthly_means <- sapply(unique(date_months), function(m) {
  cols <- date_cols[date_months == m]
  rowMeans(temp_wide_thresholded[, cols], na.rm = TRUE)
})
temp_monthly_thresholded <- cbind(
  temp_wide_thresholded[, c("latitude", "longitude", "study_ID", "species_ID", "distance")],
  as.data.frame(monthly_means)
) %>%
  select(latitude, longitude, study_ID, species_ID, distance, everything())

##now go from kelvin to celius
temp_monthly <- temp_monthly %>%
  mutate(across(`1982-01`:`2025-09`, ~ .x - 273.15))
temp_monthly_thresholded <- temp_monthly_thresholded %>%
  mutate(across(`1982-01`:`2025-09`, ~ .x - 273.15))


#### look at distributions of temp across my lat ####
temp_monthly_long <- temp_monthly %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}$"),
    names_to = "date",
    values_to = "temperature"
  )
temp_monthly_thresholded_long <- temp_monthly_thresholded %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}$"),
    names_to = "date",
    values_to = "temperature"
  )
ggplot(temp_monthly_long, aes(x = temperature)) +
  geom_histogram(binwidth = .5, fill = "lightgreen", color = "white", alpha = .5) +
  labs(
    x = "Experienced Temperatures",
    y = "Frequency") +
  theme_classic()

ggplot(temp_monthly_thresholded_long, aes(x = temperature)) +
  geom_histogram(binwidth = .5, fill = "lightgreen", color = "white", alpha = .5) +
  labs(
    x = "Experienced Temperatures",
    y = "Frequency") +
  theme_classic()
##no difference from thresholding my point data..


###for now, flagging 2_0047, 1_0019 needs to be in marine, 2_0093 is an estuary
temp_wide_unflagged <- temp_monthly %>%
  filter(!(study_ID %in% c("2_0093", "1_0019", "2_0047")))
freshwater_temperatures <- temp_wide_unflagged %>%
  rowwise() %>%  # operate across columns for each row
  mutate(
    temp_mean   = mean(c_across(`1982-01`:`2025-09`), na.rm = TRUE),
    temp_sd     = sd(c_across(`1982-01`:`2025-09`), na.rm = TRUE),
    temp_median = median(c_across(`1982-01`:`2025-09`), na.rm = TRUE),
    temp_min    = min(c_across(`1982-01`:`2025-09`), na.rm = TRUE),
    temp_max    = max(c_across(`1982-01`:`2025-09`), na.rm = TRUE),
    temp_q_low  = quantile(c_across(`1982-01`:`2025-09`), probs = 0.025, na.rm = TRUE),
    temp_q_high = quantile(c_across(`1982-01`:`2025-09`), probs = 0.975, na.rm = TRUE),
    temp_range  = temp_max - temp_min
  ) %>%
  ungroup()
## save raw temp file
saveRDS(freshwater_temperatures, file = here("processed-data", "freshwater_temperatures_my_points.RDS"))



