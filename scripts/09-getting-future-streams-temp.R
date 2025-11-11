### this is a script to get freshwater temperature data from future streams ###
# goal: to get a raster of monthly averages from 1982-2025

### loading/installing required packages ###
library(ncdf4)
library(terra)
library(here)
library(dplyr)
library(tidyverse)

#### 01: Merge 10-15 yr raster chunks of weekly temp data ####
#14 year file chunks, historical and present weekly 

  ## 1979 thr 1985
  file1979thr1985 <- "waterTemp_weekAvg_output_E2O_hist_1979-01-07_to_1985-12-30.nc"
  r_temp1979thr1985 <- rast((here("raw-data", file1979thr1985)), subds = "waterTemperature")
  time_values <- time(r_temp1979thr1985)
  layer_names <- format(as.Date(time_values), "%Y-%m-%d")
  names(r_temp1979thr1985) <- layer_names
  names(r_temp1979thr1985)
  layer_1981 <- names(r_temp1979thr1985)[[157]]
  r_temp1982thr1985 <- subset(r_temp1979thr1985, 157:364)
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

## merge rasters 
  freshwater_r_temp <- c(r_temp1982thr1985, r_temp1986thr1995, r_temp1996thr2005, r_temp2006thr2019, r_temp2020thr2025)

#check raster out
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

<<<<<<< HEAD
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
freshwater_monthly <- r_monthly
names(freshwater_monthly) <- month
names(freshwater_monthly)
#save file locally so don't have to do this computation again
writeCDF(freshwater_monthly, filename = here("processed-data", "freshwater_monthly.nc"))
=======
#### 02: average across weeks to get monthly ####
  ##filter out high values//make them NA
  threshold <- 350 # 76.86°C
  freshwater_r_temp[freshwater_r_temp > 350] <- NA
  
  ##convert to celcius
  freshwater_r_temp_cel <- freshwater_r_temp - 273.15
  
  #naming thing
  dates <- as.Date(names(freshwater_r_temp_cel))  # assuming layer names are dates
  month_group <- format(dates, "%Y-%m")
  
  ##average from weekly → monthly
  r_monthly <- tapp(freshwater_r_temp_cel, month_group, function(x) mean(x, na.rm = TRUE))
  
  ## adjust monthly layer names
  unique_month_group <- unique(month_group)
  month <- as.Date(paste0(unique_month_group, "-01"))
  names(r_monthly) <- month
  
  #make space/check out raster
  rm(freshwater_r_temp)
  names(r_monthly)
  head(r_monthly)
  res(r_monthly)

  
#### 03: save file locally so don't have to do this computation again ####
writeCDF(r_monthly, filename = here("processed-data", "freshwater_monthly.nc"))
>>>>>>> 4c96e9213abb73a2150a99bc40f6d13db4449252

#### 04: compute summary stats on raster ####
  ## load in raster, check names/rename if neccessary
  freshwater_monthly <- rast((here("processed-data", "freshwater_monthly.nc")))
  names(freshwater_monthly)
  #need to rename monthly values
  dates <- seq(as.Date("1982-01-01"), as.Date("2025-09-01"), by = "month")
  names(freshwater_monthly) <- dates
  
  ## computing summary stats across layers
  freshwater_summary <- app(
    freshwater_monthly,
    fun = function(x) {
      c(mean = mean(x, na.rm = TRUE),
        sd   = sd(x, na.rm = TRUE),
        min  = min(x, na.rm = TRUE),
        max  = max(x, na.rm = TRUE),
        q2.5 = quantile(x, 0.025, na.rm = TRUE),
        q97.5= quantile(x, 0.975, na.rm = TRUE))
    }
  )
  ## convert across to celcius
  freshwater_summary_cel <- freshwater_summary - 273.15
  
  ## save as seperate file
  writeCDF(freshwater_summary_cel, filename = here("processed-data", "freshwater_monthly_summarized.nc"))

#### 05: masking discharge ####
  rm(list=ls()) #make room/clean environment
  ##load data
  freshwater_temp <- rast((here("processed-data", "freshwater_summary_cel"))) #average across months from 1982-2025
  discharge <- rast(here("raw-data", "discharge_Avg.nc"))
  coastline <- ne_coastline(returnclass = "sf", scale = 110)
  
  ## check alignment
  ext(freshwater_temp)
  ext(discharge)
  res(freshwater_temp)
  res(discharge)
  
  
#### 06: extracting mypoint data ####
  ## load required datasets
  datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))
  curves <- readRDS(here('processed-data', 'wild-tpcs.Rds'))

  #get freshwater fish
  freshwater <- datasets %>% 
    filter(land_or_sea == "terrestrial") %>%
    filter(!(is.na(latitude))) %>%
    filter(!(is.na(longitude))) 
  #get lat/long
  unique_lat_long <- freshwater %>%
    select(latitude, longitude) %>%
    distinct()
  #check where points fall, some estuaries to be dealt with
  new_my_points <- vect(unique_lat_long, geom = c("longitude", "latitude"), crs = crs(freshwater_temp))

  