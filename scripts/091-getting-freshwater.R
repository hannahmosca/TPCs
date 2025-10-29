### new script for working with future streams data ###
#required packages
library(ncdf4)
library(terra)
library(here)
#14 year file chunks, historical and present weekly, need to get them to 1 (monthly from 1982-2025)

## 1979 thr 1985
file1979thr1985 <- "waterTemp_weekAvg_output_E2O_hist_1979-01-07_to_1985-12-30.nc"
r_temp1979thr1985 <- rast((here("raw-data", file1979thr1985)), subds = "waterTemperature")
time_values <- time(r_temp1979thr1985)
layer_names <- format(as.Date(time_values), "%Y-%m-%d")
names(r_temp1979thr1985) <- layer_names
names(r_temp1979thr1985)
layer_1981 <- names(r_temp1979thr1985)[[157]]
r_temp1981thr1985 <- subset(r_temp1979thr1985, 157:364)
## now have 1982 to 1985
names(r_temp1981thr1985) #208 weeks: starting from 1982-01-07 to 1985-12-30

## 1986 thr 1995
file1986thr1995 <- "waterTemp_weekAvg_output_E2O_hist_1986-01-07_to_1995-12-30.nc"
r_temp1986thr1995 <- rast((here("raw-data", file1986thr1995)), subds = "waterTemperature")
time_values <- time(r_temp1986thr1995)
layer_names <- format(as.Date(time_values), "%Y-%m-%d")
names(r_temp1986thr1995) <- layer_names
names(r_temp1986thr1995) #520 weeks: starting from 1986-01-07 to 1995-12-30
#merge rasters
mergedr_temp <- merge(r_temp1981thr1985, r_temp1986thr1995)
