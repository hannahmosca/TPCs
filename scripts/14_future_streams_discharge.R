### the script is to work with masking the freshwater values with no discharge###
rm(list=ls())
#load libraries
library(terra)
library(ncdf4)
library(here)
library(dplyr)
library(matrixStats)
library(ggplot2)
install.packages("rnaturalearthdata")
install.packages("rnaturalearth")
library(rnaturalearth)
library(rnaturalearthdata)
##load temp raster and load discharge raster files
freshwater_monthly <- rast((here("processed-data", "freshwater_monthly.nc")))
names(freshwater_monthly)

#need to rename monthly values
dates <- seq(as.Date("1982-01-01"), as.Date("2025-09-01"), by = "month")
names(freshwater_monthly) <- dates

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
new_layers <- 
discharge <- rast(here("raw-data", "discharge_Avg.nc"))
coastline <- ne_coastline(returnclass = "sf", scale = 110)










#check the spatial stuff, ensure all matches
res(discharge)
ext(discharge)
ext(freshwater_annual)
res(freshwater_annual)
nlyr(freshwater_annual) #36 layers
nlyr(discharge) #1 layer
names(freshwater_annual)

## 
Qlim <- 3 # this defines the limit for discharge
discharge_mask <- discharge >= Qlim
land_mask <- discharge <= Qlim #want these values to be black
plot(discharge_mask[[1]])
plot(land_mask[[1]])

#should likely make this a mean first, before masking beause it will be faster
freshwater_masked <- mask(freshwater_annual, discharge_mask, maskvalues = FALSE)
df <- as.data.frame(freshwater_masked, xy = TRUE, na.rm = TRUE)

ggplot(df) +
  geom_raster(aes(x = x, y = y, fill = temp_mean), interpolate = TRUE) +
  scale_fill_viridis_c(option = "C") +
  geom_sf(data = coastline,
          color = "black",
          fill = NA,
          size = 0.9) +
  coord_sf(xlim = c(-180, 180),
           ylim = c(-50, 90)) +
  theme_void() +
  theme(legend.position = "none")

df <- df %>%
  mutate(across("waterTemp_1":"waterTemp_36", ~ .x - 273.15))
temp_matrix <- as.matrix(df[, -c(1, 2)])
df$temp_mean   <- rowMeans(temp_matrix, na.rm = TRUE)
df$temp_sd     <- rowSds(temp_matrix, na.rm = TRUE)
df$temp_median <- rowMedians(temp_matrix, na.rm = TRUE)
df$temp_min    <- rowMins(temp_matrix, na.rm = TRUE)
df$temp_max    <- rowMaxs(temp_matrix, na.rm = TRUE)
df$temp_range  <- df$temp_max - df$temp_mean
df$q_low <- rowQuantiles(temp_matrix, probs = 0.025, na.rm = TRUE)
df$q_high <- rowQuantiles(temp_matrix, probs = 0.975, na.rm = TRUE)

fitted_datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))

ggplot(df_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = q_low, ymax = q_high), fill = "lightgreen", alpha = .6, size = 1.2) +
  geom_line(aes (y = temp_mean), color = "darkgreen", size = 2) +
  geom_point(data = fitted_datasets %>%
               filter(land_or_sea == "terrestrial") %>%
               filter(topt_TF == TRUE), aes(x = latitude, y = topt), color = "black", alpha = .4) +
  labs(x = "Latitude", y = "Water Temperature (°C)", title = "qlim=20") +
  theme_classic()


df_coarse <- df %>%
  mutate(lat_bin = cut(y, breaks = seq(floor(min(y)),
                                              ceiling(max(y)),
                                              by = 1))) %>% 
  group_by(lat_bin) %>%
  summarise(
    latitude = mean(y, na.rm = TRUE),  
    temp_mean = mean(temp_mean, na.rm = TRUE),
    temp_min = min(temp_min, na.rm = TRUE),
    temp_max = max(temp_max, na.rm = TRUE),
    q_low = quantile(q_low, probs = 0.025, na.rm = TRUE),
    q_high = quantile(q_high, probs = 0.975, na.rm = TRUE),
    temp_median = median(temp_median, na.rm = TRUE)
  )


# could try with weekly output file so I have the same xt and res for when I try with averaged monthly
waterTemp_weekAvg_output_E2O_hist_1996-01-07_to_2005-12-30.nc