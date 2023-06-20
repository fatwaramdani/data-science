# Set working directory
setwd("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 7/Tsukubadai-24May2023_psscene_analytic_8b_udm2/files")

# load required packages
library(raster)
library(rgdal)

# Importing satellite data
tsukuba <- brick("20230524_002457_23_2455_3B_AnalyticMS_8b_clip.tif")

# Check metadata
print(tsukuba)

# RGB combination - natural colour (Red-Green-Blue)
plotRGB(tsukuba, r=6, g=4, b=2, stretch = "lin")
plotRGB(tsukuba, r=6, g=4, b=2, stretch = "hist")

# RGB combination - false colour of vegetation (NIR-Red-Green)
plotRGB(tsukuba, r=8, g=6, b=4, stretch = "lin")

# RGB combination - false colour of urban area (Green-NIR-Blue)
plotRGB(tsukuba, r=4, g=8, b=2, stretch = "lin")


# Calculate NDVI
bandRed <- raster(tsukuba, layer=4)
bandNIR <- raster(tsukuba, layer=8)
NDVI <- (bandNIR-bandRed)/(bandNIR+bandRed)
plot(NDVI, col= rev(terrain.colors(10)))

# Define threshold values for low, medium, and high classes
low_threshold <- 0.0
medium_threshold <- 0.2
high_threshold <- 0.4

# Create binary masks for each class
low_mask <- NDVI <= low_threshold
medium_mask <- NDVI > low_threshold & NDVI <= medium_threshold
high_mask <- NDVI > medium_threshold & NDVI <= high_threshold

# Calculate the area of each class
low_area <- cellStats(low_mask, sum)* 0.0001
medium_area <- cellStats(medium_mask, sum)* 0.0001
high_area <- cellStats(high_mask, sum)* 0.0001

# Print the results
cat("Low NDVI Area:", low_area, "ha")
cat("Medium NDVI Area:", medium_area, "ha")
cat("High NDVI Area:", high_area, "ha")
