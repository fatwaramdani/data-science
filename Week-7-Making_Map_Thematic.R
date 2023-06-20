# Set working directory
setwd("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 7/jpn_adm_2019_shp")

# load required packages
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)

# read in the shapefile data
japan_admin <- st_read("jpn_admbnda_adm1_2019.shp")

# Plot map (1)
ggplot() + 
  geom_sf(data = japan_admin)

# Plot map (2)
tm_shape(japan_admin) + 
  tm_polygons()+
  tmap_options(check.and.fix = TRUE)

# customize the thematic map
tm_shape(japan_admin) + 
  tm_polygons("ADM1_JA")+
  tmap_options(check.and.fix = TRUE)

# interactive map
tmap_mode("view")

tm_shape(japan_admin) + 
  tm_polygons("ADM1_JA", title = "Prefectures")+
  tmap_options(check.and.fix = TRUE)
