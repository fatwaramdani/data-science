# Set working directory
setwd("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 7/ne_110m_admin_0_countries")

# Install and load required packages
install.packages(c("sf", "tmap", "leaflet"))
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
library(leaflet)

# 1. DATA PREPARATION
# Import CSV
library(readr)
AWH <- read_csv("annual-working-hours-vs-gdp-per-capita-pwt.csv")

#Rename only the 1st, 4th to 6th column names
colnames(AWH)[c(1, 4,5,6)] <- c("Country", "WH", "GDP", "POP")

#Delete NA cell
selected_data <- na.omit(AWH)
View(selected_data)

# read in the shapefile data
world_admin <- st_read("ne_110m_admin_0_countries.shp")

# Delete unnecessary columns except Country's name
world <- world_admin %>% select(NAME_LONG)

# Rename header
colnames(world)[c(1)] <- c("Country")

# joins data to the shapefile
world_wh <- merge(world, selected_data, by="Country")

# 2. DATA VISUALIZATION
#Creating quick map
qtm(world_wh, fill = "WH")

# includes a histogram in the legend
tm_shape(world_wh) + 
  tm_fill("WH", style = "quantile", n = 5,
          palette = "Reds", legend.hist = TRUE)

# adds in layout, gets rid of frame
tm_shape(world_wh) + 
  tm_fill("WH", palette = "Reds",
          style = "quantile", title = "Annual working hour per worker") +
  tm_borders(alpha=.4) +
  tm_compass(type="arrow", position=c("right", "top")) +
  tm_layout(main.title = "World's annual working hours",
            main.title.position = "center",
            main.title.color = "blue", 
            legend.text.size = 0.7,
            legend.title.size = 1, 
            legend.position = c("left", "bottom"), 
            frame = FALSE)

# turns view map on
tmap_mode("view") #then Run script line 46

#urn tmap back to the plot view
tmap_mode("plot") #then Run script line 46

################################################################################
# create a ggplot object
ggplot() + 
  # add the shapefile as a layer
  geom_sf(data = world_wh, aes(fill = WH, color = GDP)) +
  # add a legend for the fill color
  scale_fill_gradient(low = "blue", high = "orange", name = "WH") +
  # add a legend for the color
  scale_color_gradient(low = "red", high = "green", name = "GDP") +
  # add title
  labs(title = "Bivariate Choropleth Map") +
  # set the map theme
  theme_bw()
