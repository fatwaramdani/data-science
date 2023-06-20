# Set working directory
setwd("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 7/jpn_adm_2019_shp")

# Install and load required packages
install.packages(c("sf", "tmap", "leaflet")) # do it if not installed yet
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
library(leaflet)

# 1. DATA PREPARATION
# Import XL
library(readxl)
japan_pop <- read_excel("y700203000.xlsx")
View(japan_pop)

#Create new dataframe consist only Prefecture's name, Pop2010, and Pop2019
japan_pop <- japan_pop[,c(3, 28,34)]

#Delete NA cell
japan_data <- na.omit(japan_pop)

#Rename column names
colnames(japan_data)[c(1,2,3)] <- c("ID", "POP10", "POP19")

# Convert to numeric
japan_data$POP10 <- as.numeric(japan_data$POP10)
japan_data$POP19 <- as.numeric(japan_data$POP19)

#Calculate the Population changed
japan_data$POPULATION_CHANGE <- japan_data$POP19 - japan_data$POP10

# delete the row with value "Japan" in column "ID"
japan_data <- subset(japan_data, ID != "Japan")
View(japan_data)

# save data to a CSV file
write.csv(japan_data, "japandata.csv", row.names = FALSE)

# read in the shapefile data
japan_admin <- st_read("jpn_admbnda_adm1_2019.shp")

# Check if the shapefile has any invalid geometries
is_valid <- st_is_valid(japan_admin)

# If the shapefile has invalid geometries, fix them using st_make_valid()
if (!all(is_valid)) {
  japan_admin <- st_make_valid(japan_admin)
}

# Delete unnecessary columns except Prefecture's name
japan <- japan_admin %>% select(ADM1_EN)

# Rename header
colnames(japan)[c(1)] <- c("ID")

# joins data to the shapefile
# replace values in the dataframe
library(stringr)
rep_str = c('Hyōgo'='Hyogo','Kōchi'='Kochi','Ōita'='Oita')
japan$ID <- str_replace_all(japan$ID, rep_str)

# Merge data
merge_data_left <- left_join(japan, japan_data, by= "ID")
merge_data_right <- right_join(japan, japan_data, by= "ID")

# joins data to the shapefile
merge_data_left$POP19[1:47] <- merge_data_right$POP19[1:47]
merge_data_left$POP10[1:47] <- merge_data_right$POP10[1:47]
merge_data_left$POPULATION_CHANGE[1:47] <- merge_data_right$POPULATION_CHANGE[1:47]

merge_data <- merge_data_left

# 2. DATA VISUALIZATION
#Creating quick map
tm_shape(merge_data) + 
  tm_polygons("POP19")+
  tmap_options(check.and.fix = TRUE, max.categories = 47)

#Creating quick map
tm_shape(merge_data) + 
  tm_polygons("POP10")+
  tmap_options(check.and.fix = TRUE, max.categories = 47)

#Creating quick map
tm_shape(merge_data) + 
  tm_polygons("POPULATION_CHANGE")+
  tmap_options(check.and.fix = TRUE, max.categories = 47)

# turns view map on
tmap_mode("view")

# adds in layout, gets rid of frame
tm_shape(merge_data) + 
  tm_fill("POPULATION_CHANGE", palette = "RdBu",
          style = "pretty", title = "Population Changes") +
  tm_borders(alpha=.4) +
  tm_compass(type="arrow", position=c("right", "top")) +
  tm_layout(main.title = "Japan Population Changes (2010 to 2019)",
            main.title.position = "center",
            main.title.color = "blue", 
            legend.text.size = 0.7,
            legend.title.size = 1, 
            legend.position = c("right", "bottom"), 
            frame = FALSE)

# turn tmap back to the plot view
tmap_mode("plot")

# includes a histogram in the legend
tm_shape(merge_data) + 
  tm_fill("POPULATION_CHANGE", style = "pretty", n = 5,
          palette = "RdBu", title = "Population Changes", legend.hist = TRUE)+
  tm_layout(main.title = "Japan Population Changed (2010 to 2019)",
            main.title.position = "center",
            main.title.color = "blue")+
  tm_borders(alpha=.4)+
  tm_compass(type="rose", position=c("right", "top"), size = 2) 


# add scale bar and change the compass style
tm_shape(merge_data) + 
  tm_fill("POPULATION_CHANGE", style = "pretty", n = 5,
          palette = "RdBu", title = "Population Changes")+
  tm_layout(main.title = "Japan Population Changed (2010 to 2019)",
            main.title.position = "center",
            main.title.color = "blue")+
  tm_borders(alpha=.4)+
  tm_compass(type="4star", position=c("right", "top"), size = 2)+
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "white",
    lwd = 1)

