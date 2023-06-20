install.packages("rvest")
library(rvest)
library(tidyverse)

# Set working directory
setwd("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 8")

# Web Scrapping
covid <- read_html("https://en.wikipedia.org/wiki/Statistics_of_the_COVID-19_pandemic_in_Japan")

# Checking the data
class(covid)
covid

# View inside data
html_text(covid)

# Using only table
tab <- covid %>% html_nodes("table")

# Check Table 1
tab[[1]] 

# Select only Table 1
tab <- tab[[1]] %>% html_table(header = TRUE, fill = TRUE)

# Check again the result
class(tab)

# Rename header
tab <- tab %>% setNames(c("Island", "Region", "Prefecture", "Cases", "Deaths"))

#delete 4th,5th and 1st rows
tab <- tab[-c(1,49, 50, 51, 52),]

head(tab)
view(tab)

# Convert to numeric
tab$Cases <- as.numeric(tab$Cases)
tab$Deaths <- as.numeric(tab$Deaths)

# Create bar plot
ggplot(tab)+
  geom_col(aes(x = Prefecture, y = Cases, fill = factor(Region)))+
  theme(axis.text.x = element_text(size = 5, angle = 45, vjust = 0.5))

# Create a new dataframe with cumulative cases by Region
tab_new <- tab %>%
  group_by(Region) %>%
  mutate(label_y = cumsum(Cases))

# Plot in cumulative
ggplot(tab_new, aes(x = Island, y = Cases, fill = factor(Region))) +
  geom_col() +
  theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 0.5))

# Save in CSV format
write.csv(tab,"D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 8/jpn_covid19.csv", row.names = FALSE)

################################################################################

# CREATE COVID-19 MAP OF JAPAN
library(sf)
library(dplyr)
library(tmap)

# read in the shapefile data
japan_admin <- st_read("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 7/jpn_adm_2019_shp/jpn_admbnda_adm1_2019.shp")

# Check if the shapefile has any invalid geometries
is_valid <- st_is_valid(japan_admin)

# If the shapefile has invalid geometries, fix them using st_make_valid()
if (!all(is_valid)) {
  japan_admin <- st_make_valid(japan_admin)
}

# Delete unnecessary columns except Prefecture's name
japan <- japan_admin %>% select(ADM1_EN)

# Rename header
colnames(japan)[c(1)] <- c("Prefecture")

# replace values in the dataframe
library(stringr)
rep_str = c('Hyōgo'='Hyogo','Kōchi'='Kochi','Ōita'='Oita')
japan$Prefecture <- str_replace_all(japan$Prefecture, rep_str)

# Remove "Prefecture" from each element
tab$Prefecture <- gsub("Prefecture", "", tab$Prefecture)
tab$Prefecture <- str_replace_all(tab$Prefecture, rep_str)

# joins data to the shapefile
# Merge data
merge_data_left <- left_join(japan, tab, by="Prefecture")
merge_data_right <- right_join(japan, tab, by="Prefecture")


merge_data_left$Cases[1:47] <- merge_data_right$Cases[1:47]
merge_data_left <- merge_data_left[, -c(2:3, 5)]

japan_covid <- merge_data_left

# Update the value of Toyama
japan_covid$Cases[43] <- 802


# Plot the map
library(OpenStreetMap)
library(tmaptools)

osm_world <- read_osm(japan_covid)

tm_shape(osm_world) + tm_rgb()+
  tm_shape(japan_covid) + 
  tm_fill("Cases", palette = "Reds",
          style = "pretty", title = "Number of Cases") +
  tm_borders(alpha=.4) +
  tm_compass(type="arrow", position=c("right", "top")) +
  tm_layout(main.title = "COVID-19 Pandemic in Japan",
            main.title.position = "center",
            main.title.color = "red", 
            legend.text.size = 0.7,
            legend.title.size = 1, 
            legend.position = c("right", "bottom"), 
            frame = T)
# Reclass
# Define the reclassification thresholds
high_threshold <- 10000
medium_threshold <- 500

# Create a new column "Classification" with initial value "Low" for all rows
japan_covid$Classification <- "Low"

# Reclassify the values based on the defined thresholds
japan_covid$Classification[japan_covid$Cases > high_threshold] <- "High"
japan_covid$Classification[japan_covid$Cases > medium_threshold &
                             japan_covid$Cases <= high_threshold] <- "Medium"

tm_shape(osm_world) + tm_rgb()+
  tm_shape(japan_covid) + 
  tm_fill("Classification", palette = "RdYlGn",
          style = "pretty", title = "Classification Cases") +
  tm_borders(alpha=.4) +
  tm_compass(type="arrow", position=c("right", "top")) +
  tm_layout(main.title = "COVID-19 Pandemic in Japan",
            main.title.position = "center",
            main.title.color = "red",
            panel.labels = c("as of 2021/01/17"),
            panel.label.color = "black",
            legend.text.size = 0.7,
            legend.title.size = 1, 
            legend.position = c("right", "bottom"), 
            frame = T)
