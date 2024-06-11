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

###############
# DATA MINING #
###############

# Rename header
tab <- tab %>% setNames(c("Island", "Region", "Prefecture", "Cases", "Deaths"))

#delete multiple rows
tab <- tab[-c(1, 49, 50, 51, 52),]

# Update the value of Nagasaki
tab$Prefecture[43] <- "Nagasaki Prefecture"
tab$Cases[43] <- 1246

# Check the result
head(tab)
view(tab)

# Convert to numeric
tab$Cases <- as.numeric(tab$Cases)
tab$Deaths <- as.numeric(tab$Deaths)

# Create bar plot
ggplot(tab)+
  geom_col(aes(x = Prefecture, y = Cases, fill = factor(Region)))+
  theme(axis.text.x = element_text(size = 5, angle = 45, vjust = 0.5))+
  labs(title = "COVID-19 Cases by Prefecture",
       x = "Prefectures",
       y = "Cases")

# Create a new dataframe with cumulative cases by Region
tab_new <- tab %>%
  group_by(Region) %>%
  mutate(Total_Cases = cumsum(Cases))

# Plot in cumulative
ggplot(tab_new, aes(x = Island, y = Cases, fill = factor(Region))) +
  geom_col() +
  theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 0.5))+
  labs(title = "COVID-19 Cases by Region",
       x = "Region",
       y = "Cases")

# Save in CSV format
write.csv(tab,"D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 8/jpn_covid19.csv", row.names = FALSE)

###############################
# CREATE COVID-19 MAP OF JAPAN #
###############################

# Load needed library
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
colnames(japan)[c(1)] <- c("ID")

# Remove all spaces from the "ID" column
japan$ID <- str_trim(japan$ID)

# replace values in the dataframe
library(stringr)
rep_str = c('Hyōgo'='Hyogo','Kōchi'='Kochi','Ōita'='Oita')
japan$ID <- str_replace_all(japan$ID, rep_str)

# Remove "Prefecture" from each element
tab_new$Prefecture <- gsub("Prefecture", "", tab_new$Prefecture)
tab_new$Prefecture <- str_replace_all(tab_new$Prefecture, rep_str)

# Delete unnecessary columns except "Prefecture", "Cases"
tab_new <- tab_new[, c("Prefecture", "Cases")]

# Rename header
colnames(tab_new)[c(1)] <- c("ID")

# Remove all spaces from the "ID" column
tab_new$ID <- str_trim(tab_new$ID)

# Merge data to shapefile
merge_data <- left_join(japan, tab_new, by= "ID")
japan_covid <- merge_data

# Plot the map
library(OpenStreetMap)
library(tmaptools)

# Create bounding box for base map
osm_world <- read_osm(japan_covid)

# Plot using tmap
tm_shape(osm_world) + tm_rgb()+
tm_shape(japan_covid) + 
  tm_fill("Cases", palette = "YlOrRd", # you can add (-YlOrRd) to create opposite color
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
high_threshold <- 12000
medium_threshold <- 2000

# Create a new column "Classification" with initial value "Low" for all rows
japan_covid$Classification <- "Low"

# Reclassify the values based on the defined thresholds
japan_covid$Classification[japan_covid$Cases > high_threshold] <- "High"
japan_covid$Classification[japan_covid$Cases >= medium_threshold &
                             japan_covid$Cases <= high_threshold] <- "Medium"

# Plot reclass map using tmap
tm_shape(osm_world) + tm_rgb()+
  tm_style("natural", frame.lwd=5) +
  tm_shape(japan_covid) + 
  tm_fill("Classification", palette = "Spectral",
          style = "pretty", title = "Classification Cases") +
  tm_borders(alpha=.4) +
  tm_compass(type="4star", position=c("right", "top")) +
  tm_layout(main.title = "COVID-19 Pandemic in Japan",
            main.title.position = "center",
            main.title.color = "red",
            panel.labels = c("as of 2021/01/17"),
            panel.label.color = "black",
            legend.text.size = 0.7,
            legend.title.size = 1, 
            legend.position = c("right", "bottom"), 
            frame = T)+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_logo("https://web.dpipe.tsukuba.ac.jp/wp-content/uploads/sites/19/2018/05/UTLogo2.png",
          height = 1.5) +
  tm_credits("Data source: multiple sources", fontface = "italic", align = "right") +
  tm_credits("Author: Data Science Class", fontface = "bold", align = "right")+
  tm_legend(position=c("left", "top"), bg.color="grey80")

qtm(japan_covid, fill="Cases", projection=4326, inner.margins=0) +
  tm_grid(x = seq(-180, 180, by=10), y=seq(-90,90,by=10), col = "gray70") +
  tm_xlab("Longitude") +
  tm_ylab("Latitude")


# Using ggplot
ggplot(japan_covid, aes(fill = Cases)) +
  geom_sf()  +  
  scale_fill_viridis_c("Cases") +
  labs(title = "COVID-19 Pandemic in Japan", 
       x = "Longitude", y = "Latitude") +  # Customize labels
  theme_gray() +  # Set theme, you can choose theme_bw(), theme_linedraw(), theme_light(), theme_dark(), theme_minimal()
  theme(plot.title = element_text(hjust = 0.5),  # Center title
        plot.background = element_rect(fill = "white"))
