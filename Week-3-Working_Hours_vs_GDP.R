# Check and set working directory
getwd()
setwd("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/Working hours")


library(ggplot2)
library(dplyr)
library(readr)

AWH <- read_csv("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/Working hours/annual-working-hours-vs-gdp-per-capita-pwt.csv")
View(AWH)

#Rename only the 1st, 4th to 7th column names
colnames(AWH)[c(1, 4, 5, 6, 7)] <- c("Country", "WH", "GDP", "POP", "Continent")
View(AWH)


# Create Descriptive Summary Statistic
# Example 1
summary(AWH)

# Example 2
# Calculate summary statistics
summary_stats <- data.frame(
  Mean = sapply(AWH[, c("WH", "GDP", "POP")], mean),
  Median = sapply(AWH[, c("WH", "GDP", "POP")], median),
  SD = sapply(AWH[, c("WH", "GDP", "POP")], sd),
  Min = sapply(AWH[, c("WH", "GDP", "POP")], min),
  Max = sapply(AWH[, c("WH", "GDP", "POP")], max)
)
# Print the summary statistics table
print(summary_stats)

# Example 3
install.packages("skimr")
library(skimr)
skim(AWH)

# Example 4
install.packages(c("flextable", "janitor"))
library(flextable)
library(janitor)


# #Remove NA values (not used since any rows with NA values will be removed)
# AWH <- na.omit(AWH)
# View(AWH)
# 
# # Subset single data
# df_japan <- subset(AWH, Country == "Japan")
# View(df_japan)
# 
# #Delete columns
# df_japan <- df_japan[,-c(4,5,7)]
# 
# ggplot(df_japan, aes(x = Year, y = POP, color = Country)) +
#   geom_line() +
#   labs(x = "Year", y = "Population", color = "Country")
# 
# # subset multiple country
# library(dplyr)
# multi_country <- AWH %>% filter(Country %in% c("Japan", "United States"))
# multi_country <- multi_country[,-c(4,5,7)]
# 
# # Create a ggplot with a line graph
# ggplot(multi_country, aes(x = Year, y = POP, color = Country)) +
#   geom_line() +
#   labs(x = "Year", y = "Population", color = "Country")
# 
# # Get the mean Pop growth rate for each country
# AWH <- AWH[,-c(4,5,7)]
# AWH <- na.omit(AWH)
# pop_mean <- data.frame(country = AWH$Country, mean_growth = rowMeans(AWH[, -1], na.rm = TRUE))

# Step 1: Install countrycode if not installed
if (!require(countrycode)) install.packages("countrycode")
library(countrycode)

# Step 2: Fill in the empty 'World regions according to OWID' column
AWH$`Continent` <- ifelse(
  is.na(AWH$`Continent`) | AWH$`Continent` == "",
  countrycode(AWH$Country, origin = "country.name", destination = "continent"),
  AWH$`Continent`
)

# Create the 1st bubble plot
ggplot(AWH, aes(x = GDP, y = WH, size = POP, color = Continent)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF")) +
  scale_size(range = c(0.5, 6)) +
  labs(title = "Annual Working Hours, GDP, and Population by Continent", 
       x = "GDP (Millions of US Dollars)", 
       y = "Annual Working Hours per Worker", 
       size = "Population (Millions)", 
       color = "Continent") +
  theme_classic()

#Create the 2nd bubble plot
# group the data by continent and select the top 20 countries by GDP
big_gdp <- AWH %>%
  #filter(Year == 2019) %>%
  group_by(Continent) %>%
  arrange(desc(GDP)) %>%
  slice_head(n = 20)

# group the data by continent and select the bottom 20 countries by GDP
low_gdp <- AWH %>%
  #filter(Year == 2019) %>%
  group_by(Continent) %>%
  arrange(GDP) %>%
  slice_head(n = 20)

# group the data by continent and select the top 20 countries by working hours
long_working_hours <- AWH %>%
  #filter(Year == 2019) %>%
  group_by(Continent) %>%
  arrange(desc(WH)) %>%
  slice_head(n = 20)

# group the data by continent and select the bottom 20 countries by working hours
short_working_hours <- AWH %>%
  #filter(Year == 2019) %>%
  group_by(Continent) %>%
  arrange(WH) %>%
  slice_head(n = 20)

# combine the selected data into a single data frame
selected_data <- bind_rows(big_gdp, low_gdp, long_working_hours, short_working_hours)
selected_data <- na.omit(selected_data)
# remove duplicate rows
#selected_data <- selected_data[!duplicated(selected_data), ]

# create the bubble plot
library(ggpmisc)
ggplot(data=selected_data, aes(x=GDP, y=WH, size=POP, color=Continent)) +
  #stat_poly_line() +
  #stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n")))+
  geom_smooth(method = lm, col = "#141F52", fill = "grey72", size = 1.5,
              fullrange = TRUE)+
  geom_point(alpha=0.7) +
  scale_size(range=c(5, 15), breaks=c(0.5e7, 1e7, 2e7, 5e7, 1e8, 2e8, 5e8), labels=c("5", "10M", "20M", "50M", "100M", "200M", "500M")) +
  scale_x_continuous(limits=c(0, 1e+05))+
  geom_text(aes(label=Country), color= "black", size=3, check_overlap = TRUE, nudge_x = 0.2) +
  theme_minimal() +
  labs(
    title = "Workers in poorer countries tend to work more",
    subtitle = "Average working hours are 40 % higher in Cambodia than in the U.S",
    caption = "Source: Our World in Data.
    (https://ourworldindata.org/grapher/annual-working-hours-vs-gdp-per-capita-pwt)
    Visualization: Data Science Class",
    x = "GDP per capita (USD)",
    y = "Average annual working hours per worker",
    fill = NULL
  )+
  scale_color_manual(values=c("#F8766D", "#00BFC4", "#FFD700", "#E76BF3", "#00FF00", "#F0E442", "#A65628")) # customize the colors

#Create histogram
ggplot(selected_data, aes(x = WH, color = Continent)) +
  geom_histogram(fill="white", position="dodge")

#Create boxplot
ggplot(selected_data, aes(x = Continent, y = WH, fill = Continent)) + 
  geom_boxplot() +
  coord_flip()

