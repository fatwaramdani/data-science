library(readr)
library(tidyr)
GDP <- read_csv("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/GDP1999-2022/GDP by Country 1999-2022.csv")
View(GDP)

# Subset data for a single country
japan_gdp <- subset(GDP, Country == "Japan")

# Reshape the data into a longer format
japan_gdp <- pivot_longer(japan_gdp, cols = -Country, names_to = "year", values_to = "value")
japan_gdp$year <- as.numeric(as.character(japan_gdp$year))

# Create a ggplot with a line graph
ggplot(japan_gdp, aes(x = year, y = value, color = Country)) +
  geom_line() +
  labs(x = "Year", y = "GDP", color = "Country")+
  ggtitle("GDP Growth of Japan")


# subset multiple country
library(dplyr)
multi_country <- GDP %>% filter(Country %in% c("Japan", "India", "China"))

# Reshape the data into a longer format
df_long <- pivot_longer(multi_country, cols = -Country, names_to = "year", values_to = "value")
df_long$year <- as.numeric(as.character(df_long$year))

# Create a ggplot with a line graph
ggplot(df_long, aes(x = year, y = value, color = Country)) +
  geom_line() +
  labs(x = "Year", y = "GDP", color = "Country")+
  ggtitle("GDP Growth of Japan, India, and China")

#The lowest GDP growth by country
library(ggplot2)
lowest_gdp <- GDP %>%
  gather(year, gdp, -Country) %>%
  group_by(Country) %>%
  summarize(lowest_gdp = min(diff(gdp)))#%>%
#slice_head(n = 20)


ggplot(lowest_gdp, aes(x = Country, y = lowest_gdp)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Lowest GDP Growth by Country", x = "Country", y = "GDP Growth")+
  theme(axis.text.x = element_text(size = 6, angle = 45))

#The highest GDP growth by country
highest_gdp <- GDP %>%
  gather(year, gdp, -Country) %>%
  group_by(Country) %>%
  summarize(highest_gdp = max(diff(gdp)))#%>%
#slice_head(n = 20)

ggplot(highest_gdp, aes(x = Country, y = highest_gdp)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Highest GDP Growth by Country", x = "Country", y = "GDP Growth")+
  theme(axis.text.x = element_text(size = 6, angle = 45))

#The average GDP growth comparison
avg_gdp <- GDP %>%
  gather(year, gdp, -Country) %>%
  group_by(Country) %>%
  summarize(avg_gdp = mean(diff(gdp)))%>%
  slice_head(n = 20)

ggplot(avg_gdp, aes(x = Country, y = avg_gdp)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_hline(yintercept = mean(avg_gdp$avg_gdp), color = "red") +
  labs(title = "Average GDP Growth Comparison", x = "Country", y = "GDP Growth")+
  theme(axis.text.x = element_text(size = 6, angle = 45))


# Get the mean GDP growth rate for each country
gdp_mean <- data.frame(country = GDP$Country, mean_growth = rowMeans(GDP[, -1], na.rm = TRUE))

# Sort by GDP growth rate
gdp_mean_sorted <- gdp_mean[order(gdp_mean$mean_growth),]

# Get the top ten lowest GDP growth rate countries
top_10_low_gdp <- gdp_mean_sorted[1:10,]

# Plot the data
ggplot(top_10_low_gdp, aes(x = country, y = mean_growth)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  xlab("Country") + ylab("GDP Growth Rate") +
  ggtitle("Top 10 Countries with Lowest GDP Growth Rate")+
  theme(axis.text.x = element_text(size = 8, angle = -45))

# Sort by GDP growth rate
gdp_mean_sorted <- gdp_mean[order(gdp_mean$mean_growth, decreasing = TRUE),]

# Get the top ten highest GDP growth rate countries
top_10_high_gdp <- gdp_mean_sorted[1:10,]

# Plot the data
ggplot(top_10_high_gdp, aes(x = country, y = mean_growth)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  xlab("Country") + ylab("GDP Growth Rate") +
  ggtitle("Top 10 Countries with Highest GDP Growth Rate")+
  geom_hline(yintercept = mean(gdp_mean$mean_growth), color = "red", linetype = "dashed")

# Get the mean GDP growth rate for each country
gdp_mean <- data.frame(country = GDP$Country, mean_growth = rowMeans(GDP[, -1], na.rm = TRUE))

# Sort by GDP growth rate
gdp_mean_sorted <- gdp_mean[order(gdp_mean$mean_growth, decreasing = TRUE),]

# Get the top ten highest GDP growth rate countries
top_10_high_gdp <- gdp_mean_sorted[1:10,]

# Plot the data
ggplot(gdp_mean, aes(x = country, y = mean_growth)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  xlab("Country") + ylab("GDP Growth Rate") +
  ggtitle("Average GDP Growth Rate by Country") +
  geom_hline(yintercept = mean(gdp_mean$mean_growth), color = "red", linetype = "dashed") +
  geom_text(data = top_10_high_gdp, aes(label = country), vjust = -0.2, angle = 10)+
  theme(axis.text.x = element_text(size = 2, angle = 45))

#############################################################
#Combining multiple plots
library(patchwork)
(ggplot(japan_gdp, aes(x = year, y = value, color = Country)) +
    geom_line() +
    labs(x = "Year", y = "GDP", color = "Country")+
    ggtitle("GDP Growth of Japan")+
    ggplot(df_long, aes(x = year, y = value, color = Country)) +
    geom_line() +
    labs(x = "Year", y = "GDP", color = "Country")+
    ggtitle("GDP Growth of Japan, India, and China")+
    ggplot(top_10_low_gdp, aes(x = country, y = mean_growth)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    xlab("Country") + ylab("GDP Growth Rate") +
    ggtitle("Top 10 Countries with Lowest GDP Growth Rate")+
    theme(axis.text.x = element_text(size = 8, angle = -45))+
    ggplot(top_10_high_gdp, aes(x = country, y = mean_growth)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    xlab("Country") + ylab("GDP Growth Rate") +
    ggtitle("Top 10 Countries with Highest GDP Growth Rate")+
    geom_hline(yintercept = mean(gdp_mean$mean_growth), color = "red", linetype = "dashed"))+
  theme(axis.text.x = element_text(size = 8, angle = 45))
