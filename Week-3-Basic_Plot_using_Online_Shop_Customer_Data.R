install.packages(readr)

library(readr)

OSCSD <- read_csv("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/Online Shop Customer Sales Data/Online Shop Customer Sales Data.csv")
View(OSCSD)

library(ggplot2)

# Create a dataframe with the necessary variables
data <- OSCSD
View(data)

#Plot Scatterplot
ggplot(data, aes(x = Age, y = Revenue_Total)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Age vs Revenue_Total",
       x = "Age",
       y = "Revenue_Total") +
  geom_text(aes(x = min(data$Age), y = max(data$Revenue_Total),
                label = paste("cor = ", round(cor(data$Age, data$Revenue_Total), 2))), hjust = 0)


# Plot a bubble chart with 'Age' and 'Revenue' as x- and y- axes, respectively, and 'Purchase' as the size of the bubbles
data$Pay_Method <- as.factor(data$Pay_Method) # convert 'cyl' column to a factor variable

ggplot(data, aes(x = Age, y = Revenue_Total, size = Purchase_VALUE, color = Pay_Method)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(2, 12)) + # adjust the range of bubble sizes
  labs(x = "Age", y = "Revenue_Total", title = "Bubble Plot of Online Shop Customer Sales Data")


# load required package
library(plotly)

# create bubble plot
plot_ly(data, x = ~Age, y = ~Time_Spent, color = ~Pay_Method, size = ~N_Purchases,
        type = "scatter", mode = "markers",
        hoverinfo = "text",
        text = paste("Gender: ", data$Gender,
                     "<br>Pay_Method: ", data$Pay_Method,
                     "<br>Time_Spent: ", data$Time_Spent)) %>%
  layout(title = "Age vs Gender vs Pay_Method vs Time_Spent",
         xaxis = list(title = "Age"),
         yaxis = list(title = "Time_Spent"))
###########################################################
library(tidyverse)
library(lubridate)

# calculate the percentage of pay method 
pay_pct <- data %>% 
  group_by(Pay_Method) %>% 
  summarize(count = n()) %>% 
  mutate(pct = count / sum(count) * 100)

# plot the percentage of pay method
ggplot(pay_pct, aes(x = factor(Pay_Method), y = pct, fill = factor(Pay_Method))) + 
  geom_bar(stat = "identity") + 
  labs(title = "Percentage of Pay Method")

# calculate the percentage of male and female customers
gender_pct <- data %>% 
  group_by(Gender) %>% 
  summarize(count = n()) %>% 
  mutate(pct = count / sum(count) * 100)

# plot the percentage of male and female customers
ggplot(gender_pct, aes(x = factor(Gender), y = pct, fill = factor(Gender))) + 
  geom_bar(stat = "identity") + 
  labs(title = "Percentage of Male and Female Customers")

# Distribution of Gender Across All Age Customers
ggplot(data, aes(x=Age, fill=factor(Gender))) + 
  geom_histogram(binwidth = 5) + 
  labs(title="Distribution of Gender Across All Age Customers")

# Distribution of Browser Across All Age Customers
ggplot(data, aes(x=Age, fill=factor(Browser))) + 
  geom_histogram(binwidth = 5) + 
  labs(title="Distribution of Browser Across All Age Customers")

# calculate the total revenue by pay method
revenue_pay <- data %>% 
  group_by(Pay_Method) %>% 
  summarize(total_revenue = sum(Revenue_Total))

# plot the total revenue by pay method
ggplot(revenue_pay, aes(x = factor(Pay_Method), y = total_revenue, fill = factor(Pay_Method))) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total Revenue by Pay Method")

# calculate the total revenue by gender
revenue_gender <- data %>% 
  group_by(Gender) %>% 
  summarize(total_revenue = sum(Revenue_Total))

# plot the total revenue by gender
ggplot(revenue_gender, aes(x = factor(Gender), y = total_revenue, fill = factor(Gender))) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total Revenue by Gender")

# calculate the revenue distribution among age groups
revenue_age <- data %>% 
  group_by(Age) %>% 
  summarize(total_revenue = sum(Revenue_Total)) %>% 
  mutate(age_group = cut(Age, breaks = seq(0, 100, by = 10), include.lowest = TRUE))

# plot the revenue distribution among age groups
ggplot(revenue_age, aes(x = age_group, y = total_revenue, fill = age_group)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Revenue Distribution Among Age Groups")

# calculate the total purchase distribution among age groups
purchase_age <- data %>% 
  group_by(Age) %>% 
  summarize(total_purchases = sum(N_Purchases)) %>% 
  mutate(age_group = cut(Age, breaks = seq(0, 100, by = 10), include.lowest = TRUE))

# plot the total purchase distribution among age groups
ggplot(purchase_age, aes(x = age_group, y = total_purchases, fill = age_group)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total Purchase Distribution Among Age Groups")

# Total Revenue by Months
data$Purchase_DATE <- as.Date(data$Purchase_DATE, format="%d.%m.%y")
data$Month <- format(data$Purchase_DATE, "%m")
data %>%
  group_by(Month) %>%
  summarize(total_revenue = sum(Revenue_Total))

# plot the total revenue by months
total_month <- data %>% 
  group_by(Month) %>% 
  summarize(count = n()) %>% 
  mutate(sum = count)

ggplot(total_month, aes(x = factor(Month), y = sum, fill = factor(Month))) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total Revenue by Month")


# Total Revenue by Year
data$Year <- format(data$Purchase_DATE, "%Y")
data %>%
  group_by(Year) %>%
  summarize(total_revenue = sum(Revenue_Total))

# plot the total revenue by Year
total_year <- data %>% 
  group_by(Year) %>% 
  summarize(count = n()) %>% 
  mutate(sum = count)

ggplot(total_year, aes(x = factor(Year), y = sum, fill = factor(Year))) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total Revenue by Year")
