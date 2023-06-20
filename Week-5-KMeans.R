library(ggplot2)
library(dplyr)
library(readr)

AWH <- read_csv("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/Working hours/annual-working-hours-vs-gdp-per-capita-pwt.csv")
View(AWH)

#Rename only the 1st, 4th to 6th column names
colnames(AWH)[c(1, 4,5,6)] <- c("Country", "WH", "GDP", "POP")

#Delete NA cell
selected_data <- na.omit(AWH)

#k-means clustering of GDP growth
library(cluster) # for clustering analysis

# Standardize mean_growth column
selected_data$GDP <- scale(selected_data$GDP)

# Perform k-means clustering with 4 clusters
set.seed(123)
kmeans_result <- kmeans(selected_data$GDP, centers = 4)

# Add cluster labels to original data
selected_data$cluster <- kmeans_result$cluster

# Plot the clusters using ggplot
ggplot(selected_data, aes(x = Country, y = GDP, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "GDP", fill = "Cluster") +
  theme(axis.text.x = element_text(size = 5, angle = 45))

#Plot cluster by continent
ggplot(selected_data, aes(y = cluster)) +
  geom_bar(aes(fill = Continent)) +
  ggtitle("Count of Clusters by Continent") +
  theme(plot.title = element_text(hjust = 0.5))


#k-means clustering of WH
# Standardize WH" column
selected_data$WH <- scale(selected_data$WH)

# Perform k-means clustering with 4 clusters
set.seed(123)
kmeans_result <- kmeans(selected_data$WH, centers = 4)

# Add cluster labels to original data
selected_data$cluster <- kmeans_result$cluster

# Add cluster labels to the original data frame
selected_data$cluster <- as.factor(kmeans_result$cluster)

# Create a scatterplot using ggplot
ggplot(selected_data, aes(x=GDP, y=WH, color=cluster)) + 
  geom_point() + 
  ggtitle("Clustering Analysis") +
  geom_text(aes(label=Country), color= "black", size=2, check_overlap = TRUE)+
  xlab("GDP") + 
  ylab("WH")



#Create boundary
install.packages("factoextra")
library(factoextra)

#Define the optimum no of cluster
fviz_nbclust(scale(selected_data[,4:6]), kmeans, nstart=100, method = "wss") + 
  geom_vline(xintercept = 5, linetype = 1)

# Fancy kmeans
kmeans_fancy <- kmeans(scale(selected_data[,4:6]), 5, nstart = 100)
# plot the clusters
fviz_cluster(kmeans_fancy, data = scale(selected_data[,4:6]), geom = c("point"),ellipse.type = "euclid")


