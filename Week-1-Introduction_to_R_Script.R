attach(mtcars)
head(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
install.packages("ggplot2")
library(ggplot2)
scatter_plot <- ggplot(mtcars, aes(x = wt, y = mpg, color = "#f7aa58")) +
  geom_point() +  # Add points to the plot
  labs(x = "Weight", y = "MPG", title = "Scatterplot of MPG vs Weight") +  # Add axis labels and plot title
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = 'none') 
print(scatter_plot)

