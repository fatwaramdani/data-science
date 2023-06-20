library(ggplot2)
library(ggridges)

view(iris)

# Color by quantiles
p <- ggplot(iris, aes(x = Petal.Length, y = Species, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles")

# Highlight the tails of the distributions
q <- ggplot(iris, aes(x = Petal.Length, y = Species, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975)) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))

# Colour density
r <- ggplot(iris, aes(x = Petal.Length, y = Species, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)

# Styling jittered points
s <- ggplot(iris, aes(x = Petal.Length, y = Species, fill = Species)) +
  geom_density_ridges(
    aes(point_color = Species, point_fill = Species, point_shape = Species),
    alpha = .2, point_alpha = 1, jittered_points = TRUE) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))

#############################################################
#Combining multiple plots
library(patchwork)
p + q + r + s
p / r / q / s

# Save combined chart as PNG 
ggsave("probability_density_iris.png", width = 14, height = 6)



