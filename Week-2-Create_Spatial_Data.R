# Install and load required library
install.packages("sf")
library(sf)

# create a data frame of x, y coordinates
df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))

# create a spatial object using sf
points <- st_as_sf(df, coords = c("x", "y"), crs = 4326)

# plot the points
plot(points)

# create a data frame of x, y coordinates for the line
df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))

# create a matrix of coordinates for the line
coords <- cbind(df$x, df$y)

# create a spatial object using sf
line <- st_linestring(coords)

# plot the line
plot(line)

# create a data frame of x, y coordinates for the polygon
df <- data.frame(x = c(0, 1, 1, 0, 0), y = c(0, 0, 1, 1, 0))

# create a matrix of coordinates for the polygon
coords <- cbind(df$x, df$y)

# create a spatial object using sf
poly <- st_polygon(list(coords))

# plot the polygon
plot(poly)
