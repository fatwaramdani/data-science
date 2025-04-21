# Load required packages
library(imager)

# Load image
image <- load.image("image.jpg")

# Display original image
plot(image)

# Convert image to grayscale
gray_image <- grayscale(image)

# Display grayscale image
plot(gray_image)

# Invert grayscale image
inverted_gray_image <- 1 - gray_image

# Display inverted grayscale image
plot(inverted_gray_image)

# Save inverted grayscale image
save.image(inverted_gray_image, "inverted_gray_image.jpg")

# Plot the original and mirrored images
par(mfrow = c(1, 2))
plot(image, main = "Original Image")
plot(inverted_gray_image, main = "Inverted Grayscale Image")




