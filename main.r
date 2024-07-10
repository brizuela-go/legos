library(brickr)
library(png)
library(jpeg)

# Function to read the image based on its file extension
read_image <- function(file_path) {
  file_ext <- tools::file_ext(file_path)
  if (file_ext == "jpg" || file_ext == "jpeg") {
    return(jpeg::readJPEG(file_path))
  } else if (file_ext == "png") {
    return(png::readPNG(file_path))
  } else {
    stop("Unsupported image format")
  }
}

demo_img <- tempfile(fileext = ".jpg") # Ensure the correct file extension is used
download.file("https://i.natgeofe.com/n/4f5aaece-3300-41a4-b2a8-ed2708a0a27c/domestic-dog_thumb_square.jpg", demo_img, mode = "wb")

# Read the image and get its dimensions
img <- read_image(demo_img)
img_width <- dim(img)[2]
img_height <- dim(img)[1]

# Determine if the image is square or rectangular
if (img_width == img_height) {
  img_size <- 36 # Square image
  cat("Square image\n")
} else {
  # Calculate img_size based on the aspect ratio
  cat("Rectangular image\n")
  if (img_width > img_height) {
    img_size <- c(36, round(36 * (img_height / img_width)))
  } else {
    img_size <- c(round(36 * (img_width / img_height)), 36)
  }
}



# Generate mosaic
mosaic <- image_to_mosaic(img, img_size = img_size, use_bricks = c("1x1"))

# Save each generated mosaic
mosaic %>% build_mosaic()

# Build pieces table
build_pieces_table(mosaic)
