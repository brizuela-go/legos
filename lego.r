# plumber.R
library(plumber)
library(brickr)
library(png)
library(jpeg)
library(magrittr)
library(base64enc)
library(ggplot2)

#* @apiTitle LEGO Mosaic API

# Function to read the image based on its base64 string and file extension
read_image_from_base64 <- function(base64_str, file_ext) {
  temp_file <- tempfile(fileext = paste0(".", file_ext))
  writeBin(base64decode(base64_str), temp_file)
  if (file_ext == "jpg" || file_ext == "jpeg") {
    return(jpeg::readJPEG(temp_file))
  } else if (file_ext == "png") {
    return(png::readPNG(temp_file))
  } else {
    stop("Unsupported image format")
  }
}

#* @post /upload
#* @param image_base64: string
#* @param file_ext: string
#* @response 200 Returns the LEGO mosaic pieces table and image in base64
function(image_base64, file_ext) {
  # Read the image from base64 string
  img <- read_image_from_base64(image_base64, file_ext)
  img_width <- dim(img)[2]
  img_height <- dim(img)[1]

  # Determine if the image is square or rectangular
  if (img_width == img_height) {
    img_size <- 36 # Square image
  } else {
    # Calculate img_size based on the aspect ratio
    if (img_width > img_height) {
      img_size <- c(36, round(36 * (img_height / img_width)))
    } else {
      img_size <- c(round(36 * (img_width / img_height)), 36)
    }
  }

  # Generate mosaic
  mosaic <- image_to_mosaic(img, img_size = img_size, use_bricks = c("1x1"))

  # Save the generated mosaic image to a file
  mosaic_file <- tempfile(fileext = ".png")
  ggplot_mosaic <- mosaic %>% build_mosaic()
  ggsave(mosaic_file, ggplot_mosaic)

  # Encode the mosaic image to base64
  mosaic_base64 <- base64encode(mosaic_file)

  # Build pieces table
  pieces_table <- build_pieces_table(mosaic)

  # Return the pieces table and mosaic image in base64
  list(
    pieces_table = pieces_table,
    mosaic_image_base64 = mosaic_base64
  )
}
