library(plumber)
library(brickr)
library(png)
library(jpeg)
library(magrittr)
library(ggplot2)
library(base64enc)
library(tools)

# Function to read the image from a URL
read_image_from_url <- function(url, ext) {
  temp_file <- tempfile()
  download.file(url, temp_file, mode = "wb")

  if (ext %in% c("jpg", "jpeg")) {
    return(jpeg::readJPEG(temp_file))
  } else if (ext == "png") {
    return(png::readPNG(temp_file))
  } else {
    stop("Unsupported image format")
  }
}

#* @post /upload
#* @param image_url: string
#* @param panel_size: string
#* @param image_ext: string
#* @response 200 Returns the LEGO mosaic pieces table and image in base64
function(image_url, panel_size, image_ext) {
  # Debug message for received parameters
  print(paste("Received image_url:", image_url))
  print(paste("Received panel_size:", panel_size))
  print(paste("Received image_ext:", image_ext))

  # Read the image from URL
  img <- tryCatch(
    {
      read_image_from_url(image_url, tolower(image_ext))
    },
    error = function(e) {
      stop("Failed to read image: ", e$message)
    }
  )

  # Debug message for image dimensions
  img_width <- dim(img)[2]
  img_height <- dim(img)[1]
  print(paste("Image dimensions - Width:", img_width, "Height:", img_height))

  # Determine if the image is square or rectangular
  if (img_width == img_height) {
    img_size <- 36 * as.integer(panel_size)
  } else {
    # Calculate img_size based on the aspect ratio
    if (img_width > img_height) {
      img_size <- c(36 * as.integer(panel_size), round((36 * as.integer(panel_size)) * (img_height / img_width)))
    } else {
      img_size <- c(round((36 * as.integer(panel_size)) * (img_width / img_height)), 36 * as.integer(panel_size))
    }
  }

  # Print debug info for img_size
  print(paste("Generated img_size:", img_size))

  # Generate mosaic
  mosaic <- tryCatch(
    {
      image_to_mosaic(img, img_size = img_size, use_bricks = c("1x1"))
    },
    error = function(e) {
      stop("Failed to generate mosaic: ", e$message)
    }
  )

  # Save the generated mosaic image to a file
  mosaic_file <- tempfile(fileext = ".png")
  ggplot_mosaic <- mosaic %>% build_mosaic()
  ggsave(mosaic_file, ggplot_mosaic)

  instructions_file <- tempfile(fileext = ".png")
  ggplot_instructions <- mosaic %>% build_instructions()
  ggsave(instructions_file, ggplot_instructions)


  # Encode the mosaic image to base64
  mosaic_base64 <- base64enc::base64encode(mosaic_file)

  # Encode the instructions image to base64
  instructions_base64 <- base64enc::base64encode(instructions_file)



  # Build pieces table
  pieces_table <- tryCatch(
    {
      build_pieces_table(mosaic)
    },
    error = function(e) {
      stop("Failed to build pieces table: ", e$message)
    }
  )

  # Return the pieces table and mosaic image in base64
  list(
    pieces_table = pieces_table,
    mosaic_image_base64 = mosaic_base64,
    instructions_image_base64 = instructions_base64
  )
}

# Enable CORS
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    forward()
  }
}
