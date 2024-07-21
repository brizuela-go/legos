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
#* @param panels: string
#* @param image_ext: string
#* @param panel_size: string
#* @param aspect_ratio: string
#* @response 200 Returns the LEGO mosaic pieces table and image in base64
function(image_url, panels, image_ext, panel_size, aspect_ratio) {
  # Debug message for received parameters
  print(paste("Received image_url:", image_url))
  print(paste("Received panels:", panels))
  print(paste("Received image_ext:", image_ext))
  print(paste("Received panel_size:", panel_size))
  print(paste("Received aspect_ratio:", aspect_ratio))

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

  # Calculate the total number of bricks
  panel_size <- as.integer(panel_size)
  panels <- as.integer(panels)
  total_bricks <- panels * (panel_size^2)

  # Parse the aspect ratio
  aspect_ratio_split <- strsplit(aspect_ratio, "/")[[1]]
  aspect_ratio_width <- as.numeric(aspect_ratio_split[1])
  aspect_ratio_height <- as.numeric(aspect_ratio_split[2])

  # Calculate the root square of the total number of bricks
  root_bricks <- sqrt(total_bricks)

  # Adjust image size based on the aspect ratio
  img_size <- if (aspect_ratio_width >= aspect_ratio_height) {
    c(root_bricks, round(root_bricks * (aspect_ratio_height / aspect_ratio_width)))
  } else {
    c(round(root_bricks * (aspect_ratio_width / aspect_ratio_height)), root_bricks)
  }

  # Print debug info for img_size
  print(paste("Generated img_size:", paste(img_size, collapse = "x")))

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

  # Add panel lines to the mosaic plot if panels > 1
  if (panels > 1) {
    panel_width <- img_size[1] / sqrt(panels)
    panel_height <- img_size[2] / sqrt(panels)

    # Calculate the range for x and y lines, ensuring they extend to the edges
    x_lines <- seq(0, img_size[1], by = panel_width)
    y_lines <- seq(0, img_size[2], by = panel_height)

    ggplot_mosaic <- ggplot_mosaic +
      geom_hline(yintercept = y_lines, color = "white") +
      geom_vline(xintercept = x_lines, color = "white")
  }

  ggsave(mosaic_file, ggplot_mosaic)

  # Generate and save instructions image
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
