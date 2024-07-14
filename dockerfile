# Use the rocker/r-ver base image with R 4.4.1
FROM rocker/r-ver:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libjpeg-dev \
    libpng-dev \
    build-essential \
    cmake \
    libmagick++-dev

# Install R packages from CRAN
RUN install2.r plumber png jpeg magrittr base64enc ggplot2 remotes magick

# Install brickr from GitHub
RUN R -e "remotes::install_github('ryantimpe/brickr')"

# Copy plumber script
COPY lego.r /app/lego.r
COPY run_server.r /app/run_server.r

# Set the working directory
WORKDIR /app

# Expose the port
EXPOSE 8000

# Run the plumber API
CMD ["Rscript", "run_server.r"]
