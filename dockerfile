# Use the rocker/r-ver base image
FROM rocker/r-ver:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libjpeg-dev \
    libpng-dev

# Install R packages
RUN R -e "install.packages(c('plumber', 'brickr', 'png', 'jpeg', 'magrittr', 'base64enc', 'ggplot2'), repos='http://cran.rstudio.com/')"

# Copy plumber script
COPY plumber.R /app/plumber.R
COPY run_server.R /app/run_server.R

# Set the working directory
WORKDIR /app

# Expose the port
EXPOSE 8000

# Run the plumber API
CMD ["Rscript", "run_server.R"]
