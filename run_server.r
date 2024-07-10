# run_server.R
library(plumber)

# Load the plumber API script
r <- plumb("plumber.R")

# Run the server on port 8000
r$run(host = "0.0.0.0", port = 8000)
