# run_server.R
library(plumber)


options(
    plumber_limits = list(
        post_size_mb = 10 # Adjust size limit as needed (in MB)
    )
)


# Load the plumber API script
r <- plumb("lego.r")

# Run the server on port 8000
r$run(host = "0.0.0.0", port = 8000)
