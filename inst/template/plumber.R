library(plumber)

# replace filename
r <- plumb("main.R")

# replace host and port
r$run(host = "0.0.0.0", port = 8899)
