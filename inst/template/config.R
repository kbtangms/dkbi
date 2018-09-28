library(curl)

# Databases Declaration ---------------------------------------------------

# init
db <- new.env()
#db$forecast <- "Forecast"


# API Declaration ---------------------------------------------------------

# init
api <- new.env()
#api$pricing <- "some_pricing_api"


# Consul Configuration ----------------------------------------------------

# init
consul <- new.env()

# default read from .Renviron. If null, switch to machine hostname
if (is.null(nslookup(Sys.getenv("consul.host"), error = FALSE))) {
    consul$host <- Sys.info()[["nodename"]]

} else {
    consul$host <- Sys.getenv("consul.host")

}

# not changing port and swagger from .Renviron
consul$port = Sys.getenv("consul.port")
consul$swagger = Sys.getenv("consul.swagger")

# Token Validation If Needed ----------------------------------------------

# function as a service
validate_token <- function(token) {
    # return true/false
    token == Sys.getenv("common.secret")

}
