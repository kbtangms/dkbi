# Read from Consul --------------------------------------------------------

#' Obtain key / value from Consul
#' @description See https://www.consul.io/ for more about Consul.
#'
#' @param key Key name to retrieve
#' @param path Path name to retrieve
#' @param ... One or many keys under path to retrieve.
#' All but path name will be coersed into a list.
#'
#' @return
#' Decoded value retrieved from Consul. 'NA' if empty.
#'
#' @importFrom magrittr %>%
#' @importFrom httr GET content
#' @importFrom base64enc base64decode
#' @export
#' @rdname get_kv
#'
#' @examples
#' \dontrun{
#'
#' # Return a string
#' get_kv("fruit/apple")
#'
#' # Return a list of values
#' get_batch_kv("fruit", c("apple", "banana", "coconut"))
#' # This works too
#' get_batch_kv("fruit", "apple", "banana", "coconut")
#'
#' }
#'
get_kv <- function(key) {

    # required paramters
    params <- c("host", "port", "swagger")

    # defensive
    if(!all(params %in% ls(envir = consul))) {
        stop("One or more Consul parameters cannot be found.")
    }

    # form url
    url <- sprintf("http://%s:%s/%s/%s", consul$host, consul$port, consul$swagger, key)

    # send request
    res <- GET(url) %>% content()

    # if return empty
    if(!length(res) > 0) {
        return(NA)
    }

    # extract value and decode
    tryCatch(
        res[[1]]$Value %>% base64decode() %>% rawToChar(),
        error = function(e) { print(e); NA }
    )

}

#' @export
#' @rdname get_kv
get_batch_kv <- function(path, ...) {

    # required paramters
    params <- c("host", "port", "swagger")

    # defensive
    if(!all(params %in% ls(envir = consul))) {
        stop("One or more Consul parameters cannot be found.")
    }

    # forming url
    url <- sprintf("http://%s:%s/%s/%s?recurse", consul$host, consul$port, consul$swagger, path)

    # send GET request
    res <- GET(url) %>% content()

    # if return empty
    if (!length(res) > 0) {
        return(NULL)
    }

    # first on list is folder/, not useful
    res[[1]] <- NULL

    # from Consul
    consul.k <- sapply(res, function(x) x[["Key"]])

    # from user
    user.k <- sapply(list(...), function(x) paste0(path, "/", x))

    # extract value based on keys specified by user
    encoded_vals <- sapply(user.k, function(x) {
        tryCatch(
            res[[which(consul.k == x)]]["Value"],
            error = function(e) "JQ=="
        )
    })

    # decode
    vals <- lapply(encoded_vals, function(x) {
        if(!is.na(x)) {
            tryCatch(
                rawToChar(base64decode(x)),
                error = function(e) NA
            )}
    })

    # format names of list
    names(vals) <- names(vals) %>%
        gsub(pattern = "^\\w*/", replacement = "") %>%
        gsub(pattern = "\\.\\w+", replacement = "")

    # return
    vals

}


# Establish Connection to Common Database ---------------------------------

#' Establish connection to MySQL database
#' @description Fetch credentials from Consul k/v store and establish connection to MySQL.
#'
#' @param mysql_id Database to connect to
#' @param drv MySQL driver. Default RMySQL::MySQL
#'
#' @return
#' Database connection if successful.
#'
#' @importFrom RMySQL MySQL
#' @export
#'
#' @examples
#' \dontrun{
#' est_mysql_conn('somedb')
#' }

est_mysql_conn <- function(mysql_id, drv = MySQL()) {

    # required parameters to establish connection
    params <- c("username", "password", "host", "port", "database")

    # fetch credentials
    db_config <- get_batch_kv(mysql_id, params)

    # check if all required params are specified
    if(!all(params %in% names(db_config))){
        stop("One or more MySQL parameters is missing")
    }

    # test connection here

    # est conn
    c <- DBI::dbConnect(
        drv,
        user = db_config[["username"]],
        password = db_config[["password"]],
        host = db_config[["host"]],
        port = as.numeric(db_config[["port"]]),
        db = mysql_id
    )

    # return connection
    return(c)

}

#' Establish connection to MongoDB
#' @description Fetch credentials from Consul k/v store and establish connection to MySQL.
#'
#' @param mysql_id Database to connect to
#'
#' @return
#' Database connection if successful.
#'
#' @importFrom mongolite mongo
#' @export
#'
#' @examples
#' \dontrun{
#' est_mongo_conn('somedb')
#' }
est_mongo_conn <- function(mongo_id) {

    # required parameters to establish connection
    params <- c("username", "password", "host", "port", "database", "collection")

    # fetch mongo credentials
    mg_config <- get_batch_kv(mongo_id, params)

    # check if the credentials are specified
    if(!all(params %in% names(mg_config))){
        stop("One or more Mongo parameters is missing")
    }

    # est conn
    c <- mongo(
        collection = mg_config$collection,
        url = with(mg_config,
                   # mongodb://username:password@host:port
                   sprintf("mongodb://%s:%s@%s:%d/", username, password, host, as.numeric(port))),
        db = mg_config$database
    )

    # return connection
    return(c)

}


# Create Config.R from Template -------------------------------------------

#' Create template for 'Config.R'
#'
#' @param path Path to generate template
#'
#' @return
#' Boolean - True if success, False if failure
#'
#' @export
#'
init_config <- function(path) {

    # filepath
    file <- sprintf("%s/config.R", path)

    # not overwrite if file exists in path
    if(file.exists(file)) {
        message("File exists in %s. Not proceeding.", path)
        return(FALSE)
    }
    # read from template
    template <- readLines(system.file("template", "config.R", package = "dkbi"))

    # export
    write(template, file)

    # return boolean
    file.exists(file)

}


