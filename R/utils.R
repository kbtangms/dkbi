#' @importFrom httr status_code
check_status <- function(res){
    stop_if_not(.x = status_code(res),
                .p = ~ .x == 200,
                msg = "The API returned an error")
}


#' Obtain key/value from Consul
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
