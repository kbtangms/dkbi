#' @importFrom httr status_code
check_status <- function(res){
    stop_if_not(.x = status_code(res),
                .p = ~ .x == 200,
                msg = "The API returned an error")
}


#' Get a single key/value from Consul
#'
#' @param key which key to retrieve
#'
#' @return
#' Decoded value retrieved from Consul. 'NA' if empty.
#'
#' @importFrom magrittr %>%
#' @importFrom httr GET content
#' @importFrom base64enc base64decode
#' @export
#'
#' @examples
#' \dontrun{
#' get_kv("fruit/apple")
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


