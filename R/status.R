# Status constants.
.STATUS_SUCCESS <- 0
.STATUS_FAILURE <- 1

.escape_html <- function(text) {
    htmltools::htmlEscape(as.character(text))
}

.status <- function(result, status_msg) {
    return(data.frame(success = result, status = shiny::HTML(status_msg)))
}
