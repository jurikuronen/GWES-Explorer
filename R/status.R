# Status constants.
.STATUS_SUCCESS <- 0
.STATUS_FAILURE <- 1

.status <- function(result, status_msg) {
    return(data.frame(success = result, status_msg = shiny::HTML(status_msg)))
}