# Status codes.
.STATUS_SUCCESS <- 0
.STATUS_FAILURE <- 1

# Escapes text for HTML.
.escape_html <- function(text) {
    htmltools::htmlEscape(text)
}

# Creates a status result.
.status <- function(status_code, status_message) {
    data.frame(
        success = status_code,
        # Use HTML formatting in status messages.
        status = shiny::HTML(status_message)
    )
}
