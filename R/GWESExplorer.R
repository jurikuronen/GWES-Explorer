#' @title Launch GWES-Explorer
#'
#' @description Launch the Shiny app for GWES-Explorer.
#'
#' @param launch_in_browser Boolean value, true by default. If set to true, launches GWES-Explorer in the user's default browser.
#' @param max_request_size Integer value, 32MB (32 * 1024 * 1024) by default. Sets maximum input file size limit.
#'
#' @return NULL
#'
#' @export launch_GWESExplorer
launch_GWESExplorer <- function(launch_in_browser = TRUE, max_request_size = 32 * 1024 * 1024) {
    options(shiny.maxRequestSize = max_request_size) # Set maximum file size limit.
    .initialize_data_keys()
    ui <- .create_ui()
    shiny::shinyApp(ui, .server, options = list(launch.browser = launch_in_browser))
}
