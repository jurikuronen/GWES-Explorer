#' @title Launch GWES-Explorer
#'
#' @description Launch the Shiny app for GWES-Explorer.
#'
#' @param launch_in_browser Boolean value, true by default. If set to true, launches GWES-Explorer in the user's default browser.
#'
#' @return NULL
#'
#' @export launch_GWESExplorer
launch_GWESExplorer <- function(launch_in_browser = TRUE) {
    options(shiny.maxRequestSize = 512 * 1024 * 1024) # Increase maximum file size limit to 512MB.
    .initialize_data_keys()
    ui <- .create_ui()
    shiny::shinyApp(ui, .server, options = list(launch.browser = launch_in_browser))
}
