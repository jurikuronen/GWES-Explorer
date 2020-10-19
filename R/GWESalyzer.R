
#' @title Launch GWESalyzer
#'
#' @description Launch the Shiny app for GWESalyzer.
#'
#' @param launch_in_browser Boolean value, true by default. If set to true, launches GWESalyzer in the user's default browser.
#'
#' @return NULL
#'
#' @export launch_GWESalyzer
launch_GWESalyzer <- function(launch_in_browser = TRUE) {
    shiny::shinyApp(.ui, .server, options = list(launch.browser = launch_in_browser))
}
