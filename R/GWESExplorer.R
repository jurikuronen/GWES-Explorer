#' @title Launch GWES-Explorer
#'
#' @description Launch the Shiny app for GWES-Explorer.
#'
#' @param launch_in_browser Whether to launch GWES-Explorer in the user's default browser. Defaults to TRUE.
#' @param max_request_size Maximum input file size in bytes. Defaults to 32 MiB.
#'
#' @return A Shiny application object.
#'
#' @export launch_GWESExplorer
launch_GWESExplorer <- function(launch_in_browser = TRUE,
                                max_request_size = 32 * 1024 * 1024)
{
    options(shiny.maxRequestSize = max_request_size) # Set maximum file size limit.

    shiny::shinyApp(.create_ui_layout(),
                    .server,
                    options = list(launch.browser = launch_in_browser))
}
