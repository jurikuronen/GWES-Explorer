#' @title Launch GWES-Explorer
#'
#' @description Launch the Shiny app for GWES-Explorer.
#'
#' @param launch_in_browser Launch GWES-Explorer in the user's defualt browser. True by default.
#' @param max_request_size Maximum input file size limit in bits. 32MB by default.
#'
#' @return NULL
#'
#' @export launch_GWESExplorer
launch_GWESExplorer <- function(launch_in_browser = TRUE,
                                max_request_size = 32 * 1024 * 1024)
{
    options(shiny.maxRequestSize = max_request_size) # Set maximum file size limit.

    # Initialize data keys.
    .data$edges <- NULL
    .data$gff <- NULL
    .data$msa <- NULL
    .data$outliers <- NULL
    .data$outliers_direct <- NULL
    .data$phenotype <- NULL
    .data$tree <- NULL

    shiny::shinyApp(.create_ui_layout(),
                    .server,
                    options = list(launch.browser = launch_in_browser))
}
