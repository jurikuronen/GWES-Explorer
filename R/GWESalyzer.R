
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

# Global environments, where data and settings are stored.
.data <- new.env()
.settings <- new.env()

# Default settings.
.settings$circular_plot_radius <- 450
.settings$circular_plot_n_groups <- 12
.settings$circular_plot_n_regions <- 10
# .settings$circular_plot_right_side_size <- 240
# .settings$circular_plot_right_side_adjustment<- 110
# .settings$circular_plot_right_side_vertical_adjustment <- 100
.settings$circular_plot_radius_offset1 <- 100
.settings$circular_plot_radius_offset2 <- 150
.settings$circular_plot_gene_circle_arc_angle1 <- 45
.settings$circular_plot_gene_circle_arc_angle2 <- .settings$circular_plot_gene_circle_arc_angle1 * (.settings$circular_plot_radius - .settings$circular_plot_radius_offset1) / (.settings$circular_plot_radius - .settings$circular_plot_radius_offset2)

# Initialize data keys.
.data$outliers <- NULL
.data$outliers_direct <- NULL
.data$msa <- NULL
.data$tree <- NULL
.data$phenotype <- NULL
.data$gff <- NULL

