
#' @title Launch GWESalyzer
#'
#' @description Launch the Shiny app for GWESalyzer.
#'
#' @return NULL
#'
#' @export launch_GWESalyzer
launch_GWESalyzer <- function() {
    shiny::shinyApp(.ui, .server)
}

# Global environments, where data and settings are stored.
.data <- new.env()
.settings <- new.env()

# Default settings.
.settings$circular_plot_size <- 800
.settings$circular_plot_n_groups <- 12
.settings$circular_plot_n_regions <- 10
.settings$circular_plot_right_side_size <- 240
.settings$circular_plot_right_side_adjustment<- 110
.settings$circular_plot_right_side_vertical_adjustment <- 100

# Initialize data keys.
.data$outliers <- NULL
.data$outliers_direct <- NULL
.data$msa <- NULL
.data$tree <- NULL
.data$gff <- NULL
.data$gff_genes <- NULL
.data$pos1_genes <- NULL
.data$pos2_genes <- NULL
.data$pos1_regions <- NULL
.data$pos2_regions <- NULL

# These will be updated when the outliers and gff files are uploaded.
.data$region_start <- 1
.data$region_end <- 1
