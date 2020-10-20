# Global environments, where data and settings are stored.
.data <- new.env()
.settings <- new.env()

# Default settings.
.settings$circular_plot_tension <- 0.7
.settings$circular_plot_radius <- 450
.settings$circular_plot_text_size <- 12
.settings$circular_plot_small_text_size <- 7
.settings$circular_plot_padding <- 5
.settings$circular_plot_n_groups <- 12
.settings$circular_plot_n_regions <- 10
.settings$circular_plot_radius_offset <- 120
.settings$circular_plot_gene_circle_arc_angle <- 45

.settings$circular_plot_opacity_selected <- 1
.settings$circular_plot_opacity_active <- 1
.settings$circular_plot_opacity_connected <- 0.5
.settings$circular_plot_opacity_inactive <- 0.2
.settings$circular_plot_opacity_default <- 0.6
.settings$circular_plot_opacity_background <- 1
.settings$circular_plot_opacity_pos_link_selected <- 1
.settings$circular_plot_opacity_pos_link_connected <- 0.9
.settings$circular_plot_opacity_pos_link_inactive <- 0.8
.settings$circular_plot_opacity_pos_link_default <- 1

.settings$circular_plot_color_pos_link_selected <- "#8800cc"
.settings$circular_plot_color_pos_link_connected <- "#9b39cc"
.settings$circular_plot_color_pos_link_inactive <- "#9dbec9"
.settings$circular_plot_color_pos_link_default <- "#0099cc"
.settings$circular_plot_color_background <- "#bfdfff"
.settings$circular_plot_color_pos_symbol_fill <- "#d3d3d3"
.settings$circular_plot_color_pos_symbol_stroke <- "#cc3300"

# Initialize data keys.
.data$outliers <- NULL
.data$outliers_direct <- NULL
.data$msa <- NULL
.data$tree <- NULL
.data$phenotype <- NULL
.data$gff <- NULL
