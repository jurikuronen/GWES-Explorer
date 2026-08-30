# Circular plot settings.
# These settings are fixed when the GWESExplorer package is built.
# To make any changes work, rebuild the package.

################################################################################
# Color settings
################################################################################

# Background color used for feature views and tooltips.
.settings$circular_plot_background_color <- "#bfdfff"

# Color for the features in a feature view.
.settings$circular_plot_feature_color <- "#3b71d9"

# Color for the regions on the circle.
.settings$circular_plot_region_color <- "#5992ff"

# Outlier position marker colors.
.settings$circular_plot_position_marker_fill_color <- "#d3d3d3"
.settings$circular_plot_position_marker_outline_color <- "#cc3300"

# Color for outlier position links with no feature selected.
.settings$circular_plot_position_link_default_color <- "#0099cc"

# Color for outlier position links connected to one selected feature.
.settings$circular_plot_position_link_active_color <- "#9b39cc"

# Color for outlier position links not matching the selected feature(s).
.settings$circular_plot_position_link_inactive_color <- "#9dbec9"

# Color for outlier position links between both selected features.
.settings$circular_plot_position_link_selected_color <- "#8800cc"

# Vega color palette for region links with nothing selected.
.settings$circular_plot_region_link_default_color_palette <- "purples"

# Vega color palette for region links connected to the hovered-over region.
.settings$circular_plot_region_link_hovered_color_palette <- "teals"

# Vega color palette for region links connected to the selected region, or connecting both selected regions.
.settings$circular_plot_region_link_active_color_palette <- "reds"

# Vega color palette for region links not connected to the selected region, or not connecting both selected regions.
.settings$circular_plot_region_link_inactive_color_palette <- "greys"

################################################################################
# Layout settings
################################################################################

# The angular span of the circular plot in degrees.
.settings$circular_plot_circle_degrees <- 360

# Padding around the circular plot in pixels.
.settings$circular_plot_padding <- 5

# Radius of the circular plot in pixels.
.settings$circular_plot_radius <- 400

# Rotation of the circular plot in degrees.
.settings$circular_plot_rotation <- 0

# Number of region groups and regions per group for the outer circle.
.settings$circular_plot_region_group_count <- 12
.settings$circular_plot_regions_per_group_count <- 10

# Region link tension: 0 draws straight lines; increasing the value (until 1) relaxes the links through the center.
.settings$circular_plot_region_link_tension <- 0.7

# Feature view radius in pixels.
.settings$circular_plot_feature_view_1_radius <- 300
.settings$circular_plot_feature_view_2_radius <- 250

# Feature view angular span in degrees.
.settings$circular_plot_feature_view_1_degrees <- 45
.settings$circular_plot_feature_view_2_degrees <- 45

# Feature view rotation in degrees.
.settings$circular_plot_feature_view_1_rotation <- 0
.settings$circular_plot_feature_view_2_rotation <- 0

# Whether to flip a feature view's labels inward.
.settings$circular_plot_feature_view_1_flip_inwards <- FALSE
.settings$circular_plot_feature_view_2_flip_inwards <- TRUE

################################################################################
# Opacity settings
################################################################################

# Feature view and feature tooltip background opacity.
.settings$circular_plot_background_opacity <- 1

# Default opacity for regions and features.
.settings$circular_plot_region_feature_default_opacity <- 0.6

# Opacity for a hovered-over region.
.settings$circular_plot_region_hovered_opacity <- 1

# Opacity for regions connected to the selected region.
.settings$circular_plot_region_connected_opacity <- 0.5

# Opacity for regions and features not matching the selection.
.settings$circular_plot_region_feature_inactive_opacity <- 0.2

# Opacity for selected regions and features.
.settings$circular_plot_region_feature_selected_opacity <- 1

# Base opacity for region links.
.settings$circular_plot_region_link_base_opacity <- 0.6

# Base opacity for links connected to the hovered-over region.
.settings$circular_plot_region_link_hovered_base_opacity <- 1

# Base opacity for region links connected to the selected region.
.settings$circular_plot_region_link_active_base_opacity <- 0.5

# Base opacity for region links not connected to the selected region, or not connecting both selected regions.
.settings$circular_plot_region_link_inactive_base_opacity <- 0.2

# Base opacity for region links connecting both selected regions.
.settings$circular_plot_region_link_selected_base_opacity <- 1

# Base opacity for outlier position links (without selected features).
.settings$circular_plot_position_link_base_opacity <- 1

# Base opacity for outlier position links connected to the selected feature.
.settings$circular_plot_position_link_active_base_opacity <- 0.9

# Base opacity for outlier position links not matching the selected feature(s).
.settings$circular_plot_position_link_inactive_base_opacity <- 0.8

# Base opacity for outlier position links between both selected features.
.settings$circular_plot_position_link_selected_base_opacity <- 1

################################################################################
# Text settings
################################################################################

# Text size for genomic-feature labels.
.settings$circular_plot_feature_label_text_size <- 8

# Text size for region-group labels.
.settings$circular_plot_region_group_label_text_size <- 12

# Text size for feature-link tooltips.
.settings$circular_plot_feature_link_tooltip_text_size <- 10

################################################################################
# Settings derived from other settings, do not modify.
################################################################################

# Total number of circular plot regions.
.settings$circular_plot_region_count <-
    .settings$circular_plot_region_group_count * .settings$circular_plot_regions_per_group_count

# Circular plot size with extra space for margins.
.settings$circular_plot_size <- 2.1 * .settings$circular_plot_radius
