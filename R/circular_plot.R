# Creates a Shiny renderer for a precomputed circular-plot Vega specification.
# If no GFF3 data was loaded, the specification is NULL and the plot stays empty.
.render_circular_plot <- function(circular_plot_spec) {
    vegawidget::renderVegawidget({
        if (is.null(circular_plot_spec)) {
            return(NULL)
        }
        circular_plot_spec
    })
}

.set_circular_plot_signals <- function(data, selected_row) {
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_region_1",
                                    data$outliers_direct$Pos_1_region[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_feature_row_1",
                                    data$outliers_direct$Pos_1_feature_row[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_1",
                                    data$outliers_direct$Pos_1[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_region_2",
                                    data$outliers_direct$Pos_2_region[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_feature_row_2",
                                    data$outliers_direct$Pos_2_feature_row[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_2",
                                    data$outliers_direct$Pos_2[selected_row])
}

# Rescales values from a source range to a target range.
.rescale_values <- function(values, target_min, target_max, source_min, source_max) {
    if (!all(is.finite(c(source_min, source_max))) || source_min >= source_max) {
        stop("Source range must have finite limits with source_min < source_max.")
    }

    if (!all(is.finite(c(target_min, target_max))) || target_min >= target_max) {
        stop("Target range must have finite limits with target_min < target_max.")
    }

    return((values - source_min) * (target_max - target_min) / (source_max - source_min) + target_min)
}

# Precomputes necessary data for rendering the circular plot.
.precompute_circular_plot_data <- function(data) {
    # Assign each GFF row to one of the circular plot's regions.
    data$gff$feature_region_ids <- .calculate_feature_region_ids(nrow(data$gff), .settings$circular_plot_region_count)

    # Find the feature containing each outlier position.
    outlier_feature_rows <- .cpp_find_outlier_feature_rows(data$gff$start,
                                                           data$gff$end,
                                                           data$outliers_direct$Pos_1,
                                                           data$outliers_direct$Pos_2)

    position_1_feature_rows <- outlier_feature_rows$position_1_feature_row
    position_2_feature_rows <- outlier_feature_rows$position_2_feature_row

    data$outliers_direct$Pos_1_feature_row <- position_1_feature_rows
    data$outliers_direct$Pos_2_feature_row <- position_2_feature_rows
    data$outliers_direct$Pos_1_feature <- data$gff$Name[position_1_feature_rows]
    data$outliers_direct$Pos_2_feature <- data$gff$Name[position_2_feature_rows]
    data$outliers_direct$Pos_1_region <- data$gff$feature_region_ids[position_1_feature_rows]
    data$outliers_direct$Pos_2_region <- data$gff$feature_region_ids[position_2_feature_rows]

    # Calculate the MI range for circular plot weights.
    min_mi <- min(data$outliers$MI)
    max_mi <- max(data$outliers$MI)

    # Build the outer region slices and links.
    region_hierarchy <- .create_region_edge_bundling_hierarchy(data$gff$end)
    region_links <- .create_region_links(data$outliers_direct, min_mi, max_mi)
    circular_plot_spec <- .circular_plot_vega_spec(region_hierarchy, region_links)

    # Add the feature and position data used by the two inner views.
    feature_data <- .create_feature_data(data$gff)
    position_data <- .create_position_data(data$outliers_direct, data$gff)
    # Rescale MI values to 0.5-1. To be used for clearer position marker sizes.
    position_data$weight <- .rescale_values(position_data$weight, 0.5, 1, min_mi, max_mi)
    position_links <- .cpp_create_bidirectional_position_links(data$outliers_direct, position_data)
    position_links$weight <- .rescale_values(position_links$MI, 0.5, 1, min_mi, max_mi)
    feature_data <- .add_link_info_to_feature_data(feature_data, position_links)
    circular_plot_spec$data <- append(circular_plot_spec$data, .circular_plot_vega_feature_data(feature_data))
    circular_plot_spec$data <- append(circular_plot_spec$data,
                                      .circular_plot_vega_position_data_and_links(position_data,
                                                                                  position_links))
    circular_plot_spec$marks <- append(circular_plot_spec$marks, .circular_plot_vega_feature_marks())
    circular_plot_spec$marks <- append(circular_plot_spec$marks, .circular_plot_vega_position_marks())

    data$circular_plot_spec <- circular_plot_spec
}
