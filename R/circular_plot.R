.render_circular_plot <- function(data) {
    vegawidget::renderVegawidget({
        if (is.null(data$edges)) {
            return(NULL)
        }
        data$edges
    })
}

.set_circular_plot_signals <- function(data, selected_row) {
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_region_1",
                                    data$outliers_direct$Pos_1_region[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_feature_1",
                                    data$outliers_direct$Pos_1_feature_row[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_1",
                                    data$outliers_direct$Pos_1[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_region_2",
                                    data$outliers_direct$Pos_2_region[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_feature_2",
                                    data$outliers_direct$Pos_2_feature_row[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_2",
                                    data$outliers_direct$Pos_2[selected_row])
}

# Precomputes necessary data for rendering the circular plot.
.precompute_circular_plot_data <- function(data) {
    # Assign each GFF row to one of the circular plot's regions.
    data$gff$feature_regions <- .compute_feature_regions(data, .circular_plot_regions())

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
    data$outliers_direct$Pos_1_region <- data$gff$feature_regions[position_1_feature_rows]
    data$outliers_direct$Pos_2_region <- data$gff$feature_regions[position_2_feature_rows]

    # Build the outer region slices and links.
    circular_data <- .create_circular_data(data)
    top_level_dependencies <- .create_top_level_links(data)
    edges <- .circular_plot_vega_spec(circular_data, top_level_dependencies)

    # Add the feature and position data used by the two inner views.
    feature_data <- .create_feature_data(data)
    position_data <- .create_position_data(data)
    position_links <- .cpp_create_bidirectional_position_links(data$outliers_direct, position_data)
    position_links$weight <- .rescale_weights(position_links$MI, 0.5, 1)
    feature_data <- .add_link_info_to_feature_data(data, feature_data, position_links)
    edges$data <- append(edges$data, .circular_plot_vega_feature_data(feature_data))
    edges$data <- append(edges$data,
                         .circular_plot_vega_position_data_and_links(position_data, position_links))
    edges$marks <- append(edges$marks, .circular_plot_vega_feature_marks())
    edges$marks <- append(edges$marks, .circular_plot_vega_position_marks())

    data$edges <- edges
}

.rescale_weights <- function(weights, a, b) {
    min_w <- min(weights)
    max_w <- max(weights)

    # Avoid division by zero when all weights are equal.
    if (min_w == max_w) {
        return(rep((a + b) / 2, length(weights)))
    }

    return((weights - min_w) * (b - a) / (max_w - min_w) + a)
}
