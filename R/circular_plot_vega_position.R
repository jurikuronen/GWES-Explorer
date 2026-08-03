# Feature and position data for the inner circular-plot views.
.circular_plot_vega_position_data_and_links <- function(position_data, position_links) {
    position_data_and_links <- .circular_plot_vega_position_data(position_data)
    position_data_and_links <- append(position_data_and_links,
                                      .circular_plot_vega_position_links(position_links))
}

.circular_plot_vega_position_marks <- function() {
    list(
        .circular_plot_vega_position_marks_symbols(1),
        .circular_plot_vega_position_marks_symbols(2),
        .circular_plot_vega_position_marks_links()
    )
}

.circular_plot_vega_position_data <- function(position_data) {
    list(
        list(
            name = "position_data",
            values = position_data,
            transform = list(
                .vega_formula("angle_1", .position_angle_expr("feature_data", "angle_1")),
                .vega_formula("angle_2", .position_angle_expr("feature_data", "angle_2")),
                .vega_formula("x_1", "origoX + (radius_gene_view_1 - 5) * cos(PI * datum.angle_1 / 180)"),
                .vega_formula("y_1", "origoY + (radius_gene_view_1 - 5) * sin(PI * datum.angle_1 / 180)"),
                .vega_formula("x_2", "origoX + (radius_gene_view_2 - 5) * cos(PI * datum.angle_2 / 180)"),
                .vega_formula("y_2", "origoY + (radius_gene_view_2 - 5) * sin(PI * datum.angle_2 / 180)"),
                .vega_formula("feature",
                              .vega_data_query("feature_data", "datum.feature_row - 1", "feature"))
            )
        ),
        .vega_simple_filter("position_data_feature_1",
                            "position_data",
                            .is_selected_region("datum.region", 1)),
        .vega_simple_filter("position_data_feature_2",
                            "position_data",
                            .is_selected_region("datum.region", 2))
    )
}

.circular_plot_vega_position_links <- function(position_links) {
    list(
        list(
            name = "position_links",
            values = position_links,
            transform = list(
                .vega_formula("x", .vega_data_query("position_data", "datum.position_data_index_1", "x_1")),
                .vega_formula("y", .vega_data_query("position_data", "datum.position_data_index_1", "y_1")),
                .vega_formula("x2", .vega_data_query("position_data", "datum.position_data_index_2", "x_2")),
                .vega_formula("y2", .vega_data_query("position_data", "datum.position_data_index_2", "y_2"))
            )
        ),
        .vega_simple_filter("position_links_selected",
                            "position_links",
                            .and("show_gene_links",
                                 .and(.is_selected_region("datum.region_1", 1),
                                      .is_selected_region("datum.region_2", 2))))
    )
}

.circular_plot_vega_position_marks_symbols <- function(selection) {
    list(
        type = "symbol",
        name = paste0("position_symbol_", selection),
        from = list(data = paste0("position_data_feature_", selection)),
        interactive = TRUE,
        encode = list(
            enter = list(
                fill = list(value = .circular_plot_color_pos_symbol_fill()),
                stroke = list(value = .circular_plot_color_pos_symbol_stroke()),
                strokeWidth = list(value = 0.5),
                tooltip = list(signal = "{title: datum.position, 'Located in': datum.feature}")
            ),
            update = list(
                x = list(field = paste0("x_", selection)),
                y = list(field = paste0("y_", selection)),
                size = list(signal = "datum.weight * radius / 16"),
                opacity = list(value = 1)
            )
        )
    )
}

.circular_plot_vega_position_marks_links <- function() {
    list(
        type = "rule",
        from = list(data = "position_links_selected"),
        encode = list(
            enter = list(
                strokeDash = list(value = c(1, 0))
            ),
            update = list(
                x = list(field = "x"), y = list(field = "y"),
                x2 = list(field = "x2"), y2 = list(field = "y2"),
                stroke = list(
                    list(test = .position_link_is_selected(), value = .circular_plot_color_pos_link_selected()),
                    list(test = .and(.is_connected_to_selected_feature(),
                                     .negate(.both_features_are_selected())),
                         value = .circular_plot_color_pos_link_connected()),
                    list(test = .some_feature_is_selected(), value = .circular_plot_color_pos_link_inactive()),
                    list(value = .circular_plot_color_pos_link_default())
                ),
                strokeWidth = list(field = "weight"),
                strokeOpacity = list(
                    list(test = .position_link_is_selected(), signal = "opacity_pos_link_selected"),
                    list(test = .both_features_are_selected(), signal = "opacity_pos_link_inactive"),
                    list(test = .is_connected_to_selected_feature(), signal = "opacity_pos_link_connected"),
                    list(signal = "opacity_pos_link_default")
                )
            )
        )
    )
}
