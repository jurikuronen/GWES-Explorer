# Region circle data and links.
.circular_plot_vega_region_data <- function(region_data, region_links) {
    region_data <- .circular_plot_vega_region_data_tree(region_data)
    region_data <- append(region_data, .circular_plot_vega_region_links(region_links))
}

.circular_plot_vega_region_scales <- function() {
    list(
        .vega_color_scale("region_link_default_color_scale",
                          "linear",
                          "region_links",
                          "weight",
                          list(signal = "region_link_default_color_palette")),
        .vega_color_scale("region_link_active_color_scale",
                          "linear",
                          "region_links",
                          "weight",
                          list(signal = "region_link_active_color_palette")),
        .vega_color_scale("region_link_inactive_color_scale",
                          "linear",
                          "region_links",
                          "weight",
                          list(signal = "region_link_inactive_color_palette"))
    )
}

.circular_plot_vega_region_marks <- function() {
    list(
        .circular_plot_vega_region_marks_text(),
        .circular_plot_vega_region_marks_arcs(),
        .circular_plot_vega_region_marks_links()
    )
}

# Creates two Vega datasets for the circular plot's region layout:
#
# - region_data_tree: contains tree data with regions as leaf nodes,
#                     and hidden root and parent nodes to route curved links between regions.
# - region_data: subset of the above that contains only the region nodes,
#                with region angle spans already calculated for convenience.
#
# Based on Vega's edge-bundling example (https://vega.github.io/vega/examples/edge-bundling/), which implements the
# method by Holten (2006), https://doi.org/10.1109/TVCG.2006.147.
.circular_plot_vega_region_data_tree <- function(region_data) {
    # Precondition: the tree must have multiple regions per group.
    group_region_counts <- table(region_data$parent_id[which(region_data$is_region)])
    stopifnot(all(group_region_counts > 1))

    list(
        # Full data already in tree format with hidden parent nodes for the regions.
        list(
            name = "region_data_tree",
            values = region_data,
            transform = list(
                list(type = "stratify", key = "id", parentKey = "parent_id"),
                # Vega's edge-bundling example transforms the tree layout to a circular layout.
                # The default x/y coordinates are stored as alpha/beta and used to calculate the angle/radius,
                # which are then converted to the circular plot's x/y coordinates.
                list(type = "tree", method = "tidy", size = c(1, 1), as = c("alpha", "beta", "depth", "children")),
                .vega_formula("angle", "(circle_rotation + circle_degrees * datum.alpha + 270) % 360"),
                .vega_formula("bottomside", "inrange(datum.angle, [0, 180])"),
                .vega_formula("x", "center_x + circle_radius * datum.beta * cos(PI * datum.angle / 180)"),
                .vega_formula("y", "center_y + circle_radius * datum.beta * sin(PI * datum.angle / 180)")
            )
        ),
        # Filtered data that only contains the regions (hidden parent nodes removed).
        list(
            name = "region_data",
            source = "region_data_tree",
            transform = list(
                list(type = "filter", expr = "datum.is_region"),
                # Aggregate data from a region group to calculate the angle spans:
                # - group location in the tree (by min/max of alpha)
                # - count of regions in the group
                list(
                    type = "joinaggregate",
                    groupby = list("parent_id"),
                    fields = list("alpha", "alpha", NULL),
                    ops = c("min", "max", "count"),
                    as = c("first_alpha", "last_alpha", "group_region_count")
                ),
                # Pre-calculate each region's angular span in degrees.
                .vega_formula("angle_span",
                              paste("circle_degrees * (datum.last_alpha - datum.first_alpha)",
                                    "/ (datum.group_region_count - 1)"))
            )
        )
    )
}

.circular_plot_vega_region_links <- function(region_links) {
    list(
        list(
            name = "region_links",
            values = region_links,
            transform = list(.vega_formula("treepath",
                                           "treePath('region_data_tree', datum.source, datum.target)",
                                           TRUE))
        ),
        .vega_simple_filter("region_links_connected_to_selected_region", "region_links",
                            # Filter expression.
                            .and(.only_one_region_is_selected(),
                                 .or(.is_one_of_selected_regions("datum.source"),
                                     .is_one_of_selected_regions("datum.target"))))
    )
}

.circular_plot_vega_region_marks_text <- function() {
    list(
        type = "text",
        from = list(data = "region_data"),
        encode = list(
            enter = list(text = list(field = "group_coordinate_label"), baseline = list(value = "middle")),
            update = list(
                x = list(field = "x"),
                y = list(field = "y"),
                dy = list(signal = "16 * (datum.bottomside ? 1 : -1)"),
                angle = list(signal = "datum.bottomside ? datum.angle - 90 : datum.angle + 90"),
                align = list(value = "center"),
                fontSize = list(signal = "region_group_label_text_size"),
                fill = list(value = "black")
            )
        )
    )
}

.circular_plot_vega_region_marks_arcs <- function() {
    arc_width_fraction <- 1 - .settings$circular_plot_region_gap_fraction
    connected_regions <- paste("indata('region_links_connected_to_selected_region', 'source', datum.id)",
                               "||",
                               "indata('region_links_connected_to_selected_region', 'target', datum.id)")
    list(
        type = "arc",
        name = "region_arc",
        from = list(data = "region_data"),
        encode = list(
            enter = list(
                fill = list(signal = "region_color")
            ),
            update = list(
                x = list(signal = "center_x"),
                y = list(signal = "center_y"),
                startAngle = list(signal = paste0("PI / 2 + (datum.angle - ", arc_width_fraction,
                                                  " * datum.angle_span / 2) * PI / 180")),
                endAngle = list(signal = paste0("PI / 2 + (datum.angle + ", arc_width_fraction,
                                                " * datum.angle_span / 2) * PI / 180")),
                innerRadius = list(signal = "circle_radius"),
                outerRadius = list(signal = "circle_radius + 10"),
                strokeOpacity = list(value = 0),
                fillOpacity = list(
                    list(test = .is_one_of_selected_regions("datum.id"), signal = "region_feature_selected_opacity"),
                    list(test = .is_hovered_region("datum.id"), signal = "region_hovered_opacity"),
                    list(test = connected_regions, signal = "region_connected_opacity"),
                    list(test = .some_region_is_selected(), signal = "region_feature_inactive_opacity"),
                    list(signal = "region_feature_default_opacity")
                )
            )
        )
    )
}

.circular_plot_vega_region_marks_links <- function() {
    list(
        type = "group",
        from = list(facet = list(name = "path", data = "region_links", field = "treepath")),
        marks = list(list(
            type = "line",
            interactive = FALSE,
            from = list(data = "path"),
            encode = list(
                enter = list(interpolate = list(value = "bundle"), strokeWidth = list(signal = "parent.stroke_width")),
                update = list(
                    stroke = list(
                        list(test = .region_link_is_selected(),
                             scale = "region_link_active_color_scale",
                             signal = "parent.weight"),
                        list(test = .region_link_is_connected_to_hovered_region(),
                             signal = "region_link_hovered_color"),
                        list(test = .both_regions_are_selected(),
                             scale = "region_link_inactive_color_scale",
                             signal = "parent.weight"),
                        list(test = .is_connected_to_selected_region(),
                             scale = "region_link_active_color_scale",
                             signal = "parent.weight"),
                        list(test = .some_region_is_selected(),
                             scale = "region_link_inactive_color_scale",
                             signal = "parent.weight"),
                        list(scale = "region_link_default_color_scale",
                             signal = "parent.weight")
                    ),
                    strokeOpacity = list(
                        list(test = "!show_region_links", value = 0),
                        list(test = .region_link_is_selected(),
                             signal = "region_link_selected_opacity"),
                        list(test = .region_link_is_connected_to_hovered_region(),
                             signal = "region_link_hovered_opacity"),
                        list(test = .both_regions_are_selected(),
                             signal = "region_link_inactive_opacity"),
                        list(test = .and(.some_region_is_selected(),
                                         .negate(.is_connected_to_selected_region())),
                             signal = "region_link_inactive_opacity"),
                        list(test = .is_connected_to_selected_region(),
                             signal = "region_link_active_opacity"),
                        list(signal = "region_link_default_opacity * parent.weight")
                    ),
                    tension = list(signal = "region_link_tension"),
                    x = list(field = "x"),
                    y = list(field = "y")
                )
            )
        ))
    )
}
