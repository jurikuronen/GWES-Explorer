# Region circle data and links.
.circular_plot_vega_region_data <- function(region_data, region_links) {
    region_data <- .circular_plot_vega_region_data_tree(region_data)
    region_data <- append(region_data, .circular_plot_vega_region_links(region_links))
}

.circular_plot_vega_region_scales <- function() {
    list(
        .vega_color_scale("color_scale_default", "linear", "region_links", "weight", list(signal = "color_scheme_default")),
        .vega_color_scale("color_scale_active", "linear", "region_links", "weight", list(signal = "color_scheme_active")),
        .vega_color_scale("color_scale_selected", "linear", "region_links", "weight", list(signal = "color_scheme_selected")),
        .vega_color_scale("color_scale_inactive", "linear", "region_links", "weight", list(signal = "color_scheme_inactive"))
    )
}

.circular_plot_vega_region_marks <- function() {
    list(
        .circular_plot_vega_region_marks_text(),
        .circular_plot_vega_region_marks_arcs(),
        .circular_plot_vega_region_marks_links()
    )
}

.circular_plot_vega_region_data_tree <- function(region_data) {
    list(
        # Full data already in tree format with hidden parent nodes for the regions.
        list(
            name = "region_data_tree",
            values = region_data,
            transform = list(
                list(type = "stratify", key = "id", parentKey = "parent"),
                list(type = "tree", method = "tidy", size = c(1, 1), as = c("alpha", "beta", "depth", "children")),
                .vega_formula("angle", "(rotate + extent * datum.alpha + 270) % 360"),
                .vega_formula("bottomside", "inrange(datum.angle, [0, 180])"),
                .vega_formula("x", "origoX + radius * datum.beta * cos(PI * datum.angle / 180)"),
                .vega_formula("y", "origoY + radius * datum.beta * sin(PI * datum.angle / 180)")
            )
        ),
        # Contains only the regions (hidden parent nodes removed).
        .vega_simple_filter("region_data", "region_data_tree", "datum.draw")
    )
}

.circular_plot_vega_region_links <- function(region_links) {
    list(
        list(
            name = "region_links",
            values = region_links,
            transform = list(.vega_formula("treepath", "treePath('region_data_tree', datum.source, datum.target)", TRUE))
        ),
        .vega_simple_filter("region_links_connected_to_selected_region", "region_links",
            # Filter expression.
            .and(.only_one_region_is_selected(), .or(.is_one_of_selected_regions("datum.source"), .is_one_of_selected_regions("datum.target"))))
    )
}

.circular_plot_vega_region_marks_text <- function() {
    list(
        type = "text",
        from = list(data = "region_data"),
        encode = list(
            enter = list(text = list(field = "name"), baseline = list(value = "middle")),
            update = list(
                x = list(field = "x"),
                y = list(field = "y"),
                dy = list(signal = "16 * (datum.bottomside ? 1 : -1)"),
                angle = list(signal = "datum.bottomside ? datum.angle - 90 : datum.angle + 90"),
                align = list(value = "center"),
                fontSize = list(signal = "text_size_region"),
                fill = list(value = "black")
            )
        )
    )
}

.circular_plot_vega_region_marks_arcs <- function() {
    connected_regions <- "indata('region_links_connected_to_selected_region', 'source', datum.id) || indata('region_links_connected_to_selected_region', 'target', datum.id)"
    list(
        type = "arc",
        name = "region_arc",
        from = list(data = "region_data"),
        encode = list(
            enter = list(fill = list(value = "#3399ff")),
            update = list(
                x = list(signal = "origoX"),
                y = list(signal = "origoY"),
                startAngle = list(signal = paste("PI / 2 + (datum.angle - 0.95 * ", .angular_distance("region_data", "angle"), " / 2) * PI / 180", sep = "")),
                endAngle = list(signal = paste("PI / 2 + (datum.angle + 0.95 * ", .angular_distance("region_data", "angle"), " / 2) * PI / 180", sep = "")),
                innerRadius = list(signal = "radius"),
                outerRadius = list(signal = "radius + 10"),
                strokeOpacity = list(value = 0),
                fillOpacity = list(
                    list(test = .is_one_of_selected_regions("datum.id"), signal = "opacity_selected"),
                    list(test = .is_active_region("datum.id"), signal = "opacity_active"),
                    list(test = connected_regions, signal = "opacity_connected"),
                    list(test = .some_region_is_selected(), signal = "opacity_inactive"),
                    list(signal = "opacity_default")
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
                enter = list(interpolate = list(value = "bundle"), strokeWidth = list(signal = "parent.count")),
                update = list(
                    stroke = list(
                        list(test = .region_link_is_selected(), scale = "color_scale_selected", signal = "parent.weight"),
                        list(test = .region_link_is_active(), scale = "color_scale_active", signal = "opacity_active"),
                        list(test = .both_regions_are_selected(), scale = "color_scale_inactive", signal = "parent.weight"),
                        list(test = .is_connected_to_selected_region(), scale = "color_scale_selected", signal = "parent.weight"),
                        list(test = .some_region_is_selected(), scale = "color_scale_inactive", signal = "parent.weight"),
                        list(scale = "color_scale_default", signal = "parent.weight")
                    ),
                    strokeOpacity = list(
                        list(test = "!show_region_links", value = 0),
                        list(test = .region_link_is_selected(), signal = "opacity_adjustment * parent.weight"),
                        list(test = .region_link_is_active(), signal = "opacity_region_link_active"),
                        list(test = .both_regions_are_selected(), signal = "opacity_region_link_inactive"),
                        list(test = .and(.some_region_is_selected(), .negate(.is_connected_to_selected_region())), signal = "opacity_region_link_inactive"),
                        list(signal = "parent.weight")
                    ),
                    tension = list(signal = "tension"),
                    x = list(field = "x"),
                    y = list(field = "y")
                )
            )
        ))
    )
}

