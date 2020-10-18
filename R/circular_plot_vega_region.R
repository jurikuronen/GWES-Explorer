# Region circle data and links.
.circular_plot_vega_region_data <- function(region_data, region_links) {
    region_data <- .circular_plot_vega_region_data_tree(region_data)
    region_data <- append(region_data, .circular_plot_vega_region_links(region_links))
}

.circular_plot_vega_region_scales <- function() {
    list(
        .vega_color_scale("colorScaleSelected", "linear", "region_links", "weight", list(signal = "colorSchemeSelected")),
        .vega_color_scale("colorScaleSelected2", "linear", "region_links", "weight", list(signal = "colorSchemeSelected2")),
        .vega_color_scale("colorScale", "linear", "region_links", "weight", list(signal = "colorScheme")),
        .vega_color_scale("greyScale", "linear", "region_links", "weight", "greys")
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
        .vega_simple_filter("region_links_selected", "region_links",
            # Filter expression
            paste("((datum.source === selected_region_1 || datum.target === selected_region_1) && selected_region_2 === null) ||",
                "((datum.source === selected_region_2 || datum.target === selected_region_2) && selected_region_1 === null)"))
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
                fontSize = list(signal = "textSize"),
                fill = list(value = "black")
            )
        )
    )
}

.circular_plot_vega_region_marks_arcs <- function() {
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
                    list(test = "datum.id === selected_region_1 || datum.id === selected_region_2", value = 1),
                    list(test = "datum.id === active_region", value = 1),
                    list(test = "indata('region_links_selected', 'source', datum.id) || indata('region_links_selected', 'target', datum.id)", value = 0.5),
                    list(test = "selected_region_1 != null || selected_region_2 != null", value = 0.2),
                    list(value = 0.6)
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
                        list(test = "(parent.source === selected_region_1 && parent.target === selected_region_2) || (parent.source === selected_region_2 && parent.target === selected_region_1)", scale = "colorScaleSelected", signal = "parent.weight"),
                        list(test = "parent.source === active_region || parent.target === active_region", scale = "colorScaleSelected2", value = 1),
                        list(test = "indata('region_links_selected', 'source', parent.source) && indata('region_links_selected', 'target', parent.target)", scale = "colorScaleSelected", signal = "parent.weight"),
                        list(test = "selected_region_1 != null || selected_region_2 != null", scale = "greyScale", signal = "parent.weight"),
                        list(scale = "colorScale", signal = "parent.weight")
                    ),
                    strokeOpacity = list(
                        list(test = "(parent.source === selected_region_1 && parent.target === selected_region_2) || (parent.source === selected_region_2 && parent.target === selected_region_1)", signal = "parent.weight"),
                        list(test = "parent.source === active_region || parent.target === active_region", value = 1),
                        list(test = "(selected_region_1 != null || selected_region_2 != null) && !(indata('region_links_selected', 'source', parent.source) && indata('region_links_selected', 'target', parent.target))", value = 0.2),
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

