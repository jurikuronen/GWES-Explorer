# Computes difference in angle between two points in dataset.
# Assumes points at index 0 and 1 are within the same region.
.angular_distance <- function(dataset, angle_expr) {
    return(paste("(data('", dataset, "')[1].", angle_expr, " - data('", dataset, "')[0].", angle_expr, ")", sep = ""))
}

.initialize_circular_spec <- function(width, height, padding) {
    list(
        `$schema` = "https://vega.github.io/schema/vega/v5.json",
        width = width,
        height = height,
        autosize = "none",
        padding = padding
    )
}

.add_main_plot_marks <- function(spec) {
    list(
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
        ),
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
        ),
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
    )
}

.add_main_plot_scales <- function(spec) {
    list(
        list(
            name = "colorScaleSelected",
            type = "linear",
            domain = list(data = "region_links", field = "weight"),
            range = list(scheme = list(signal = "colorSchemeSelected"))
        ),
        list(
            name = "colorScaleSelected2",
            type = "linear",
            domain = list(data = "region_links", field = "weight"),
            range = list(scheme = list(signal = "colorSchemeSelected2"))
        ),
        list(
             name = "colorScale",
             type = "linear",
             domain = list(data = "region_links", field = "weight"),
             range = list(scheme = list(signal = "colorScheme"))
        ),
        list(
             name = "greyScale",
             type = "linear",
             domain = list(data = "region_links", field = "weight"),
             range = list(scheme = "greys")
        )
    )
}

.get_circular_vega_spec <- function(data, dependencies, rotate = 0, extent = 360, padding = 5, textSize = 10, innerTextSize = 8,
    colorScheme = "purples", colorSchemeSelected = "reds", colorSchemeSelected2 = "teals") {
    # Some ad-hoc code to scale the plot correctly.
    size = .settings$circular_plot_size
    width_adj = .settings$circular_plot_right_side_size
    radius = 0.95 * size / 2 # Ad-hoc computation to make sure the circular plot fits within the main Shiny panel.
    tension = .settings$circular_plot_tension
    if (size >= 800) { textSize <- textSize + 1 }
    if (size >= 1000) { textSize <- textSize + 1 }
    vegawidget::as_vegaspec(append(.initialize_circular_spec(width = size + width_adj, height = size, padding = padding), list(
        signals = .vega_signals(tension, radius, extent, rotate, textSize, innerTextSize, colorScheme, colorSchemeSelected, colorSchemeSelected2),
        data = .vega_data(data, dependencies),
        marks = .add_main_plot_marks(),
        scales = .add_main_plot_scales()
    )))
}


