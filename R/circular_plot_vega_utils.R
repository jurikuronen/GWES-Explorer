# Computes difference in angle between two points in dataset.
# Assumes points at index 0 and 1 are within the same region.
.angular_distance <- function(dataset, angle_expr) {
    return(paste0("(", .vega_data_query(dataset, 1, angle_expr), " - ", .vega_data_query(dataset, 0, angle_expr), ")"))
}

.pos_angle_expr <- function(dataset, angle_expr) {
    return(paste0("(", .vega_data_query(dataset, "datum.parent - 1", angle_expr), " - ", .angular_distance(dataset, angle_expr), " / 2 + datum.pos_in_gene * ", .angular_distance(dataset, angle_expr), ")"))
}

.vega_color_scale <- function(name, type, data, field, scheme) {
    list(name = name, type = type, domain = list(data = data, field = field), range = list(scheme = scheme))
}

.vega_formula <- function(name, expr, initonly = FALSE) {
    list(type = "formula", as = name, expr = expr, initonly = initonly)
}

.vega_simple_filter <- function(name, source, filter_expr) {
    list(name = name, source = source, transform = list(list(type = "filter", expr = filter_expr)))
}

.vega_data_query <- function(data, idx, member) {
    paste0("data('", data, "')[", idx, "].", member)
}

.vega_get_region_angle <- function() {
    .vega_data_query("region_data", "datum.region - 1", "angle")
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

# Initial Vega spec.
.circular_plot_vega_spec <- function(data, dependencies, rotate = 0, extent = 360,
    colorScheme = "purples", colorSchemeSelected = "reds", colorSchemeSelected2 = "teals") {
    radius = .get_cp_radius()
    size = 2.05 * radius # Make sure the circular plot fits within the main Shiny panel.
    padding = .get_cp_padding()
    tension = .get_cp_tension()
    textSize = .get_cp_text_size()
    innerTextSize = .get_cp_small_text_size()
    vegawidget::as_vegaspec(append(.initialize_circular_spec(width = size, height = size, padding = padding), list(
        signals = .circular_plot_vega_signals(tension, radius, extent, rotate, textSize, innerTextSize, colorScheme, colorSchemeSelected, colorSchemeSelected2),
        data = .circular_plot_vega_region_data(data, dependencies),
        marks = .circular_plot_vega_region_marks(),
        scales = .circular_plot_vega_region_scales()
    )))
}


