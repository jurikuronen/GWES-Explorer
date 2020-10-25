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

.initialize_circular_spec <- function() {
    list(
        `$schema` = "https://vega.github.io/schema/vega/v5.json",
        width = .get_cp_size(),
        height = .get_cp_size(),
        autosize = "none",
        padding = .get_cp_padding()
    )
}

# Initial Vega spec.
.circular_plot_vega_spec <- function(data, dependencies) {
    vegawidget::as_vegaspec(append(.initialize_circular_spec(), list(
        signals = .circular_plot_vega_signals(),
        data = .circular_plot_vega_region_data(data, dependencies),
        marks = .circular_plot_vega_region_marks(),
        scales = .circular_plot_vega_region_scales()
    )))
}

# Various auxiliary functions for Boolean logic inside Vega.
.or <- function(callback1, callback2) { paste0("(", callback1, " || ", callback2, ")") }
.and <- function(callback1, callback2) { paste0("(", callback1, " && ", callback2, ")") }
.negate <- function(callback) { paste0("(!", callback, ")") }

.region_is_selected <- function(selection) { paste0("(selected_region_", selection, " != null)") }
.is_active_region <- function(element) { paste0("(", element, " === active_region)") }
.is_selected_region <- function(element, selection) { paste0("(", element, " === selected_region_", selection, ")") }
.is_one_of_selected_regions <- function(element) { .or(.is_selected_region(element, 1), .is_selected_region(element, 2)) }
.some_region_is_selected <- function() { .or(.region_is_selected(1), .region_is_selected(2)) }
.both_regions_are_selected <- function() { .and(.region_is_selected(1), .region_is_selected(2)) }
.only_one_region_is_selected <- function() { .and(.some_region_is_selected(), .negate(.both_regions_are_selected())) }

.region_link_is_selected <- function() { .or(.and(.is_selected_region("parent.source", 1), .is_selected_region("parent.target", 2)), .and(.is_selected_region("parent.source", 2), .is_selected_region("parent.target", 1))) }
.is_connected_to_selected_region <- function() { .or(.is_one_of_selected_regions("parent.source"), .is_one_of_selected_regions("parent.target")) }
.region_link_is_active <- function() { .or(.is_active_region("parent.source"), .is_active_region("parent.target")) }

.gene_is_selected <- function(selection) { paste0("(selected_gene_", selection, " != null)") }
.is_active_gene <- function(element) { paste0("(", element, " === active_gene)") }
.is_selected_gene <- function(element, selection) { paste0("(", element, " === selected_gene_", selection, ")") }
.some_gene_is_selected <- function() { .or(.gene_is_selected(1), .gene_is_selected(2)) }
.both_genes_are_selected <- function() { .and(.gene_is_selected(1), .gene_is_selected(2)) }

.pos_link_is_selected <- function() { .and(.is_selected_gene("datum.gene_1", 1), .is_selected_gene("datum.gene_2", 2)) }
.is_connected_to_selected_gene <- function() { .or(.is_selected_gene("datum.gene_1", 1), .is_selected_gene("datum.gene_2", 2)) }
