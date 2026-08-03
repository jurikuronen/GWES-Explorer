# Computes difference in angle between two points in a dataset.
# Assumes points at index 0 and 1 are within the same region.
.angular_distance <- function(dataset, angle_expr) {
    return(paste0("(",
                  .vega_data_query(dataset, 1, angle_expr),
                  " - ",
                  .vega_data_query(dataset, 0, angle_expr),
                  ")"))
}

.position_angle_expr <- function(dataset, angle_expr) {
    return(paste0("(",
                  .vega_data_query(dataset, "datum.feature_row - 1", angle_expr),
                  " - ",
                  .angular_distance(dataset, angle_expr),
                  " / 2 + datum.position_in_feature * ",
                  .angular_distance(dataset, angle_expr), ")"))
}

.vega_color_scale <- function(name, type, data, field, scheme) {
    list(name = name,
         type = type,
         domain = list(data = data, field = field),
         range = list(scheme = scheme))
}

.vega_formula <- function(name, expr, initonly = FALSE) {
    list(type = "formula",
         as = name,
         expr = expr,
         initonly = initonly)
}

.vega_simple_filter <- function(name, source, filter_expr) {
    list(name = name,
         source = source,
         transform = list(list(type = "filter",
                               expr = filter_expr)))
}

.vega_data_query <- function(data, index, member) {
    paste0("data('", data, "')[", index, "].", member)
}

.vega_get_region_angle <- function() {
    .vega_data_query("region_data", "datum.region - 1", "angle")
}

.vega_get_feature_tooltip <- function() {
    list(signal = paste("{title: datum.feature,",
                        "'Location': datum.start + '-' + datum.end,",
                        "'Outliers': datum.n_outliers,",
                        "'Linked to': datum.n_features_linked_to,",
                        "'Self-links': datum.n_self_links}"))
}

.initialize_circular_spec <- function() {
    list(
        `$schema` = "https://vega.github.io/schema/vega/v5.json",
        width = .circular_plot_size(),
        height = .circular_plot_size(),
        autosize = "none",
        padding = .circular_plot_padding()
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
.or <- function(callback1, callback2) {
    paste0("(", callback1, " || ", callback2, ")")
}
.and <- function(callback1, callback2) {
    paste0("(", callback1, " && ", callback2, ")")
}
.negate <- function(callback) {
    paste0("(!", callback, ")")
}

.region_is_selected <- function(selection) {
    paste0("(selected_region_", selection, " != null)")
}

.is_active_region <- function(element) {
    paste0("(", element, " === active_region)")
}

.is_selected_region <- function(element, selection) {
    paste0("(", element, " === selected_region_", selection, ")")
}

.is_one_of_selected_regions <- function(element) {
    .or(.is_selected_region(element, 1),
        .is_selected_region(element, 2))
}

.some_region_is_selected <- function() {
    .or(.region_is_selected(1),
        .region_is_selected(2))
}

.both_regions_are_selected <- function() {
    .and(.region_is_selected(1),
         .region_is_selected(2))
}

.only_one_region_is_selected <- function() {
    .and(.some_region_is_selected(),
         .negate(.both_regions_are_selected()))
}

.region_link_is_selected <- function() {
    .or(.and(.is_selected_region("parent.source", 1),
             .is_selected_region("parent.target", 2)),
        .and(.is_selected_region("parent.source", 2),
             .is_selected_region("parent.target", 1)))
}
.is_connected_to_selected_region <- function() {
    .or(.is_one_of_selected_regions("parent.source"),
        .is_one_of_selected_regions("parent.target"))
}
.region_link_is_active <- function() {
    .or(.is_active_region("parent.source"),
        .is_active_region("parent.target"))
}

.feature_is_selected <- function(selection) {
    paste0("(selected_feature_", selection, " != null)")
}

.is_active_feature <- function(element) {
    paste0("(", element, " === active_feature)")
}

.is_selected_feature <- function(element, selection) {
    paste0("(", element, " === selected_feature_", selection, ")")
}

.some_feature_is_selected <- function() {
    .or(.feature_is_selected(1),
        .feature_is_selected(2))
}

.both_features_are_selected <- function() {
    .and(.feature_is_selected(1),
         .feature_is_selected(2))
}

.position_link_is_selected <- function() {
    .and(.is_selected_feature("datum.feature_row_1", 1),
         .is_selected_feature("datum.feature_row_2", 2))
}

.is_connected_to_selected_feature <- function() {
    .or(.is_selected_feature("datum.feature_row_1", 1),
        .is_selected_feature("datum.feature_row_2", 2))
}
