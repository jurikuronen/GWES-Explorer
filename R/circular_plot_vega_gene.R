.circular_plot_vega_gene_marks <- function() {
    list(
        .circular_plot_vega_gene_marks_background(1),
        .circular_plot_vega_gene_marks_text(1),
        .circular_plot_vega_gene_marks_arcs(1),
        .circular_plot_vega_gene_marks_background(2),
        .circular_plot_vega_gene_marks_text(2),
        .circular_plot_vega_gene_marks_arcs(2)
    )
}

.circular_plot_vega_gene_data <- function(gene_data) {
    arc_angle1 <- .get_cp_gene_arc_angle(1)
    arc_angle2 <- .get_cp_gene_arc_angle(2)
    list(
        list(
            name = "gene_data",
            values = gene_data,
            transform = list(
                .vega_formula("angle_step_size_1", paste0(arc_angle1, " * datum.step_size")),
                .vega_formula("angle_step_size_2", paste0(arc_angle2, " * datum.step_size")),
                .vega_formula("angle_1", paste0("(", .vega_get_region_angle(), " + ", arc_angle1, " * (datum.angle_step - 0.5)) % 360")),
                .vega_formula("angle_2", paste0("(", .vega_get_region_angle(), " + ", arc_angle2, " * (datum.angle_step - 0.5)) % 360")),
                .vega_formula("x_1", paste0("origoX + radius_genes_1 * cos(PI * datum.angle_1 / 180)")),
                .vega_formula("y_1", paste0("origoY + radius_genes_1 * sin(PI * datum.angle_1 / 180)")),
                .vega_formula("x_2", paste0("origoX + radius_genes_2 * cos(PI * datum.angle_2 / 180)")),
                .vega_formula("y_2", paste0("origoY + radius_genes_2 * sin(PI * datum.angle_2 / 180)")),
                .vega_formula("leftside", paste0("inrange(", .vega_get_region_angle(), ", [90, 270])"))
            )
        ),
        .vega_simple_filter("gene_data_selected_1", "gene_data", .is_selected_region("datum.region", 1)),
        .vega_simple_filter("gene_data_selected_2", "gene_data", .is_selected_region("datum.region", 2)),
        .vega_simple_filter("gene_data_selected_region_1", "region_data", .is_selected_region("datum.id", 1)),
        .vega_simple_filter("gene_data_selected_region_2", "region_data", .is_selected_region("datum.id", 2))
    )
}

.circular_plot_vega_gene_marks_text_dx <- function(selection) {
    if (selection == 1) return("datum.leftside ? -2 : 2")
    return("datum.leftside ? 7 : -7")
}

.circular_plot_vega_gene_marks_text <- function(selection) {
    align_signal <- "datum.leftside ? 'right' : 'left'"
    if (selection == 2) align_signal <- "datum.leftside ? 'left' : 'right'"
    list(
        type = "text",
        from = list(data = paste0("gene_data_selected_", selection)),
        name = paste0("gene_text_", selection),
        interactive = TRUE,
        encode = list(
            enter = list(
                text = list(field = "name"),
                baseline = list(value = "middle"),
                tooltip = list(signal = "{title: datum.name, 'Info about': 'outliers comes here'}")
            ),
            update = list(
                x = list(field = paste0("x_", selection)),
                y = list(field = paste0("y_", selection)),
                dx = list(signal = .circular_plot_vega_gene_marks_text_dx(selection)),
                angle = list(signal = paste0('datum.leftside ? datum.angle_', selection, ' + 180 : datum.angle_', selection)),
                align = list(signal = align_signal),
                fontSize = list(signal = "innerTextSize"),
                fontWeight = list(
                    list(test = .is_selected_gene("datum.id", selection), value = "bold"),
                    list(value = "normal")
                ),
                fill = list(value = "black"),
                opacity = list(value = 1)
            )
        )
    )
}

.circular_plot_vega_gene_marks_arcs <- function(selection) {
    list(
        type = "arc",
        from = list(data = paste0("gene_data_selected_", selection)),
        name = paste0("gene_arc_", selection),
        interactive = TRUE,
        encode = list(
            enter = list(
                fill = list(value = "#3399ff"),
                tooltip = list(signal = "{title: datum.name, 'Info about': 'outliers comes here'}")
            ),
            update = list(
                x = list(signal = "origoX"),
                y = list(signal = "origoY"),
                startAngle = list(signal = paste0("PI / 2 + (datum.angle_", selection, " - 0.95 * datum.angle_step_size_", selection, " / 2) * PI / 180")),
                endAngle = list(signal = paste0("PI / 2 + (datum.angle_", selection, " + 0.95 * datum.angle_step_size_", selection, " / 2) * PI / 180")),
                innerRadius = list(signal = paste0("radius_genes_", selection, " - 5")),
                outerRadius = list(signal = paste0("radius_genes_", selection)),
                strokeOpacity = list(value = 0),
                fillOpacity = list(
                    list(test = .is_selected_gene("datum.id", selection), value = .get_cp_opacity_selected()),
                    list(test = .gene_is_selected(selection), value = .get_cp_opacity_inactive()),
                    list(value = .get_cp_opacity_default())
                )
            )
        )
    )
}

.circular_plot_vega_gene_marks_background <- function(selection) {
    arc_angle <- .get_cp_gene_arc_angle(selection)
    list(
        type = "arc",
        from = list(data = paste0("gene_data_selected_region_", selection)),
        name = paste0("gene_background_", selection),
        #interactive = TRUE,
        encode = list(
            enter = list(
                fill = list(value = .get_cp_color_background()),
                stroke = list(value = "#000000"),
                strokeWidth = list(value = 0.5),
                fillOpacity = list(value = .get_cp_opacity_background()),
                strokeOpacity = list(value = 0.4)
            ),
            update = list(
                x = list(signal = "origoX"),
                y = list(signal = "origoY"),
                startAngle = list(signal = paste0("PI / 2 + (datum.angle - ", arc_angle / 2, " - ", .vega_data_query(paste0("gene_data_selected_", selection), 0, "step_size"), " * ", arc_angle, ") * PI / 180")),
                endAngle = list(signal = paste0("PI / 2 + (datum.angle + ", arc_angle / 2, " + ", .vega_data_query(paste0("gene_data_selected_", selection), 0, "step_size"), " * ", arc_angle, ") * PI / 180")),
                innerRadius = list(signal = paste0("radius_genes_", selection, " - 20 - ", 60 * (selection == 2))),
                outerRadius = list(signal = paste0("radius_genes_", selection, " + 80 - ", 60 * (selection == 2)))
            )
        )
    )
}
