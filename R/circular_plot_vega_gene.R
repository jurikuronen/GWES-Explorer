.circular_plot_vega_gene_marks <- function() {
    list(
        .circular_plot_vega_gene_marks_background(1),
        .circular_plot_vega_gene_marks_text(1),
        .circular_plot_vega_gene_marks_arcs(1),
        .circular_plot_vega_gene_marks_background(2),
        .circular_plot_vega_gene_marks_text(2),
        .circular_plot_vega_gene_marks_arcs(2),
        .circular_plot_vega_gene_marks_hover_background(),
        .circular_plot_vega_gene_marks_hover_text()
    )
}

.circular_plot_vega_gene_data <- function(gene_data) {
    list(
        list(
            name = "gene_data",
            values = gene_data,
            transform = list(
                .vega_formula("angle_step_size_1", "gene_arc_angle_1 * datum.step_size"),
                .vega_formula("angle_step_size_2", "gene_arc_angle_2 * datum.step_size"),
                .vega_formula("angle_1", paste0("(", .vega_get_region_angle(), " + gene_arc_angle_1 * (datum.angle_step - 0.5)) % 360")),
                .vega_formula("angle_2", paste0("(", .vega_get_region_angle(), " + gene_arc_angle_2 * (datum.angle_step - 0.5)) % 360")),
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
                tooltip = .vega_get_gene_tooltip()
            ),
            update = list(
                x = list(field = paste0("x_", selection)),
                y = list(field = paste0("y_", selection)),
                dx = list(signal = .circular_plot_vega_gene_marks_text_dx(selection)),
                angle = list(signal = paste0('datum.leftside ? datum.angle_', selection, ' + 180 : datum.angle_', selection)),
                align = list(signal = align_signal),
                fontSize = list(signal = "text_size_gene"),
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
                fill = list(signal = "color_gene_arc"),
                tooltip = .vega_get_gene_tooltip()
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
                    list(test = .is_selected_gene("datum.id", selection), signal = "opacity_selected"),
                    list(test = .gene_is_selected(selection), signal = "opacity_inactive"),
                    list(signal = "opacity_default")
                )
            )
        )
    )
}

.circular_plot_vega_gene_marks_hover_text <- function() {
    list(
        type = "text",
        from = list(data = "gene_data"),
        name = paste0("gene_hover_text"),
        interactive = FALSE,
        encode = list(
            enter = list(
                text = list(field = "genes_linked_to"),
                baseline = list(value = "middle")
            ),
            update = list(
                x = list(signal = "origoX - (datum.x_1 - origoX) / 2"),
                y = list(signal = "origoY - datum.length * text_size_tooltip / 2"),
                align = list(value = "center"),
                fontSize = list(signal = "text_size_tooltip"),
                fontWeight = list(list(value = "normal")),
                fill = list(value = "black"),
                opacity = list(
                    list(test = .is_active_gene("datum.id"), value = 1),
                    list(value = 0)
                )
            )
        )
    )
}

.circular_plot_vega_gene_marks_hover_background <- function() {
    list(
        type = "rect",
        from = list(data = "gene_data"),
        name = "gene_hover_background",
        interactive = FALSE,
        encode = list(
            enter = list(
                fill = list(value = .get_cp_color_background()),
                stroke = list(value = "#000000"),
                strokeWidth = list(value = 0.5),
                cornerRadius = list(value = 5)
            ),
            update = list(
                xc = list(signal = "origoX - (datum.x_1 - origoX) / 2"),
                y = list(signal = "origoY - datum.length * text_size_tooltip / 2 - text_size_tooltip"),
                width = list(signal = "25 * text_size_tooltip"),
                height = list(signal = "datum.length * (text_size_tooltip + 2) + text_size_tooltip"),
                strokeOpacity = list(
                    list(test = "datum.length === 0", value = 0),
                    list(test = .is_active_gene("datum.id"), signal = "opacity_background"),
                    list(value = 0)
                ),
                fillOpacity = list(
                    list(test = "datum.length === 0", value = 0),
                    list(test = .is_active_gene("datum.id"), value = 0.4),
                    list(value = 0)
                )
            )
        )
    )
}

.circular_plot_vega_gene_marks_background <- function(selection) {
    list(
        type = "arc",
        from = list(data = paste0("gene_data_selected_region_", selection)),
        name = paste0("gene_background_", selection),
        interactive = TRUE,
        encode = list(
            enter = list(
                fill = list(value = .get_cp_color_background()),
                stroke = list(value = "#000000"),
                strokeWidth = list(value = 0.5),
                fillOpacity = list(signal = "opacity_background"),
                strokeOpacity = list(value = 0.4)
            ),
            update = list(
                x = list(signal = "origoX"),
                y = list(signal = "origoY"),
                startAngle = list(signal = paste0("PI / 2 + (datum.angle - gene_arc_angle_", selection, " / 2 - ", .vega_data_query(paste0("gene_data_selected_", selection), 0, "step_size"), " * gene_arc_angle_", selection, ") * PI / 180")),
                endAngle = list(signal = paste0("PI / 2 + (datum.angle + gene_arc_angle_", selection, " / 2 + ", .vega_data_query(paste0("gene_data_selected_", selection), 0, "step_size"), " * gene_arc_angle_", selection, ") * PI / 180")),
                outerRadius = list(signal = paste0("radius_genes_", selection, " + 20 + text_size_gene * 9 * ", as.numeric(selection == 1))),
                innerRadius = list(signal = paste0("radius_genes_", selection, " - 20 - text_size_gene * 9 * ", as.numeric(selection == 2)))
            )
        )
    )
}
