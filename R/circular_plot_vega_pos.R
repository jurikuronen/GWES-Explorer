# Gene circle data with pos data and links.
.circular_plot_vega_pos_data_and_links <- function(pos_data, pos_links) {
    pos_data_and_links <- .circular_plot_vega_pos_data(pos_data)
    pos_data_and_links <- append(pos_data_and_links, .circular_plot_vega_pos_links(pos_links))
}

.circular_plot_vega_pos_marks <- function() {
    list(
        .circular_plot_vega_pos_marks_symbols(1),
        .circular_plot_vega_pos_marks_symbols(2),
        .circular_plot_vega_pos_marks_links()
    )
}

.circular_plot_vega_pos_data <- function(pos_data) {
    list(
        list(
            name = "pos_data",
            values = pos_data,
            transform = list(
                .vega_formula("angle_1", .pos_angle_expr("gene_data", "angle_1")),
                .vega_formula("angle_2", .pos_angle_expr("gene_data", "angle_2")),
                .vega_formula("x_1", "origoX + (radius_genes_1 - 5) * cos(PI * datum.angle_1 / 180)"),
                .vega_formula("y_1", "origoY + (radius_genes_1 - 5) * sin(PI * datum.angle_1 / 180)"),
                .vega_formula("x_2", "origoX + (radius_genes_2 - 5) * cos(PI * datum.angle_2 / 180)"),
                .vega_formula("y_2", "origoY + (radius_genes_2 - 5) * sin(PI * datum.angle_2 / 180)"),
                .vega_formula("parent_gene", .vega_data_query("gene_data", "datum.parent - 1", "name"))
            )
        ),
        .vega_simple_filter("pos_data_gene_1", "pos_data", "datum.region === selected_region_1"),
        .vega_simple_filter("pos_data_gene_2", "pos_data", "datum.region === selected_region_2"),
        list(
            name = "pos_data_selected_1",
            source = "pos_data",
            transform = list(
                list(type = "filter", expr = "datum.parent === selected_gene_1"),
                list(type = "identifier", as = "vertical_offset"),
                .vega_formula("x", expr = "right_side_start")
            )
        ),
        list(
            name = "pos_data_selected_2",
            source = "pos_data",
            transform = list(
                list(type = "filter", expr = "datum.parent === selected_gene_2"),
                list(type = "identifier", as = "vertical_offset"),
                .vega_formula("x", expr = "right_side_start + right_side_x_adj")
            )
        )
    )
}

.circular_plot_vega_pos_links <- function(pos_links) {
    list(
        list(
            name = "pos_links",
            values = pos_links,
            transform = list(
                .vega_formula("x", .vega_data_query("pos_data", "datum.pos_data_idx_1", "x_1")),
                .vega_formula("y", .vega_data_query("pos_data", "datum.pos_data_idx_1", "y_1")),
                .vega_formula("x2", .vega_data_query("pos_data", "datum.pos_data_idx_2", "x_2")),
                .vega_formula("y2", .vega_data_query("pos_data", "datum.pos_data_idx_2", "y_2"))
            )
        ),
        .vega_simple_filter("pos_links_selected", "pos_links", "datum.region_1 === selected_region_1 && datum.region_2 === selected_region_2")
    )
}

.circular_plot_vega_pos_marks_symbols <- function(selection) {
    list(
        type = "symbol",
        name = paste0("pos_symbol_", selection),
        from = list(data = paste0("pos_data_gene_", selection)),
        interactive = TRUE,
        encode = list(
            enter = list(
                fill = list(value = "#d3d3d3"),
                stroke = list(value = "#cc3300"),
                strokeWidth = list(value = 0.5),
                tooltip = list(signal = "{title: datum.name, 'Gene': datum.parent_gene, 'Other info': 'comes here'}")
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

.circular_plot_vega_pos_marks_links <- function() {
    list(
        type = "rule",
        from = list(data = "pos_links_selected"),
        encode = list(
            enter = list(
                strokeDash = list(value = c(1, 0))
            ),
            update = list(
                x = list(field = "x"), y = list(field = "y"),
                x2 = list(field = "x2"), y2 = list(field = "y2"),
                stroke = list(
                    list(test = "(selected_gene_1 === null && selected_gene_2 === datum.gene_2)
                         || (selected_gene_2 === null && selected_gene_1 === datum.gene_1)
                         || (selected_gene_1 === datum.gene_1 && selected_gene_2 === datum.gene_2)", value = "#8800cc"),
                    list(value = "#0099CC")
                ),
                strokeWidth = list(
                    list(test = "selected_gene_1 === datum.gene_1 && selected_gene_2 === datum.gene_2", value = 0.8),
                    list(test = "selected_gene_1 != null && selected_gene_2 != null", value = 0.25),
                    list(test = "selected_gene_1 === datum.gene_1 || selected_gene_2 === datum.gene_2", value = 0.65),
                    list(value = 0.5)
                ),
                opacity = list(
                    list(test = "selected_gene_1 === datum.gene_1 && selected_gene_2 === datum.gene_2", value = 1),
                    list(test = "selected_gene_1 != null && selected_gene_2 != null", value = 0.25),
                    list(test = "selected_gene_1 === datum.gene_1 || selected_gene_2 === datum.gene_2", value = 0.75),
                    list(value = 0.5)
                )
                #opacity = list(field = "datum.weight")
            )
        )
    )
}
