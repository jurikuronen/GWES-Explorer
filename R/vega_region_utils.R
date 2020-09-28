.pos_angle_expr <- function(dataset, angle_expr) {
    return(paste("(data('", dataset, "')[datum.parent - 1].", angle_expr, " - ", .angular_distance(dataset, angle_expr), " / 2 + datum.pos_in_gene * ", .angular_distance(dataset, angle_expr), ")" , sep = ""))
}

# Pos links not implemented yet.
.get_gene_data <- function(gene_data, pos_data, pos_links) {
    list(
        list(
            name = "gene_data",
            values = gene_data,
            transform = list(
                list(type = "formula", as = "angle_step_size", expr = "90 * 1.25 * datum.step_size"),
                list(type = "formula", as = "angle_leftside", expr = paste("(1.25 * 90 + 90 * 1.25 * datum.angle_step) % 360", sep = "")),
                list(type = "formula", as = "angle_rightside", expr = paste("(4.75 * 90 - 90 * 1.25 * datum.angle_step) % 360", sep = "")),
                list(type = "formula", as = "x_leftside", expr = "origoX + (small_radius - 50) * cos(PI * datum.angle_leftside / 180)"),
                list(type = "formula", as = "y_leftside", expr = "origoY + (small_radius - 50) * sin(PI * datum.angle_leftside / 180)"),
                list(type = "formula", as = "x_rightside", expr = "origoX + (small_radius - 50) * cos(PI * datum.angle_rightside / 180)"),
                list(type = "formula", as = "y_rightside", expr = "origoY + (small_radius - 50) * sin(PI * datum.angle_rightside / 180)")
            )
        ),
        list(
            name = "pos_data",
            values = pos_data,
            transform = list(
                list(type = "formula", as = "angle_leftside", expr = .pos_angle_expr("gene_data", "angle_leftside")),
                list(type = "formula", as = "angle_rightside", expr = .pos_angle_expr("gene_data", "angle_rightside")),
                list(type = "formula", as = "x_leftside", expr = "origoX + (small_radius - 55) * cos(PI * datum.angle_leftside / 180)"),
                list(type = "formula", as = "y_leftside", expr = "origoY + (small_radius - 55) * sin(PI * datum.angle_leftside / 180)"),
                list(type = "formula", as = "x_rightside", expr = "origoX + (small_radius - 55) * cos(PI * datum.angle_rightside / 180)"),
                list(type = "formula", as = "y_rightside", expr = "origoY + (small_radius - 55) * sin(PI * datum.angle_rightside / 180)")
            )
        ),
        list(
            name = "gene_data_selected_1",
            source = "gene_data",
            transform = list(
                list(type = "filter", expr = "datum.region === selected_region_1"),
                list(type = "formula", as = "x", expr = "datum.x_leftside"),
                list(type = "formula", as = "y", expr = "datum.y_leftside"),
                list(type = "formula", as = "angle", expr = "datum.angle_leftside")
            )
        ),
        list(
            name = "gene_data_selected_2",
            source = "gene_data",
            transform = list(
                list(type = "filter", expr = "datum.region === selected_region_2"),
                list(type = "formula", as = "x", expr = "datum.x_rightside"),
                list(type = "formula", as = "y", expr = "datum.y_rightside"),
                list(type = "formula", as = "angle", expr = "datum.angle_rightside")
            )
        ),
        list(
            name = "pos_data_gene_1",
            source = "pos_data",
            transform = list(
                list(type = "filter", expr = "datum.region === selected_region_1"),
                list(type = "formula", as = "x", expr = "datum.x_leftside"),
                list(type = "formula", as = "y", expr = "datum.y_leftside"),
                list(type = "formula", as = "angle", expr = "datum.angle_leftside")
            )
        ),
        list(
            name = "pos_data_gene_2",
            source = "pos_data",
            transform = list(
                list(type = "filter", expr = "datum.region === selected_region_2"),
                list(type = "formula", as = "x", expr = "datum.x_rightside"),
                list(type = "formula", as = "y", expr = "datum.y_rightside"),
                list(type = "formula", as = "angle", expr = "datum.angle_rightside")
            )
        ),
        list(
            name = "pos_data_selected_1",
            source = "pos_data",
            transform = list(
                list(type = "filter", expr = "datum.parent === selected_gene_1"),
                list(type = "identifier", as = "vertical_offset"),
                list(type = "formula", as = "x", expr = "right_side_start")
            )
        ),
        list(
            name = "pos_data_selected_2",
            source = "pos_data",
            transform = list(
                list(type = "filter", expr = "datum.parent === selected_gene_2"),
                list(type = "identifier", as = "vertical_offset"),
                list(type = "formula", as = "x", expr = "right_side_start + right_side_x_adj")
            )
        ),
        list(
            name = "pos_links",
            values = pos_links,
            transform = list(
                list(type = "formula", as = "x", expr = "data('pos_data')[datum.pos_data_idx_1].x_leftside"),
                list(type = "formula", as = "y", expr = "data('pos_data')[datum.pos_data_idx_1].y_leftside"),
                list(type = "formula", as = "x2", expr = "data('pos_data')[datum.pos_data_idx_2].x_rightside"),
                list(type = "formula", as = "y2", expr = "data('pos_data')[datum.pos_data_idx_2].y_rightside")
            )
        ),
        list(
            name = "regional_pos_links",
            source = "pos_links",
            transform = list(
                list(type = "filter", expr = "datum.region_1 === selected_region_1 && datum.region_2 === selected_region_2")
            )
        )
    )
}

.add_gene_text <- function(selection) {
    align_text <- "right"
    if (selection == 2) align_text <- "left"
    angle_val <- 180
    if (selection == 2) angle_val <- 0
    list(
        type = "text",
        from = list(data = paste0("gene_data_selected_", selection)),
        name = paste0("gene_text_", selection),
        encode = list(
            enter = list(text = list(field = "name"), baseline = list(value = "middle")),
            update = list(
                x = list(field = "x"),
                y = list(field = "y"),
                dx = list(value = 2 - 4 * (selection == 1)),
                angle = list(signal = paste(angle_val, "+ datum.angle")),
                align = list(value = align_text),
                fontSize = list(signal = "innerTextSize"),
                fontWeight = list(
                    list(test = paste0("selected_gene_", selection, " === datum.id"), value = "bold"),
                    list(value = "normal")
                ),
                fill = list(value = "black"),
                opacity = list(value = 1)
            )
        )
    )
}

.add_gene_arcs <- function(selection) {
    list(
        type = "arc",
        from = list(data = paste0("gene_data_selected_", selection)),
        name = paste0("gene_arc_", selection),
        encode = list(
            enter = list(fill = list(value = "#3399ff")),
            update = list(
                x = list(signal = "origoX"),
                y = list(signal = "origoY"),
                startAngle = list(signal = "PI / 2 + (datum.angle - 0.95 * datum.angle_step_size / 2) * PI / 180"),
                endAngle = list(signal = "PI / 2 + (datum.angle + 0.95 * datum.angle_step_size / 2) * PI / 180"),
                innerRadius = list(signal = "(small_radius - 50) - 5"),
                outerRadius = list(signal = "small_radius - 50"),
                strokeOpacity = list(value = 0),
                fillOpacity = list(
                    list(test = paste0("selected_gene_", selection, " === datum.id"), value = 1),
                    list(test = paste0("selected_gene_", selection, " != null"), value = 0.2),
                    list(value = 0.6)
                ),
                opacity = list(value = 1)
            )
        )
    )
}

.add_pos_symbols <- function(selection) {
    list(
        type = "symbol",
        name = paste0("pos_symbol_", selection),
        from = list(data = paste0("pos_data_gene_", selection)),
        encode = list(
            enter = list(
                fill = list(value = "#d3d3d3"),
                stroke = list(value = "#cc3300"),
                strokeWidth = list(value = 0.5)
            ),
            update = list(
                x = list(field = "x"),
                y = list(field = "y"),
                size = list(signal = "datum.weight * small_radius / 10"),
                opacity = list(value = 1)
            )
        )
    )
}

.add_regional_pos_links <- function() {
    list(
        type = "rule",
        from = list(data = "regional_pos_links"),
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

.add_selected_gene_text <- function(selection) {
    list(
        type = "text",
        name = paste0("selected_gene_text_", selection),
        from = list(data = paste0("pos_data_selected_", selection)),
        encode = list(
            enter = list(
                text = list(field = "name"),
                baseline = "middle"
            ),
            update = list(
                x = list(
                    list(test = paste0("selected_position_", selection, " === null"), field = "x"),
                    list(value = 0)
                ),
                y = list(
                    list(test = paste0("selected_position_", selection, " === null"),
                        signal = paste0("right_side_y_adj + 15 * (datum.vertical_offset - data('pos_data_selected_", selection, "')[0].vertical_offset)")),
                    list(value = 0)
                ),
                fontSize = list(signal = "innerTextSize"),
                fill = list(value = "black"),
                opacity = list(
                    list(test = paste0("selected_position_", selection, " === null"), value = 1),
                    list(value = 0)
                )
            )
        )
    )
}

.add_selected_gene_main_text <- function(selection) {
    list(
        type = "text",
        name = paste0("selected_gene_main_text_", selection),
        encode = list(
            enter = list(baseline = "middle"),
            update = list(
                x = list(signal = paste("right_side_start + right_side_x_adj *", selection - 1)),
                y = list(signal = "right_side_y_adj - 20"),
                fontSize = list(signal = "innerTextSize"),
                fill = list(value = "black"),
                text = list(
                    list(test = paste0("selected_position_", selection, "!= null"), value = ""),
                    list(test = paste0("selected_gene_", selection, " != null"), signal = paste0("data('gene_data')[selected_gene_", selection, " - 1].name + ':'")),
                    list(value = "")
                ),
                fontWeight = list(value = "bold")
            )
        )
    )
}

.add_selected_position_main_text <- function(selection) {
    list(
        type = "text",
        name = paste0("selected_position_main_text_", selection),
        encode = list(
            enter = list(baseline = "middle"),
            update = list(
                x = list(signal = paste("right_side_start + right_side_x_adj *", selection - 1)),
                y = list(signal = "right_side_y_adj - 40"),
                fontSize = list(signal = "innerTextSize"),
                fill = list(value = "black"),
                text = list(
                    list(test = paste0("selected_position_", selection, " != null"), signal = paste0("selected_position_", selection)),
                    list(value = "")
                ),
                fontWeight = list(value = "bold")
            )
        )
    )
}

.add_selected_position_main_gene_text <- function(selection) {
    list(
        type = "text",
        name = paste0("selected_position_main_gene_text_", selection),
        encode = list(
            enter = list(baseline = "middle"),
            update = list(
                x = list(signal = paste("right_side_start + right_side_x_adj *", selection - 1)),
                y = list(signal = "right_side_y_adj - 20"),
                fontSize = list(signal = "innerTextSize"),
                fill = list(value = "black"),
                text = list(
                    list(test = paste0("selected_position_", selection, " != null"), signal = paste0("'(' + data('gene_data')[selected_gene_", selection, " - 1].name + '):'")),
                    list(value = "")
                ),
                fontWeight = list(value = "bold")
            )
        )
    )
}

.get_gene_marks <- function() {
    list(
        .add_gene_text(1),
        .add_gene_arcs(1),
        .add_gene_text(2),
        .add_gene_arcs(2),
        .add_pos_symbols(1),
        .add_pos_symbols(2),
        .add_selected_gene_main_text(1),
        .add_selected_gene_main_text(2),
        .add_selected_gene_text(1),
        .add_selected_gene_text(2),
        .add_selected_position_main_text(1),
        .add_selected_position_main_text(2),
        .add_selected_position_main_gene_text(1),
        .add_selected_position_main_gene_text(2),
        .add_regional_pos_links()
    )
}
