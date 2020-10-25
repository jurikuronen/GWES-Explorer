.circular_plot_vega_signals <- function() {
    signals <- .main_parameter_signals()
    signals <- append(signals, .color_scheme_signals())
    signals <- append(signals, .event_listener_signals())
}

.main_parameter_signals <- function() {
    list(
        list(name = "tension", value = .get_cp_tension()),
        list(name = "radius", value = .get_cp_radius()),
        list(name = "extent", value = .get_cp_extent()),
        list(name = "radius_genes_1", value = .get_cp_radius() - .get_cp_radius_offset(1)),
        list(name = "radius_genes_2", value = .get_cp_radius() - .get_cp_radius_offset(2)),
        list(name = "rotate", value = .get_cp_rotate()),
        list(name = "textSize", value = .get_cp_text_size_default()),
        list(name = "centerTextSize", value = .get_cp_text_size_center()),
        list(name = "innerTextSize", value = .get_cp_text_size_small()),
        list(name = "gene_arc_angle_1", value = .get_cp_gene_arc_angle(1)),
        list(name = "gene_arc_angle_2", value = .get_cp_gene_arc_angle(2)),
        list(name = "origoX", update = "width / 2"),
        list(name = "origoY", update = "height / 2")
    )
}

.color_scheme_signals <- function() {
    list(
        list(name = "color_scheme_default", value = .get_cp_color_scheme_default()),
        list(name = "color_scheme_active", value = .get_cp_color_scheme_active()),
        list(name = "color_scheme_selected", value = .get_cp_color_scheme_selected()),
        list(name = "color_scheme_inactive", value = .get_cp_color_scheme_inactive())
    )
}

.event_listener_signals <- function() {
    list(
        # Mouseovered region.
        list(
            name = "active_region",
            value = NULL,
            on = list(
                list(events = "@region_arc:mouseover", update = "datum.id"),
                list(events = "mouseover[!event.item]", update = "null")
            )
        ),
        # Mouseovered gene
        list(
            name = "active_gene",
            value = NULL,
            on = list(
                list(events = "@gene_arc_1:mouseover", update = "datum.id"),
                list(events = "@gene_arc_2:mouseover", update = "datum.id"),
                list(events = "@gene_text_1:mouseover", update = "datum.id"),
                list(events = "@gene_text_2:mouseover", update = "datum.id"),
                list(events = "@gene_background_1:mouseover", update = "null"),
                list(events = "@gene_background_2:mouseover", update = "null"),
                list(events = "mouseover[!event.item]", update = "null")
            )
        ),
        # Selected regions.
        list(
            name = "selected_region_1",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "!event.shiftKey"), update = "datum.id"),
                list(events = list(type = "click", markname = "gene_text_1", filter = "!event.shiftKey"), update = "datum.region"),
                list(events = list(type = "click", markname = "gene_arc_1", filter = "!event.shiftKey"), update = "datum.region"),
                list(events = list(type = "click", filter = "!event.item && !event.shiftKey"), update = "null")
            )
        ),
        list(
            name = "selected_region_2",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "event.shiftKey"), update = "datum.id"),
                list(events = list(type = "click", markname = "gene_text_2", filter = "!event.shiftKey"), update = "datum.region"),
                list(events = list(type = "click", markname = "gene_arc_2", filter = "!event.shiftKey"), update = "datum.region"),
                list(events = list(type = "click", filter = "!event.item && event.shiftKey"), update = "null")
            )
        ),

        # Selected genes.
        list(
            name = "selected_gene_1",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", markname = "gene_text_1", filter = "!event.shiftKey"), update = "datum.id"),
                list(events = list(type = "click", markname = "gene_arc_1", filter = "!event.shiftKey"), update = "datum.id"),
                list(events = list(type = "click", markname = "gene_background_1", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", filter = "!event.item && !event.shiftKey"), update = "null")
            )
        ),
        list(
            name = "selected_gene_2",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "event.shiftKey"), update = "null"),
                list(events = list(type = "click", markname = "gene_text_2", filter = "!event.shiftKey"), update = "datum.id"),
                list(events = list(type = "click", markname = "gene_arc_2", filter = "!event.shiftKey"), update = "datum.id"),
                list(events = list(type = "click", markname = "gene_background_2", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", filter = "!event.item && event.shiftKey"), update = "null")
            )
        ),

        # Selected position.
        list(
            name = "selected_position_1",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", markname = "gene_text_1", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", markname = "gene_arc_1", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", markname = "gene_background_1", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", filter = "!event.item && !event.shiftKey"), update = "null")
            )
        ),
        list(
            name = "selected_position_2",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "event.shiftKey"), update = "null"),
                list(events = list(type = "click", markname = "gene_text_2", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", markname = "gene_arc_2", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", markname = "gene_background_2", filter = "!event.shiftKey"), update = "null"),
                list(events = list(type = "click", filter = "!event.item && event.shiftKey"), update = "null")
            )
        )
    )
}
