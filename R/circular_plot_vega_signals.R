.circular_plot_vega_signals <- function(extent, rotate, colorScheme, colorSchemeSelected, colorSchemeSelected2) {
    signals <- .main_parameter_signals(extent, rotate)
    signals <- append(signals, .color_scheme_signals(colorScheme, colorSchemeSelected, colorSchemeSelected2))
    signals <- append(signals, .event_listener_signals())
}

.main_parameter_signals <- function(extent, rotate) {
    list(
        list(name = "tension", value = .get_cp_tension()),
        list(name = "radius", value = .get_cp_radius()),
        list(name = "extent", value = extent),
        list(name = "radius_genes_1", value = .get_cp_radius() - .get_cp_radius_offset(1)),
        list(name = "radius_genes_2", value = .get_cp_radius() - .get_cp_radius_offset(2)),
        list(name = "rotate", value = rotate),
        list(name = "textSize", value = .get_cp_text_size()),
        list(name = "centerTextSize", value = .get_cp_center_text_size()),
        list(name = "innerTextSize", value = .get_cp_small_text_size()),
        list(name = "origoX", update = "width / 2"),
        list(name = "origoY", update = "height / 2")
    )
}

.color_scheme_signals <- function(colorScheme, colorSchemeSelected, colorSchemeSelected2) {
    list(
        list(name = "colorScheme", value = colorScheme),
        list(name = "colorSchemeSelected", value = colorSchemeSelected),
        list(name = "colorSchemeSelected2", value = colorSchemeSelected2)
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
                # list(events = list(type = "click", markname = "selected_gene_text_1", filter = "!event.shiftKey"), update = "datum.region"),
                # list(events = list(type = "click", markname = "selected_gene_text_2", filter = "!event.shiftKey"), update = "datum.region"),
                # list(events = list(type = "click", markname = "selected_position_text_1", filter = "!event.shiftKey"), update = "datum.target_region"),
                # list(events = list(type = "click", markname = "selected_position_text_2", filter = "!event.shiftKey"), update = "datum.target_region"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_1", filter = "!event.shiftKey"), update = "datum.target_region"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_2", filter = "!event.shiftKey"), update = "datum.target_region"),
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
                # list(events = list(type = "click", markname = "selected_gene_text_1", filter = "event.shiftKey"), update = "datum.region"),
                # list(events = list(type = "click", markname = "selected_gene_text_2", filter = "event.shiftKey"), update = "datum.region"),
                # list(events = list(type = "click", markname = "selected_position_text_1", filter = "event.shiftKey"), update = "datum.target_region"),
                # list(events = list(type = "click", markname = "selected_position_text_2", filter = "event.shiftKey"), update = "datum.target_region"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_1", filter = "event.shiftKey"), update = "datum.target_region"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_2", filter = "event.shiftKey"), update = "datum.target_region"),
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
                # list(events = list(type = "click", markname = "selected_gene_text_1", filter = "!event.shiftKey"), update = "datum.parent"),
                # list(events = list(type = "click", markname = "selected_gene_text_2", filter = "!event.shiftKey"), update = "datum.parent"),
                # list(events = list(type = "click", markname = "selected_position_text_1", filter = "!event.shiftKey"), update = "datum.target_gene"),
                # list(events = list(type = "click", markname = "selected_position_text_2", filter = "!event.shiftKey"), update = "datum.target_gene"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_1", filter = "!event.shiftKey"), update = "datum.target_gene"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_2", filter = "!event.shiftKey"), update = "datum.target_gene"),
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
                # list(events = list(type = "click", markname = "selected_gene_text_1", filter = "event.shiftKey"), update = "datum.parent"),
                # list(events = list(type = "click", markname = "selected_gene_text_2", filter = "event.shiftKey"), update = "datum.parent"),
                # list(events = list(type = "click", markname = "selected_position_text_1", filter = "event.shiftKey"), update = "datum.target_gene"),
                # list(events = list(type = "click", markname = "selected_position_text_2", filter = "event.shiftKey"), update = "datum.target_gene"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_1", filter = "event.shiftKey"), update = "datum.target_gene"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_2", filter = "event.shiftKey"), update = "datum.target_gene"),
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
                # list(events = list(type = "click", markname = "selected_gene_text_1", filter = "!event.shiftKey"), update = "datum.name"),
                # list(events = list(type = "click", markname = "selected_gene_text_2", filter = "!event.shiftKey"), update = "datum.name"),
                # list(events = list(type = "click", markname = "selected_position_text_1", filter = "!event.shiftKey"), update = "datum.target"),
                # list(events = list(type = "click", markname = "selected_position_text_2", filter = "!event.shiftKey"), update = "datum.target"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_1", filter = "!event.shiftKey"), update = "null"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_2", filter = "!event.shiftKey"), update = "null"),
                # list(events = list(type = "click", markname = "selected_position_main_gene_text_1", filter = "!event.shiftKey"), update = "null"),
                # list(events = list(type = "click", markname = "selected_position_main_gene_text_2", filter = "!event.shiftKey"), update = "null"),
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
                # list(events = list(type = "click", markname = "selected_gene_text_1", filter = "event.shiftKey"), update = "datum.name"),
                # list(events = list(type = "click", markname = "selected_gene_text_2", filter = "event.shiftKey"), update = "datum.name"),
                # list(events = list(type = "click", markname = "selected_position_text_1", filter = "event.shiftKey"), update = "datum.target"),
                # list(events = list(type = "click", markname = "selected_position_text_2", filter = "event.shiftKey"), update = "datum.target"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_1", filter = "event.shiftKey"), update = "null"),
                # list(events = list(type = "click", markname = "selected_position_gene_text_2", filter = "event.shiftKey"), update = "null"),
                # list(events = list(type = "click", markname = "selected_position_main_gene_text_1", filter = "event.shiftKey"), update = "null"),
                # list(events = list(type = "click", markname = "selected_position_main_gene_text_2", filter = "event.shiftKey"), update = "null"),
                list(events = list(type = "click", filter = "!event.item && event.shiftKey"), update = "null")
            )
        )
    )
}
