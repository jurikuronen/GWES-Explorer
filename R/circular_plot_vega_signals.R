.circular_plot_vega_signals <- function() {
    signals <- .circular_plot_signals_main()
    signals <- append(signals, .circular_plot_signals_color())
    signals <- append(signals, .circular_plot_signals_opacity())
    signals <- append(signals, .circular_plot_signals_events())
}

.circular_plot_signals_main <- function() {
    list(
        list(name = "origoX", update = "width / 2"),
        list(name = "origoY", update = "height / 2"),
        list(name = "tension", value = .circular_plot_tension()),
        list(name = "extent", value = .circular_plot_extent()),
        list(name = "rotate", value = .circular_plot_rotate()),
        list(name = "text_size_gene", value = .circular_plot_text_size_gene()),
        list(name = "text_size_region", value = .circular_plot_text_size_region()),
        list(name = "text_size_tooltip", value = .circular_plot_text_size_tooltip()),
        list(name = "radius", value = .circular_plot_radius()),
        list(name = "radius_gene_view_1", value = .circular_plot_radius_gene_view_1()),
        list(name = "radius_gene_view_2", value = .circular_plot_radius_gene_view_2()),
        list(name = "gene_arc_angle_1", value = .circular_plot_gene_arc_angle_1()),
        list(name = "gene_arc_angle_2", value = .circular_plot_gene_arc_angle_2()),
        list(name = "rotate_gene_view_1", value = .circular_plot_rotate_gene_view_1()),
        list(name = "rotate_gene_view_2", value = .circular_plot_rotate_gene_view_2()),
        list(name = "flip_gene_view_1", value = .circular_plot_flip_gene_view_1()),
        list(name = "flip_gene_view_2", value = .circular_plot_flip_gene_view_2()),
        # Compute inner angle that has equal arc length.
        # list(name = "gene_arc_angle_2",
        #      update = "gene_arc_angle_1 * (radius - radius_offset_1) / (radius - radius_offset_2)"),
        list(name = "show_region_links", value = TRUE),
        list(name = "show_gene_links", value = TRUE)
    )
}

.circular_plot_signals_color <- function() {
    list(
        list(name = "color_gene_arc", value = .circular_plot_color_gene_arc()),
        list(name = "color_region_arc", value = .circular_plot_color_region_arc()),
        list(name = "color_scheme_default", value = .circular_plot_color_scheme_default()),
        list(name = "color_scheme_active", value = .circular_plot_color_scheme_active()),
        list(name = "color_scheme_selected", value = .circular_plot_color_scheme_selected()),
        list(name = "color_scheme_inactive", value = .circular_plot_color_scheme_inactive())
    )
}

.circular_plot_signals_opacity <- function() {
    list(
        list(name = "opacity_region_link_adjustment", value = 1),
        list(name = "opacity_gene_link_adjustment", value = 1),
        list(name = "opacity_background", value = .circular_plot_opacity_background()),
        list(name = "opacity_active", value = .circular_plot_opacity_active()),
        list(name = "opacity_connected", value = .circular_plot_opacity_connected()),
        list(name = "opacity_default", value = .circular_plot_opacity_default()),
        list(name = "opacity_inactive", value = .circular_plot_opacity_inactive()),
        list(name = "opacity_selected", value = .circular_plot_opacity_selected()),
        list(name = "opacity_region_link_active",
             update = paste("opacity_region_link_adjustment *", .circular_plot_opacity_region_link_active())),
        list(name = "opacity_region_link_connected",
             update = paste("opacity_region_link_adjustment *", .circular_plot_opacity_region_link_connected())),
        list(name = "opacity_region_link_default",
             update = paste("opacity_region_link_adjustment *", .circular_plot_opacity_region_link_default())),
        list(name = "opacity_region_link_inactive",
             update = paste("opacity_region_link_adjustment *", .circular_plot_opacity_region_link_inactive())),
        list(name = "opacity_pos_link_connected",
             update = paste("opacity_gene_link_adjustment *", .circular_plot_opacity_pos_link_connected())),
        list(name = "opacity_pos_link_default",
             update = paste("opacity_gene_link_adjustment *", .circular_plot_opacity_pos_link_default())),
        list(name = "opacity_pos_link_inactive",
             update = paste("opacity_gene_link_adjustment *", .circular_plot_opacity_pos_link_inactive())),
        list(name = "opacity_pos_link_selected",
             update = paste("opacity_gene_link_adjustment *", .circular_plot_opacity_pos_link_selected()))
    )
}

.circular_plot_signals_events <- function() {
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
                list(events = list(type = "click", markname = "region_arc", filter = "!event.shiftKey"),
                     update = "datum.id"),
                list(events = list(type = "click", markname = "gene_text_1", filter = "!event.shiftKey"),
                     update = "datum.region"),
                list(events = list(type = "click", markname = "gene_arc_1", filter = "!event.shiftKey"),
                     update = "datum.region"),
                list(events = list(type = "click", filter = "!event.item && !event.shiftKey"),
                     update = "null")
            )
        ),
        list(
            name = "selected_region_2",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "event.shiftKey"),
                     update = "datum.id"),
                list(events = list(type = "click", markname = "gene_text_2", filter = "!event.shiftKey"),
                     update = "datum.region"),
                list(events = list(type = "click", markname = "gene_arc_2", filter = "!event.shiftKey"),
                     update = "datum.region"),
                list(events = list(type = "click", filter = "!event.item && event.shiftKey"),
                     update = "null")
            )
        ),

        # Selected genes.
        list(
            name = "selected_gene_1",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "gene_text_1", filter = "!event.shiftKey"),
                     update = "datum.id"),
                list(events = list(type = "click", markname = "gene_arc_1", filter = "!event.shiftKey"),
                     update = "datum.id"),
                list(events = list(type = "click", markname = "gene_background_1", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", filter = "!event.item && !event.shiftKey"),
                     update = "null")
            )
        ),
        list(
            name = "selected_gene_2",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "gene_text_2", filter = "!event.shiftKey"),
                     update = "datum.id"),
                list(events = list(type = "click", markname = "gene_arc_2", filter = "!event.shiftKey"),
                     update = "datum.id"),
                list(events = list(type = "click", markname = "gene_background_2", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", filter = "!event.item && event.shiftKey"),
                     update = "null")
            )
        ),

        # Selected position.
        list(
            name = "selected_position_1",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "gene_text_1", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "gene_arc_1", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "gene_background_1", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", filter = "!event.item && !event.shiftKey"),
                     update = "null")
            )
        ),
        list(
            name = "selected_position_2",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "gene_text_2", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "gene_arc_2", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "gene_background_2", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", filter = "!event.item && event.shiftKey"),
                     update = "null")
            )
        )
    )
}
