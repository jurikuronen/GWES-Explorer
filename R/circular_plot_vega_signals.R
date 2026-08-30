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
        list(name = "tension", value = .settings$circular_plot_region_link_tension),
        list(name = "extent", value = .settings$circular_plot_circle_degrees),
        list(name = "rotate", value = .settings$circular_plot_rotation),
        list(name = "feature_label_text_size", value = .settings$circular_plot_feature_label_text_size),
        list(name = "text_size_region", value = .settings$circular_plot_region_group_label_text_size),
        list(name = "text_size_tooltip", value = .settings$circular_plot_feature_link_tooltip_text_size),
        list(name = "radius", value = .settings$circular_plot_radius),
        list(name = "feature_view_1_radius", value = .settings$circular_plot_feature_view_1_radius),
        list(name = "feature_view_2_radius", value = .settings$circular_plot_feature_view_2_radius),
        list(name = "feature_view_1_degrees", value = .settings$circular_plot_feature_view_1_degrees),
        list(name = "feature_view_2_degrees", value = .settings$circular_plot_feature_view_2_degrees),
        list(name = "feature_view_1_rotation", value = .settings$circular_plot_feature_view_1_rotation),
        list(name = "feature_view_2_rotation", value = .settings$circular_plot_feature_view_2_rotation),
        list(name = "feature_view_1_flip_inwards", value = .settings$circular_plot_feature_view_1_flip_inwards),
        list(name = "feature_view_2_flip_inwards", value = .settings$circular_plot_feature_view_2_flip_inwards),
        # Compute inner angle that has equal arc length.
        # list(name = "feature_view_2_degrees",
        #      update = "feature_view_1_degrees * (radius - radius_offset_1) / (radius - radius_offset_2)"),
        list(name = "show_region_links", value = TRUE),
        list(name = "show_position_links", value = TRUE)
    )
}

.circular_plot_signals_color <- function() {
    list(
        list(name = "feature_color", value = .settings$circular_plot_feature_color),
        list(name = "color_region_arc", value = .settings$circular_plot_region_color),
        list(name = "color_scheme_default", value = .settings$circular_plot_region_link_default_color_palette),
        list(name = "color_scheme_active", value = .settings$circular_plot_region_link_hovered_color_palette),
        list(name = "color_scheme_selected", value = .settings$circular_plot_region_link_active_color_palette),
        list(name = "color_scheme_inactive", value = .settings$circular_plot_region_link_inactive_color_palette)
    )
}

.circular_plot_signals_opacity <- function() {
    list(
        list(name = "opacity_region_link_adjustment", value = 1),
        list(name = "position_link_opacity_adjustment", value = 1),
        list(name = "opacity_background", value = .settings$circular_plot_background_opacity),
        list(name = "opacity_active", value = .settings$circular_plot_region_hovered_opacity),
        list(name = "opacity_connected", value = .settings$circular_plot_region_connected_opacity),
        list(name = "opacity_default", value = .settings$circular_plot_region_feature_default_opacity),
        list(name = "opacity_inactive", value = .settings$circular_plot_region_feature_inactive_opacity),
        list(name = "opacity_selected", value = .settings$circular_plot_region_feature_selected_opacity),
        list(name = "opacity_region_link_active",
             update = paste("opacity_region_link_adjustment *",
                            .settings$circular_plot_region_link_hovered_base_opacity)),
        list(name = "opacity_region_link_connected",
             update = paste("opacity_region_link_adjustment *",
                            .settings$circular_plot_region_link_active_base_opacity)),
        list(name = "opacity_region_link_default",
             update = paste("opacity_region_link_adjustment *", .settings$circular_plot_region_link_base_opacity)),
        list(name = "opacity_region_link_inactive",
             update = paste("opacity_region_link_adjustment *",
                            .settings$circular_plot_region_link_inactive_base_opacity)),
        list(name = "opacity_pos_link_connected",
             update = paste("position_link_opacity_adjustment *",
                            .settings$circular_plot_position_link_active_base_opacity)),
        list(name = "opacity_pos_link_default",
             update = paste("position_link_opacity_adjustment *",
                            .settings$circular_plot_position_link_base_opacity)),
        list(name = "opacity_pos_link_inactive",
             update = paste("position_link_opacity_adjustment *",
                            .settings$circular_plot_position_link_inactive_base_opacity)),
        list(name = "opacity_pos_link_selected",
             update = paste("position_link_opacity_adjustment *",
                            .settings$circular_plot_position_link_selected_base_opacity))
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
        # Mouseovered feature.
        list(
            name = "active_feature",
            value = NULL,
            on = list(
                list(events = "@feature_arc_1:mouseover", update = "datum.feature_row"),
                list(events = "@feature_arc_2:mouseover", update = "datum.feature_row"),
                list(events = "@feature_text_1:mouseover", update = "datum.feature_row"),
                list(events = "@feature_text_2:mouseover", update = "datum.feature_row"),
                list(events = "@feature_background_1:mouseover", update = "null"),
                list(events = "@feature_background_2:mouseover", update = "null"),
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
                list(events = list(type = "click", markname = "feature_text_1", filter = "!event.shiftKey"),
                     update = "datum.region"),
                list(events = list(type = "click", markname = "feature_arc_1", filter = "!event.shiftKey"),
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
                list(events = list(type = "click", markname = "feature_text_2", filter = "!event.shiftKey"),
                     update = "datum.region"),
                list(events = list(type = "click", markname = "feature_arc_2", filter = "!event.shiftKey"),
                     update = "datum.region"),
                list(events = list(type = "click", filter = "!event.item && event.shiftKey"),
                     update = "null")
            )
        ),

        # Selected features.
        list(
            name = "selected_feature_1",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "feature_text_1", filter = "!event.shiftKey"),
                     update = "datum.feature_row"),
                list(events = list(type = "click", markname = "feature_arc_1", filter = "!event.shiftKey"),
                     update = "datum.feature_row"),
                list(events = list(type = "click", markname = "feature_background_1", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", filter = "!event.item && !event.shiftKey"),
                     update = "null")
            )
        ),
        list(
            name = "selected_feature_2",
            value = NULL,
            on = list(
                list(events = list(type = "click", markname = "region_arc", filter = "event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "feature_text_2", filter = "!event.shiftKey"),
                     update = "datum.feature_row"),
                list(events = list(type = "click", markname = "feature_arc_2", filter = "!event.shiftKey"),
                     update = "datum.feature_row"),
                list(events = list(type = "click", markname = "feature_background_2", filter = "!event.shiftKey"),
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
                list(events = list(type = "click", markname = "feature_text_1", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "feature_arc_1", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "feature_background_1", filter = "!event.shiftKey"),
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
                list(events = list(type = "click", markname = "feature_text_2", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "feature_arc_2", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", markname = "feature_background_2", filter = "!event.shiftKey"),
                     update = "null"),
                list(events = list(type = "click", filter = "!event.item && event.shiftKey"),
                     update = "null")
            )
        )
    )
}
