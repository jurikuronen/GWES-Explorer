# Creates all signals used by the circular plot's Vega specification.
.circular_plot_vega_signals <- function() {
    c(
        .circular_plot_vega_signals_layout(),
        .circular_plot_vega_signals_text(),
        .circular_plot_vega_signals_visibility(),
        .circular_plot_vega_signals_color(),
        .circular_plot_vega_signals_opacity(),
        .circular_plot_vega_signals_events()
    )
}

# Creates signals that control the layout of the circular plot.
.circular_plot_vega_signals_layout <- function() {
    list(
        list(name = "center_x", update = "width / 2"),
        list(name = "center_y", update = "height / 2"),
        list(name = "region_link_tension", value = .settings$circular_plot_region_link_tension),
        list(name = "circle_degrees", value = .settings$circular_plot_circle_degrees),
        list(name = "circle_rotation", value = .settings$circular_plot_rotation),
        list(name = "circle_radius", value = .settings$circular_plot_radius),
        list(name = "feature_view_1_radius", value = .settings$circular_plot_feature_view_1_radius),
        list(name = "feature_view_2_radius", value = .settings$circular_plot_feature_view_2_radius),
        list(name = "feature_view_1_degrees", value = .settings$circular_plot_feature_view_1_degrees),
        list(name = "feature_view_2_degrees", value = .settings$circular_plot_feature_view_2_degrees),
        list(name = "feature_view_1_rotation", value = .settings$circular_plot_feature_view_1_rotation),
        list(name = "feature_view_2_rotation", value = .settings$circular_plot_feature_view_2_rotation),
        list(name = "feature_view_1_flip_inwards", value = .settings$circular_plot_feature_view_1_flip_inwards),
        list(name = "feature_view_2_flip_inwards", value = .settings$circular_plot_feature_view_2_flip_inwards)
    )
}

# Creates signals that control text sizes for the circular plot.
.circular_plot_vega_signals_text <- function() {
    list(
        list(name = "feature_label_text_size", value = .settings$circular_plot_feature_label_text_size),
        list(name = "region_group_label_text_size", value = .settings$circular_plot_region_group_label_text_size),
        list(name = "feature_link_tooltip_text_size", value = .settings$circular_plot_feature_link_tooltip_text_size)
    )
}

# Creates signals that control link visibility for the circular plot.
.circular_plot_vega_signals_visibility <- function() {
    list(
        list(name = "show_region_links", value = TRUE),
        list(name = "show_position_links", value = TRUE)
    )
}

# Creates signals that control colors for the circular plot.
.circular_plot_vega_signals_color <- function() {
    list(
        list(name = "feature_color", value = .settings$circular_plot_feature_color),
        list(name = "region_color", value = .settings$circular_plot_region_color),
        list(name = "region_link_default_color_palette",
             value = .settings$circular_plot_region_link_default_color_palette),
        list(name = "region_link_hovered_color_palette",
             value = .settings$circular_plot_region_link_hovered_color_palette),
        list(name = "region_link_active_color_palette",
             value = .settings$circular_plot_region_link_active_color_palette),
        list(name = "region_link_inactive_color_palette",
             value = .settings$circular_plot_region_link_inactive_color_palette)
    )
}

# Creates signals that control opacity for the circular plot.
.circular_plot_vega_signals_opacity <- function() {
    list(
        # Backgrounds, regions, and features.
        list(name = "background_opacity", value = .settings$circular_plot_background_opacity),
        list(name = "region_hovered_opacity", value = .settings$circular_plot_region_hovered_opacity),
        list(name = "region_connected_opacity", value = .settings$circular_plot_region_connected_opacity),
        list(name = "region_feature_default_opacity", value = .settings$circular_plot_region_feature_default_opacity),
        list(name = "region_feature_inactive_opacity", value = .settings$circular_plot_region_feature_inactive_opacity),
        list(name = "region_feature_selected_opacity", value = .settings$circular_plot_region_feature_selected_opacity),

        # Region links.
        list(name = "region_link_opacity_adjustment", value = 1),
        list(name = "region_link_hovered_opacity",
             update = paste("region_link_opacity_adjustment *",
                            .settings$circular_plot_region_link_hovered_base_opacity)),
        list(name = "region_link_active_opacity",
             update = paste("region_link_opacity_adjustment *",
                            .settings$circular_plot_region_link_active_base_opacity)),
        list(name = "region_link_default_opacity",
             update = paste("region_link_opacity_adjustment *", .settings$circular_plot_region_link_base_opacity)),
        list(name = "region_link_inactive_opacity",
             update = paste("region_link_opacity_adjustment *",
                            .settings$circular_plot_region_link_inactive_base_opacity)),
        list(name = "region_link_selected_opacity",
             update = paste("region_link_opacity_adjustment *",
                            .settings$circular_plot_region_link_selected_base_opacity)),

        # Outlier position links.
        list(name = "position_link_opacity_adjustment", value = 1),
        list(name = "position_link_active_opacity",
             update = paste("position_link_opacity_adjustment *",
                            .settings$circular_plot_position_link_active_base_opacity)),
        list(name = "position_link_default_opacity",
             update = paste("position_link_opacity_adjustment *", .settings$circular_plot_position_link_base_opacity)),
        list(name = "position_link_inactive_opacity",
             update = paste("position_link_opacity_adjustment *",
                            .settings$circular_plot_position_link_inactive_base_opacity)),
        list(name = "position_link_selected_opacity",
             update = paste("position_link_opacity_adjustment *",
                            .settings$circular_plot_position_link_selected_base_opacity))
    )
}

# Creates signals that track hover and selection in the circular plot.
.circular_plot_vega_signals_events <- function() {
    list(
        # Hovered region ID.
        list(
            name = "hovered_region",
            value = NULL,
            on = list(
                list(events = "@region_arc:mouseover", update = "datum.id"),
                list(events = "mouseover[!event.item]", update = "null")
            )
        ),
        # Hovered feature's 1-based row.
        list(
            name = "hovered_feature_row",
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
        # Selected region IDs for feature views 1 and 2.
        # Clicking (or shift-clicking) an empty area of the plot clears view 1 (or 2).
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

        # Selected feature rows.
        # Clicking the feature view background clears the feature selection.
        list(
            name = "selected_feature_row_1",
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
            name = "selected_feature_row_2",
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

        # Selected positions.
        # TODO: remove; these signals are unused.
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
