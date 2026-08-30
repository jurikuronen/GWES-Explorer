# Creates controls for adjusting the circular plot layout.
.circular_plot_layout_controls <- function() {
    # Show the layout options when the user selects "Layout".
    shiny::conditionalPanel(
        condition = "input.show_circular_plot_layout_settings",
        .div_inline_block(
            width_cm = 5,
            shiny::sliderInput(
                inputId = "circular_plot_radius",
                label = "Circular plot radius:",
                min = 100,
                max = .settings$circular_plot_radius,
                value = .settings$circular_plot_radius,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 5,
            shiny::sliderInput(
                inputId = "circular_plot_rotate",
                label = "Circular plot rotation:",
                min = 0,
                max = 360,
                value = .settings$circular_plot_rotation,
                step = 1
            )
        ),
        shiny::br(),
        .div_inline_block(
            width_cm = 5,
            shiny::sliderInput(
                inputId = "circular_plot_feature_view_1_radius",
                label = "Feature view 1 radius:",
                min = 0,
                max = 400,
                value = .settings$circular_plot_feature_view_1_radius,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 5,
            shiny::sliderInput(
                inputId = "circular_plot_feature_view_1_degrees",
                label = "Feature view 1 angular span:",
                min = 15,
                max = 135,
                value = .settings$circular_plot_feature_view_1_degrees,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 5,
            shiny::sliderInput(
                inputId = "circular_plot_feature_view_1_rotation",
                label = "Feature view 1 rotation:",
                min = 0,
                max = 360,
                value = .settings$circular_plot_feature_view_1_rotation,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 4,
            shiny::checkboxInput(
                inputId = "circular_plot_feature_view_1_flip_inwards",
                label = "Flip feature view 1 labels inward:",
                value = .settings$circular_plot_feature_view_1_flip_inwards
            )
        ),
        shiny::br(),
        .div_inline_block(
            width_cm = 5,
            shiny::sliderInput(
                inputId = "circular_plot_feature_view_2_radius",
                label = "Feature view 2 radius:",
                min = 0,
                max = 400,
                value = .settings$circular_plot_feature_view_2_radius,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 5,
            shiny::sliderInput(
                inputId = "circular_plot_feature_view_2_degrees",
                label = "Feature view 2 angular span:",
                min = 15,
                max = 135,
                value = .settings$circular_plot_feature_view_2_degrees,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 5,
            shiny::sliderInput(
                inputId = "circular_plot_feature_view_2_rotation",
                label = "Feature view 2 rotation:",
                min = 0,
                max = 360,
                value = .settings$circular_plot_feature_view_2_rotation,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 4,
            shiny::checkboxInput(
                inputId = "circular_plot_feature_view_2_flip_inwards",
                label = "Flip feature view 2 labels inward:",
                value = .settings$circular_plot_feature_view_2_flip_inwards
            )
        )
    )
}

# Creates controls for changing text sizes in the circular plot.
.circular_plot_text_size_controls <- function() {
    # Show the text size options when the user selects "Text size".
    shiny::conditionalPanel(
        condition = "input.show_circular_plot_text_size_settings",
        .div_inline_block(
            width_cm = 4,
            shiny::sliderInput(
                inputId = "circular_plot_text_size_region",
                label = "Region label size:",
                min = 6,
                max = 25,
                value = .settings$circular_plot_region_group_label_text_size,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 4,
            shiny::sliderInput(
                inputId = "circular_plot_feature_label_text_size",
                label = "Genomic-feature label size:",
                min = 6,
                max = 20,
                value = .settings$circular_plot_feature_label_text_size,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 4,
            shiny::sliderInput(
                inputId = "circular_plot_text_size_tooltip",
                label = "Tooltip text size:",
                min = 6,
                max = 20,
                value = .settings$circular_plot_feature_link_tooltip_text_size,
                step = 1
            )
        )
    )
}

# Creates controls for changing link visibility in the circular plot.
.circular_plot_visibility_controls <- function() {
    # Show the visibility options when the user selects "Visibility".
    shiny::conditionalPanel(
        condition = "input.show_circular_plot_visibility_settings",
        .div_inline_block(
            width_cm = 4,
            shiny::sliderInput(
                inputId = "circular_plot_opacity_region_link_adjustment",
                label = "Region link opacity:",
                min = 0,
                max = 1,
                value = 1,
                step = 0.01
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 4,
            shiny::sliderInput(
                inputId = "circular_plot_position_link_opacity_adjustment",
                label = "Position link opacity:",
                min = 0,
                max = 1,
                value = 1,
                step = 0.01
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 4,
            shiny::checkboxInput(
                inputId = "circular_plot_show_region_links",
                label = "Show region links:",
                value = TRUE
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 4,
            shiny::checkboxInput(
                inputId = "circular_plot_show_position_links",
                label = "Show position links:",
                value = TRUE
            )
        )
    )
}

# Creates the "Circular plot" tab panel.
.circular_plot_panel <- function() {
    shiny::tabPanel(
        title = "Circular plot",
        shiny::br(),

        # Switches to open plot settings.
        .div_inline_block(
            width_cm = 3,
            content = "Modify figure:"
        ),
        .plot_settings_switch(
            input_id = "show_circular_plot_layout_settings",
            label = "Layout"
        ),
        .plot_settings_switch(
            input_id = "show_circular_plot_text_size_settings",
            label = "Text size"
        ),
        .plot_settings_switch(
            input_id = "show_circular_plot_visibility_settings",
            label = "Visibility"
        ),

        # Controls for the selected plot settings.
        .circular_plot_layout_controls(),
        .circular_plot_text_size_controls(),
        .circular_plot_visibility_controls(),
        shiny::br(),
        shiny::br(),

        # Circular plot output.
        vegawidget::vegawidgetOutput(outputId = "circular_plot"),
        shiny::br(),
        shiny::br(),

        # Instructions for using the plot.
        shiny::p(
            "Click a region to open a feature view. Shift-click a region to open a second feature view for comparison."
        ),
        shiny::p(
            paste(
                "Click an empty area of the plot to close the first feature view. Shift-click an empty area to close",
                "the second."
            )
        )
    )
}
