# Creates controls for changing point sizes in the GWES Manhattan plot.
.gwes_plot_point_size_controls <- function() {
    # Show the point size options when the user selects "Point size".
    shiny::conditionalPanel(
        condition = "input.show_gwes_plot_point_size_settings",
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "gwes_direct_point_size",
                label = "Direct point size:",
                min = 0.05,
                max = 10,
                value = 1,
                step = 0.05
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "gwes_indirect_point_size",
                label = "Indirect point size:",
                min = 0.05,
                max = 10,
                value = 0.5,
                step = 0.05
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "gwes_highlight_size",
                label = "Highlight size:",
                min = 0.05,
                max = 30,
                value = 5,
                step = 0.1
            )
        )
    )
}

# Creates controls for changing text sizes in the GWES Manhattan plot.
.gwes_plot_text_size_controls <- function() {
    # Show the text size options when the user selects "Text size".
    shiny::conditionalPanel(
        condition = "input.show_gwes_plot_text_size_settings",
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "gwes_axis_text_size",
                label = "Axis text size:",
                min = 6,
                max = 50,
                value = 14,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "gwes_axis_title_size",
                label = "Axis title size:",
                min = 6,
                max = 50,
                value = 14,
                step = 1
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "gwes_legend_text_size",
                label = "Legend text size:",
                min = 6,
                max = 50,
                value = 14,
                step = 1
            )
        )
    )
}

# Creates controls for changing colors in the GWES Manhattan plot.
.gwes_plot_color_controls <- function() {
    # Show the color options when the user selects "Color".
    shiny::conditionalPanel(
        condition = "input.show_gwes_plot_color_settings",
        .div_inline_block(
            width_cm = 6,
            colourpicker::colourInput(
                inputId = "gwes_direct_point_color",
                label = "Direct point color:",
                value = "steelblue"
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 6,
            colourpicker::colourInput(
                inputId = "gwes_indirect_point_color",
                label = "Indirect point color:",
                value = "grey"
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 6,
            colourpicker::colourInput(
                inputId = "gwes_highlight_color",
                label = "Highlight color:",
                value = "red"
            )
        )
    )
}

# Creates the "GWES Manhattan" tab panel.
.gwes_plot_panel <- function() {
    shiny::tabPanel(
        title = "GWES Manhattan",
        shiny::br(),

        # Switches to open plot settings.
        .div_inline_block(
            width_cm = 3,
            content = "Modify figure:"
        ),
        .plot_settings_switch(
            input_id = "show_gwes_plot_point_size_settings",
            label = "Point size"
        ),
        .plot_settings_switch(
            input_id = "show_gwes_plot_text_size_settings",
            label = "Text size"
        ),
        .plot_settings_switch(
            input_id = "show_gwes_plot_color_settings",
            label = "Color"
        ),

        # Controls for the selected plot settings.
        .gwes_plot_point_size_controls(),
        .gwes_plot_text_size_controls(),
        .gwes_plot_color_controls(),
        shiny::br(),
        shiny::br(),

        # GWES Manhattan plot output.
        shiny::plotOutput(
            outputId = "manhattan_plot",
            width = "30cm",
            height = "10cm",
            click = "manhattan_plot_click",

            # Clear the brushed area after redrawing the plot so another double-click resets the zoom.
            brush = shiny::brushOpts(
                id = "manhattan_plot_brush",
                resetOnNew = TRUE
            ),
            dblclick = "manhattan_plot_double_click"
        ),
        shiny::br(),

        # Table of outliers near the selected direct point.
        shiny::tableOutput(outputId = "manhattan_plot_table"),
        shiny::br(),

        # Controls for downloading the plot.
        .plot_download_controls(
            prefix = "gwes_manhattan_plot",
            width = 30,
            height = 10
        ),
        shiny::br(),

        # Instructions for using the plot.
        shiny::p("Select rows in the table on the left to highlight the corresponding pairs in the plot."),
        shiny::p("Drag over an area and double-click to zoom in; double-click again to reset the zoom."),
        shiny::p("Click near a direct point to show nearby outliers in the table above.")
    )
}
