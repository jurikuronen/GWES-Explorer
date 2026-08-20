# Creates controls for changing the displayed Tree-MSA plot size.
.tree_plot_size_controls <- function() {
    # Show the figure size options when the user selects "Figure size".
    shiny::conditionalPanel(
        condition = "input.show_tree_plot_size_settings",
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "tree_plot_width",
                label = "Figure width (cm):",
                min = 10,
                max = 50,
                value = 20,
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
                inputId = "tree_plot_height",
                label = "Figure height (cm):",
                min = 10,
                max = 50,
                value = 20,
                step = 1
            )
        )
    )
}

# Creates controls for adjusting the Tree-MSA column labels.
.tree_plot_column_label_controls <- function() {
    # Show the column label options when the user selects "Column labels".
    shiny::conditionalPanel(
        condition = "input.show_tree_plot_column_label_settings",
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "tree_heatmap_column_label_angle",
                label = "Column label angle:",
                min = 0,
                max = 90,
                value = .settings$tree_heatmap_column_label_angle,
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
                inputId = "tree_heatmap_column_label_font_size",
                label = "Column label font size:",
                min = 2,
                max = 30,
                value = .settings$tree_heatmap_column_label_font_size,
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
                inputId = "tree_heatmap_column_label_offset_x",
                label = "Column label shift left:",
                min = 0,
                max = 0.2,
                value = .settings$tree_heatmap_column_label_offset_x,
                step = 0.01
            )
        ),
        .div_inline_block(
            width_cm = 0.5,
            content = NULL
        ),
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "tree_heatmap_column_label_offset_y",
                label = "Column label shift down:",
                min = 0,
                max = 200,
                value = .settings$tree_heatmap_column_label_offset_y,
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
                inputId = "tree_plot_bottom_margin_multiplier",
                label = "Column label bottom margin:",
                min = 1,
                max = 20,
                value = .settings$tree_plot_bottom_margin_multiplier,
                step = 1
            )
        )
    )
}

# Creates controls for adjusting the Tree-MSA legend.
.tree_plot_legend_controls <- function() {
    # Show the legend options when the user selects "Legend".
    shiny::conditionalPanel(
        condition = "input.show_tree_plot_legend_settings",
        .div_inline_block(
            width_cm = 6,
            shiny::sliderInput(
                inputId = "tree_legend_text_size",
                label = "Legend font size:",
                min = 8,
                max = 30,
                value = .settings$tree_legend_text_size,
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
                inputId = "tree_legend_key_size",
                label = "Legend key size:",
                min = 0.5,
                max = 3,
                value = .settings$tree_legend_key_size,
                step = 0.1
            )
        )
    )
}

# Creates the "Tree-MSA" tab panel.
.tree_plot_panel <- function() {
    shiny::tabPanel(
        title = "Tree-MSA",
        shiny::br(),

        # Switches to open plot settings.
        .div_inline_block(
            width_cm = 3,
            content = "Modify figure:"
        ),
        .plot_settings_switch(
            input_id = "show_tree_plot_size_settings",
            label = "Figure size"
        ),
        .plot_settings_switch(
            input_id = "show_tree_plot_column_label_settings",
            label = "Column labels"
        ),
        .plot_settings_switch(
            input_id = "show_tree_plot_legend_settings",
            label = "Legend"
        ),

        # Controls for the selected plot settings.
        .tree_plot_size_controls(),
        .tree_plot_column_label_controls(),
        .tree_plot_legend_controls(),
        shiny::br(),
        shiny::br(),

        # Tree-MSA plot output.
        shiny::uiOutput(outputId = "tree_plot_ui_output"),
        shiny::br(),

        # Controls for downloading the plot.
        .plot_download_controls(
            prefix = "phylogenetic_tree_plot",
            width = 15,
            height = 15
        )
    )
}
