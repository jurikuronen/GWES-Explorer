.tree_plot_panel <- function() {
    shiny::tabPanel("Tree-MSA",
        shiny::br(),
        .div_inline_block(3, "Modify figure:"),
        .div_inline_block(4, .prettySwitch("show_tree_plot_fig_size", "Figure size")),
        .div_inline_block(4, .prettySwitch("show_tree_plot_label_prop", "Column labels")),
        .div_inline_block(4, .prettySwitch("show_tree_plot_legend_prop", "Legend")),
        shiny::conditionalPanel(
            condition = "input.show_tree_plot_fig_size",
            .div_inline_block(6, shiny::sliderInput("tree_plot_width", "Figure width (cm):", min = 10, max = 30, value = 20)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("tree_plot_height", "Figure height (cm):", min = 10, max = 30, value = 20))
        ),
        shiny::conditionalPanel(
            condition = "input.show_tree_plot_label_prop",
            .div_inline_block(6, shiny::sliderInput("tree_label_angle", "Column label angle:", min = 0, max = 90, value = 30)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("tree_label_fs", "Column label font size:", min = 2, max = 10, value = 5)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("tree_label_offset_x", "Column label offset X:", min = 0, max = 0.2, value = 0.1)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("tree_label_offset_y", "Column label offset Y:", min = 0, max = 200, value = 100))
        ),
        shiny::conditionalPanel(
            condition = "input.show_tree_plot_legend_prop",
            .div_inline_block(6, shiny::sliderInput("tree_legend_fs", "Legend font size:", min = 8, max = 20, value = 14)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("tree_legend_size", "Legend size:", min = 0.5, max = 2, value = 1.4))
        ),
        shiny::br(), shiny::br(),
        shiny::uiOutput("treeUI")
    )
}
