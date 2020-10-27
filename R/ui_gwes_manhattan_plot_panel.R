.gwes_plot_panel <- function() {
    shiny::tabPanel("GWES Manhattan",
                    shiny::br(),
                    .div_inline_block(3, "Modify figure:"),
                    .div_inline_block(4, .prettySwitch("show_gwes_plot_marker_size", "Marker size")),
                    .div_inline_block(4, .prettySwitch("show_gwes_plot_text_size", "Text size")),
                    .div_inline_block(4, .prettySwitch("show_gwes_plot_colors", "Color")),
                    shiny::conditionalPanel(
                        condition = "input.show_gwes_plot_marker_size",
                        .div_inline_block(6, shiny::sliderInput("gwes_size_direct", "Direct point size:", min = 0.05, max = 5, value = 1, step = 0.05)),
                        .div_inline_br_block(0.5),
                        .div_inline_block(6, shiny::sliderInput("gwes_size_indirect", "Indirect point size:", min = 0.05, max = 5, value = 0.5, step = 0.05)),
                        .div_inline_br_block(0.5),
                        .div_inline_block(6, shiny::sliderInput("gwes_selection_size", "Selection size:", min = 0.05, max = 20, value = 5, step = 0.1)),
                    ),
                    shiny::conditionalPanel(
                        condition = "input.show_gwes_plot_text_size",
                        .div_inline_block(6, shiny::sliderInput("gwes_axis_size", "Axis text size:", min = 6, max = 30, value = 14, step = 1)),
                        .div_inline_br_block(0.5),
                        .div_inline_block(6, shiny::sliderInput("gwes_title_size", "Axis title size:", min = 6, max = 30, value = 14, step = 1)),
                        .div_inline_br_block(0.5),
                        .div_inline_block(6, shiny::sliderInput("gwes_legend_size", "Legend size:", min = 6, max = 30, value = 14, step = 1))
                    ),
                    shiny::conditionalPanel(
                        condition = "input.show_gwes_plot_colors",
                        .div_inline_block(6, colourpicker::colourInput("gwes_color_direct", "Direct color:", value = "steelblue")),
                        .div_inline_br_block(0.5),
                        .div_inline_block(6, colourpicker::colourInput("gwes_color_indirect", "Indirect color:", value = "grey")),
                        .div_inline_br_block(0.5),
                        .div_inline_block(6, colourpicker::colourInput("gwes_selection_color", "Selection color:", value = "red")),
                        .div_inline_br_block(0.5),
                    ),
                    shiny::br(), shiny::br(),
                    shiny::plotOutput("manhattan_plot",
                                      width = "30cm",
                                      height = "10cm",
                                      click = "manhattan_plot_click",
                                      brush = shiny::brushOpts(id = "manhattan_plot_brush", resetOnNew = TRUE),
                                      dblclick = "manhattan_plot_double_click"
                    ),
                    shiny::br(),
                    shiny::tableOutput("manhattan_plot_table"),
                    shiny::br(),
                    shiny::h4("Select rows in the table on the left. The corresponding pairs are highlighted in the scatter plot."),
                    shiny::h4("Select an area and double click in it to zoom in. Double click to zoom back out."),
                    shiny::h4("Click on (or near) a direct point to aggregate the corresponding data into the table above.")
    )
}
