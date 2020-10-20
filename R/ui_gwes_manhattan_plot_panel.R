.gwes_plot_panel <- function() {
    shiny::tabPanel("GWES Manhattan",
                    shiny::br(), shiny::br(), shiny::br(), shiny::br(), shiny::br(),
                    shiny::plotOutput("manhattan_plot",
                                      width = "30cm",
                                      height = "10cm",
                                      click = "manhattan_plot_click",
                                      brush = shiny::brushOpts(id = "manhattan_plot_brush", resetOnNew = TRUE),
                                      dblclick = "manhattan_plot_double_click"
                    ),
                    shiny::br(), shiny::br(),
                    shiny::h4("Select rows in the table on the left. The corresponding pairs are plotted in red in the scatter plot."),
                    shiny::h4("Select an area and double click in it to zoom in. Double click to zoom back out."),
                    shiny::h4("Click on (or near) a black point to see the corresponding data in the table below:"),
                    shiny::br(),
                    shiny::tableOutput("manhattan_plot_table")
    )
}
