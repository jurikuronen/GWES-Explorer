.circular_plot_panel <- function() {
    shiny::tabPanel("Circular plot",
                    shiny::br(), shiny::br(),
                    vegawidget::vegawidgetOutput("circular_plot"),
                    shiny::br(), shiny::br(),
                    shiny::h4("Click a region to open gene view. Shift-clicking another region opens another gene view for comparison."),
                    shiny::h4("To hide gene views, click outside the gene view areas.")
    )
}
