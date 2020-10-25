.create_ui <- function() {
    # Define UI for application
    shiny::fluidPage(
        # Page with several tabs
        shiny::navbarPage(title = "GWESalyzer",
           id = "tabs",
           .about_panel(),
           shiny::tabPanel(title = "Upload data", .data_sidebar_layout()),
           shiny::tabPanel(title = "Analyse data", .plot_sidebar_layout())
        )
    )
}
