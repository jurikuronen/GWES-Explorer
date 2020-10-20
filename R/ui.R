.create_ui <- function() {
    # Define UI for application
    shiny::fluidPage(
        shinyjs::useShinyjs(),

        # Page with several tabs
        shiny::navbarPage(title = "GWESalyzer",
           id = "tabs",
           .about_panel(),
           shiny::tabPanel(title = "Upload data", .data_sidebar_layout()),
           shiny::tabPanel(title = "Analyse SpydrPick output", .plot_sidebar_layout())
        )
    )
}
