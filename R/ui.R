# Creates the application's top-level browser interface.
.create_ui_layout <- function() {
    shiny::fluidPage(
        # Set up the Shiny app to use shinyjs; required for the shinyjs::reset() calls to work.
        shinyjs::useShinyjs(),
        # Display the application's main sections as tabs in a navigation bar at the top of the page.
        shiny::navbarPage(
            title = "GWES-Explorer",
            .ui_about_tab_panel(),
            .ui_upload_data_tab_panel(),
            .ui_explore_results_tab_panel()
        )
    )
}
