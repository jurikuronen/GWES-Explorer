# Create the UI definition of the Shiny app.
.create_ui_layout <- function() {
    shiny::fluidPage(
        # Set up the Shiny app to use shinyjs; required for the shinyjs::reset() calls to work.
        shinyjs::useShinyjs(),
        shiny::navbarPage(
            title = "GWES-Explorer",
            id = "tabs",
            .ui_about_tab_panel(),
            .ui_upload_data_tab_panel(),
            .ui_analyze_data_tab_panel()
        )
    )
}
