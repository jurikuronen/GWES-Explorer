# Create the UI definition of the Shiny app.
.create_ui_layout <- function() {
    shiny::fluidPage(
        shiny::navbarPage(
            title = "GWES-Explorer",
            id = "tabs",
            .ui_about_tab_panel(),
            .ui_upload_data_tab_panel(),
            .ui_analyse_data_tab_panel()
        )
    )
}
