# Create the UI definition of the Shiny app.
.create_ui_layout <- function() {
    version <- as.character(utils::packageVersion("GWESExplorer"))

    shinyjs::useShinyjs()
    shiny::fluidPage(
        shiny::navbarPage(
            title = paste0("GWES-Explorer v", version),
            id = "tabs",
            .ui_about_tab_panel(version),
            .ui_upload_data_tab_panel(),
            .ui_analyse_data_tab_panel()
        )
    )
}
