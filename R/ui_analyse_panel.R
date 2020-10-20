.plot_sidebar_layout <- function() {
    shiny::sidebarLayout(
        # Sidebar panel - a DT table listing the direct outliers
        shiny::sidebarPanel(
            # Select phenotype
            shiny::selectInput(
                inputId = "select_phenotype",
                label = "Select phenotype:",
                choices = c("No phenotype selected")
            ),
            # Input - select one or multiple rows
            shiny::radioButtons(
                inputId = "select_row_type",
                label = "Select single or multiple rows:",
                choices = c(Single = "single", Multiple = "multiple"),
                selected = "single",
                inline = TRUE
            ),
            shiny::div(DT::DTOutput("outliers_table"), style = "font-size:70%")
        ),

        # Main panel - various plots shown in sub-panels
        shiny::mainPanel(
            shiny::tabsetPanel(
                type = "tabs",
                .gwes_plot_panel(),
                .tree_plot_panel(),
                .circular_plot_panel()
            )
        )
    )
}
