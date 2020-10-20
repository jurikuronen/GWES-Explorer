# Build fails if these are not called here. I have no idea why.
source("R/ui_about_panel.R")
source("R/ui_analyse_panel.R")
source("R/ui_circular_plot_panel.R")
source("R/ui_data_panel.R")
source("R/ui_data_sidebar.R")
source("R/ui_gwes_manhattan_plot_panel.R")
source("R/ui_phylogenetic_tree_plot_panel.R")
source("R/ui_utils.R")

# Define UI for application
.ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),

    # Page with several tabs
    shiny::navbarPage(title = "GWESalyzer",
       id = "tabs",
       .about_panel(),
       shiny::tabPanel(title = "Upload data", .data_sidebar_layout()),
       shiny::tabPanel(title = "Analyse SpydrPick output", .plot_sidebar_layout())
    )
)
