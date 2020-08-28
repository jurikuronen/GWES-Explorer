.data_sidebar_layout <- function() {
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::tags$p("All plots require the SpydrPick outliers file."),
            shiny::fileInput(
                inputId = "file_outliers",
                label = "SpydrPick outliers file (.outliers):",
                accept = ".outliers"
            ),
            shiny::tags$hr(),

            shiny::tags$p("Tree plot requires tree, fasta and loci files. Optionally, you can upload a phenotypic data file."),
            shiny::fileInput(
                inputId = "file_tree",
                label = "Tree file (Newick [.nwk] or Nexus [.nex]):",
                accept = c(".nwk",".nex")
            ),
            shiny::fileInput(
                inputId = "file_fasta",
                label = "Fasta file (.fasta, .fa or .aln):",
                accept = c(".fasta", ".fa", ".aln")
            ),
            shiny::fileInput(
                inputId = "file_loci",
                label = "Loci file (.loci):",
                accept = ".loci"
            ),
            shiny::fileInput(
                inputId = "file_phenotype",
                label = "Phenotypic data file (.csv, .txt):",
                accept = c(".csv", ".txt")
            ),
            shiny::tags$hr(),

            shiny::tags$p("Circular plot requires a gff file."),
            # Input - Upload gff file:
            shiny::fileInput(
                inputId = "file_gff",
                label = "Gff file (.gff3):",
                accept = ".gff3"
            ),
            shiny::tags$hr(),

            shiny::actionButton(inputId = "read_data_button", label = "Read in data"),
            shiny::div(style="display: inline-block; vertical-align:top; width: 100px;", shiny::br()),

            shiny::div(
                style = "display: inline-block; vertical-align:top; width: 150px;",
                shinyjs::hidden(shiny::p(id = "reading_data_div", shiny::textOutput("data_loaded")))
            )

        ),

        shiny::mainPanel(
            shiny::h4("Upload files in the correct formats. Description of correct formats comes here. GFF should be a GFF3 format with fields 'type', 'start' and 'end' specified. Further, there should be an 'attributes' field with a 'Name' tag.")
        )
    )
}

.settings_sidebar_layout <- function() {
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::tags$p("Circular plot: Plot size in pixels."),
            shiny::sliderInput("circular_plot_size", "Size", 500, 1200,
                               value = .settings$circular_plot_size, step = 10),
            shiny::tags$p("Circular plot: Split chromosome into this many groups."),
            shiny::sliderInput("circular_plot_n_groups", "Number of groups", 8, 24,
                               value = .settings$circular_plot_n_groups, step = 1),
            shiny::tags$p("Circular plot: Have each group further contain this many regions."),
            shiny::sliderInput("circular_plot_n_regions", "Number of regions per group", 8, 24,
                               value = .settings$circular_plot_n_regions, step = 1),
            shiny::tags$p("Circular plot: Edge tension parameter."),
            shiny::sliderInput("circular_plot_tension", "Tension", 0, 1, value = 0.7, step = 0.05),

            shiny::tags$hr(),

            shiny::actionButton(inputId = "apply_settings_button", label = "Apply settings"),
            shiny::div(style="display: inline-block; vertical-align:top; width: 100px;", shiny::br()),

            shiny::div(
                style = "display: inline-block; vertical-align:top; width: 150px;",
                shinyjs::hidden(shiny::p(id = "settings_div", shiny::textOutput("settings_applied")))
            )
        ),
        shiny::mainPanel(
            shiny::h4("Settings for plots.")
        )
    )
}

.div_inline_block <- function(width, content) {
    shiny::div(style = paste("display: inline-block; vertical-align: top; width:", width, "cm"), content)
}

.div_inline_br_block <- function(width) { .div_inline_block(width, shiny::br()) }
.prettySwitch <- function(id, name) { shinyWidgets::prettySwitch(id, name, fill = TRUE, status = "primary", value = FALSE) }

.tree_plot_panel <- function() {
    shiny::tabPanel("Tree-MSA",
        .div_inline_block(3, "Modify figure:"),
        .div_inline_block(4, .prettySwitch("show_fig_size", "Figure size")),
        .div_inline_block(4, .prettySwitch("show_label_prop", "Column labels")),
        .div_inline_block(4, .prettySwitch("show_legend_prop", "Legend")),
        shiny::conditionalPanel(
            condition = "input.show_fig_size",
            .div_inline_block(6, shiny::sliderInput("width", "Figure width (cm):", min = 10, max = 30, value = 20)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("height", "Figure height (cm):", min = 10, max = 30, value = 20))
        ),
        shiny::conditionalPanel(
            condition = "input.show_label_prop",
            .div_inline_block(6, shiny::sliderInput("tree_label_angle", "Column label angle:", min = 0, max = 90, value = 30)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("tree_label_fs", "Column label font size:", min = 2, max = 10, value = 5)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("tree_label_offset_x", "Column label offset X:", min = 0, max = 0.2, value = 0.1)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("tree_label_offset_y", "Column label offset Y:", min = 0, max = 200, value = 100))
        ),
        shiny::conditionalPanel(
            condition = "input.show_legend_prop",
            .div_inline_block(6, shiny::sliderInput("tree_legend_fs", "Legend font size:", min = 8, max = 20, value = 14)),
            .div_inline_br_block(0.5),
            .div_inline_block(6, shiny::sliderInput("tree_legend_size", "Legend size:", min = 0.5, max = 2, value = 1.4))
        ),
        shiny::tags$hr(),
        shiny::br(), shiny::br(),
        shiny::uiOutput("treeUI")
    )
}

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

                .tree_plot_panel(),

                # Sub-panel 2 - GWES Manhattan plot
                shiny::tabPanel("GWES Manhattan",
                                shiny::br(),  shiny::br(),  shiny::br(),  shiny::br(),  shiny::br(),
                                shiny::plotOutput("manhattan_plot",
                                                  width = "30cm",
                                                  height = "10cm",
                                                  click = "manhattan_plot_click",
                                                  brush = shiny::brushOpts(id = "manhattan_plot_brush", resetOnNew = TRUE),
                                                  dblclick = "manhattan_plot_double_click"
                                ),
                                shiny::br(), shiny::br(),
                                shiny::h4("Select rows in the table on the left. The corresponding pairs are plotted in red in the scatter plot."),
                                shiny::br(), shiny::br(),
                                shiny::h4("Select an area and double click in it to zoom in. Double click to zoom back out."),
                                shiny::br(), shiny::br(),
                                shiny::h4("Click on (or near) a black point to see the corresponding data in the table below:"),
                                shiny::br(), shiny::br(),
                                shiny::tableOutput("manhattan_plot_table")
                ),

                # Sub-panel 3 - Circular plot
                shiny::tabPanel("Circular plot",
                                shiny::br(), shiny::br(),
                                vegawidget::vegawidgetOutput("circular_plot"),
                                shiny::br(), shiny::br(),
                                shiny::h4("Click and shift-click regions to zoom into gene view. Clicking (and shift-clicking) the genes opens lists on the right side of the plot.")
                )
            )
        )
    )
}

# Define UI for application
.ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),

    # Page with several tabs
    shiny::navbarPage(title = "GWESalyzer",
               id = "tabs", # for keeping track of movement between tabs

               shiny::tabPanel(title = "About",
                        shiny::h4("Something describing the visualisation tool, to be added...")
               ),

               shiny::tabPanel(title = "Upload data", .data_sidebar_layout()),
               shiny::tabPanel(title = "Plot settings", .settings_sidebar_layout()),
               shiny::tabPanel(title = "Analyse SpydrPick output", .plot_sidebar_layout())
    )
)
