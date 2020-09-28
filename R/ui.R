.div_inline_block <- function(width, content) { shiny::div(style = paste0("display: inline-block; vertical-align: top; width: ", width, "cm"), content)}
.div_inline_br_block <- function(width) { .div_inline_block(width, shiny::br()) }
.prettySwitch <- function(id, name) { shinyWidgets::prettySwitch(id, name, fill = TRUE, status = "primary", value = FALSE) }

.data_sidebar_layout <- function() {
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::p("All plots require the SpydrPick outliers file."),
            shiny::fileInput(
                inputId = "file_outliers",
                label = "SpydrPick outliers file (.outliers, .txt):",
                accept = c(".outliers", ".txt")
            ),
            shiny::hr(),

            shiny::p("Tree plot requires tree, fasta and loci files. Optionally, you can upload a phenotypic data file."),
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
            shiny::hr(),

            shiny::p("Circular plot requires a gff file."),
            shiny::fileInput(
                inputId = "file_gff",
                label = "Gff file (.gff3):",
                accept = ".gff3"
            ),
            shiny::hr(),

            shiny::actionButton(inputId = "read_data_button", label = "Read in data"),
            .div_inline_br_block(3),
            .div_inline_block(4.5, shinyjs::hidden(shiny::p(id = "reading_data_div", shiny::textOutput("data_loaded"))))
        ),

        shiny::mainPanel(
            shiny::h3("Data formats"),
            shiny::br(),
            shiny::h4("SpydrPick outliers file (.outliers, .txt)"),
            shiny::p("The outliers file obtained with SpydrPick should be a space-delimited no-headers file with at least the following columns:"),
            shiny::code("Pos_1", shiny::HTML("&nbsp;"),
                        "Pos_2", shiny::HTML("&nbsp;"),
                        "Distance", shiny::HTML("&nbsp;"),
                        "Direct", shiny::HTML("&nbsp;"),
                        "MI", shiny::HTML("&nbsp;"),
                        "MI_wogaps"),
            shiny::br(), shiny::br(),
            shiny::p("Above, ",
                     shiny::code("Pos_1"),
                     " and ",
                     shiny::code("Pos_2"),
                     " refer to a pair of positions in the output that are ",
                     shiny::code("Distance"),
                     " base pairs apart and whose MI score is",
                     shiny::code("MI"),
                     " and MI score without gaps is ",
                     shiny::code("MI_wogaps"),
                     ". The ",
                     shiny::code("Direct"),
                     " column is a 1/0 Boolean value from the ARACNE filtering step, where 0 indicates being filtered out by ARACNE."),
            shiny::br(),
            shiny::strong("All plots provided by GWESalyzer require the outliers file."),
            shiny::br(), shiny::br(),
            shiny::h4("Phylogenetic tree files"),
            shiny::p("The phylogenetic tree plot requires a Newick or Nexus tree file, a fasta and a loci file.",
                    "Optionally, you can upload a phenotypic data file."),
            shiny::br(),
            shiny::h4("GFF3 file for the circular plot"),
            shiny::p("The circular plot requires a GFF3 file with the fields"),
            shiny::code("type", shiny::HTML("&nbsp;"),
                        "start", shiny::HTML("&nbsp;"),
                        "end", shiny::HTML("&nbsp;"),
                        "attributes"),
            shiny::br(), shiny::br(),
            shiny::p("defined. Further, the ",
                     shiny::code("attributes"),
                     " field should have a ",
                     shiny::code("Name"),
                     " tag present.")
        )
    )
}

.gwes_plot_panel <- function() {
    shiny::tabPanel("GWES Manhattan",
                    shiny::br(), shiny::br(), shiny::br(), shiny::br(), shiny::br(),
                    shiny::plotOutput("manhattan_plot",
                                      width = "30cm",
                                      height = "10cm",
                                      click = "manhattan_plot_click",
                                      brush = shiny::brushOpts(id = "manhattan_plot_brush", resetOnNew = TRUE),
                                      dblclick = "manhattan_plot_double_click"
                    ),
                    shiny::br(), shiny::br(),
                    shiny::h4("Select rows in the table on the left. The corresponding pairs are plotted in red in the scatter plot."),
                    shiny::h4("Select an area and double click in it to zoom in. Double click to zoom back out."),
                    shiny::h4("Click on (or near) a black point to see the corresponding data in the table below:"),
                    shiny::br(),
                    shiny::tableOutput("manhattan_plot_table")
    )
}

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
        shiny::hr(),
        shiny::br(), shiny::br(),
        shiny::uiOutput("treeUI")
    )
}

.circular_plot_panel <- function() {
    shiny::tabPanel("Circular plot",
                    shiny::br(), shiny::br(),
                    vegawidget::vegawidgetOutput("circular_plot"),
                    shiny::br(), shiny::br(),
                    shiny::h4("Click and shift-click regions to zoom into gene view. Clicking (and shift-clicking) the genes opens lists on the right side of the plot.")
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
                .gwes_plot_panel(),
                .tree_plot_panel(),
                .circular_plot_panel()
            )
        )
    )
}

.about_panel <- function() {
    shiny::tabPanel(title = "About",
        shiny::h2("GWESalyzer - a genome-wide epistasis analyzer."),
        shiny::p("GWESalyzer is an interactive visualization tool designed to be used in conjunction with ",
        shiny::HTML('<a href="https://github.com/santeripuranen/SpydrPick" target="_blank">SpydrPick</a>.'),
        "GWESalyzer relies heavily on the Shiny and Vega libraries to deploy it as a web application with multiple plot types ",
        "to visualize the genomic data. GWESalyzer operates on the outliers file generated by SpydrPick ",
        "to produce a GWES Manhattan plot. Two additional plots can be generated, namely a phylogenetic tree ",
        "plot by providing either a Newick or a Nexus file in addition to a FASTA and a loci file, ",
        "and a highly interactive circular chromosome plot by providing a GFF3 file."),
        style = "font-size: 16px; word-wrap: break-word; width: 800px")
}

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
