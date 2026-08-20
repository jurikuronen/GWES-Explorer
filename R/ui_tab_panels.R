# Creates the "About" tab panel.
.ui_about_tab_panel <- function() {
    shiny::tabPanel(
        title = "About",
        shiny::h2("GWES-Explorer - an interactive genome-wide epistasis visualization tool"),
        # Display the installed package version.
        shiny::p(paste("Version", as.character(utils::packageVersion("GWESExplorer")))),
        shiny::h3("Overview"),
        shiny::p(
            "GWES-Explorer is an interactive browser-based visualization tool for exploring genome-wide epistasis ",
            "study results. It provides three visualizations: the GWES Manhattan, Tree-MSA and circular plots. The ",
            "GWES Manhattan plot allows the user to examine the signal peak regions. The Tree-MSA plot provides ",
            "information about the population allele distribution at the related SNP loci together with optional ",
            "phenotype data. The circular plot uses genomic annotations from a GFF3 file and allows the user to ",
            "explore interactions at the gene level. All three plots update in response to the selected interaction."
        ),
        shiny::h3("Using GWES-Explorer"),
        shiny::p(
            "Use the ", shiny::strong("Upload data"),
            " tab to load GWES results and any supplementary files required by the visualizations. An example dataset ",
            "is also provided for trying out the application."
        ),
        shiny::p(
            "Use the ", shiny::strong("Analyze data"),
            " tab to select interactions from the outliers table and examine them in the available plots. Additional ",
            "plots can be generated when the required supplementary data are provided."
        ),
        shiny::h3("GitHub"),
        shiny::p(
            shiny::tags$a(
                href = "https://github.com/jurikuronen/GWES-Explorer",
                # Keep GWES-Explorer open when the user follows the repository link.
                target = "_blank",
                "https://github.com/jurikuronen/GWES-Explorer"
            )
        ),
        style = paste("font-size: 16px;",
                      "word-wrap: break-word;",
                      "width: 800px")
    )
}

# Creates the "Upload data" sidebar for selecting and loading data files.
.ui_upload_data_sidebar_panel <- function() {
    shiny::sidebarPanel(
        shiny::p("All plots require the SpydrPick outliers file."),
        shiny::uiOutput("outliers_file_input"),
        shiny::hr(),

        shiny::p("Tree plot requires tree, fasta and loci files. Optionally, you can upload a phenotypic data file."),
        shiny::uiOutput("tree_file_input"),
        shiny::uiOutput("fasta_file_input"),
        shiny::uiOutput("loci_file_input"),
        shiny::uiOutput("phenotype_file_input"),
        shiny::hr(),

        shiny::p("Circular plot requires a gff file."),
        shiny::uiOutput("gff_file_input"),
        shiny::hr(),

        shiny::actionButton(
            inputId = "clear_file_selections_button",
            label = "Clear file selections"
        ),
        # Align the data clearing and loading buttons to the right of the file-selection button.
        shiny::div(
            style = "float: right;",
            shiny::actionButton(
                inputId = "clear_loaded_data_button",
                label = "Clear loaded data"
            ),
            .div_inline_block(1, NULL),
            shiny::actionButton(
                inputId = "load_data_button",
                label = "Load data"
            )
        ),
        shiny::br(),
        shiny::br(),
        shiny::div(
            style = "float: right;",
            shiny::actionButton(
                inputId = "load_example_data_button",
                label = "Load example data (Maela)"
            )
        ),
        # Clear the preceding floats so the status messages appear below the buttons.
        shiny::p(
            style = "clear: both;",
            shiny::textOutput("data_load_result", inline = TRUE)
        ),
        shiny::p(shiny::htmlOutput("data_load_status", inline = TRUE))
    )
}

.ui_upload_data_main_panel <- function() {
    shiny::mainPanel(
        shiny::h3("Data formats"),
        shiny::br(),
        shiny::h4("SpydrPick outliers file (.outliers, .txt)"),
        shiny::p("The outliers file obtained with SpydrPick should be a space-delimited no-headers file with at least ",
                 "the following columns:"),
        shiny::code("Pos_1", shiny::HTML("&nbsp;"),
                    "Pos_2", shiny::HTML("&nbsp;"),
                    "Distance", shiny::HTML("&nbsp;"),
                    "Direct", shiny::HTML("&nbsp;"),
                    "MI", shiny::HTML("&nbsp;"),
                    "MI_wogaps"),
        shiny::br(), shiny::br(),
        shiny::p("Above, ", shiny::code("Pos_1"), " and ", shiny::code("Pos_2"), " refer to a pair of positions in ",
                 "the output that are ", shiny::code("Distance"), " base pairs apart and whose MI score is",
                 shiny::code("MI"), " and MI score without gaps is ", shiny::code("MI_wogaps"), ". The ",
                 shiny::code("Direct"), " column is a 1/0 Boolean value from the ARACNE filtering step, where 0 ",
                 "indicates being filtered out by ARACNE."),
        shiny::br(),
        shiny::strong("All plots provided by GWES-Explorer require the outliers file."),
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
        shiny::p("defined. GWES-Explorer primarily looks for \"gene\" in the ", shiny::code("type"), "field and ",
                 "secondarily \"CDS\". Further, the ", shiny::code("attributes"), " field should have a ",
                 shiny::code("Name"), " tag present.")
    )
}

.ui_upload_data_tab_panel <- function() {
    shiny::tabPanel(
        title = "Upload data",
        shiny::sidebarLayout(
            .ui_upload_data_sidebar_panel(),
            .ui_upload_data_main_panel()
        )
    )
}

.ui_analyze_data_tab_panel <- function() {
    shiny::tabPanel(
        title = "Analyze data",
        shiny::sidebarLayout(
            # Sidebar panel - a DT table listing the direct outliers.
            shiny::sidebarPanel(
                # Drop-down list for selecting phenotype.
                shiny::selectInput(
                    inputId = "select_phenotype",
                    label = "Select phenotype:",
                    choices = c("No phenotype selected" = 0)
                ),
                # Setting to allow selecting multiple outlier rows.
                shiny::radioButtons(
                    inputId = "select_row_type",
                    label = "Select single or multiple rows:",
                    choices = c(Single = "single",
                                Multiple = "multiple"),
                    selected = "single",
                    inline = TRUE
                ),
                # Display outliers table.
                shiny::div(DT::DTOutput("outliers_table"),
                           style = "font-size: 70%")
            ),

            # Main panel with various plot tabs available.
            shiny::mainPanel(
                shiny::tabsetPanel(
                    type = "tabs",
                    .gwes_plot_panel(),
                    .tree_plot_panel(),
                    .circular_plot_panel()
                )
            )
        )
    )
}
