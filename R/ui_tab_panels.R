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
        shiny::p("All plots require an outliers file in the SpydrPick format."),
        shiny::uiOutput("outliers_file_input"),
        shiny::hr(),

        shiny::p("The Tree-MSA plot requires tree, FASTA and loci files. A phenotype data file is optional."),
        shiny::uiOutput("tree_file_input"),
        shiny::uiOutput("fasta_file_input"),
        shiny::uiOutput("loci_file_input"),
        shiny::uiOutput("phenotype_file_input"),
        shiny::hr(),

        shiny::p("The circular plot requires a GFF3 file."),
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

# Creates the "Upload data" main panel describing the input file requirements.
.ui_upload_data_main_panel <- function() {
    shiny::mainPanel(
        shiny::h3("Data formats"),
        shiny::br(),

        shiny::h4("SpydrPick outliers file (.outliers, .txt)"),
        shiny::p("All plots require an outliers file in the SpydrPick format. The file must be space-delimited and ",
                 "contain no header row. The first five columns, in this order, are required:"),
        shiny::code("Pos_1 Pos_2 Distance Direct MI"),
        shiny::br(), shiny::br(),
        shiny::p(shiny::code("Pos_1"), " and ", shiny::code("Pos_2"), " specify a pair of genomic positions, ",
                 shiny::code("Distance"), " base pairs apart, with mutual information score ", shiny::code("MI"),
                 ". ", shiny::code("Direct"), " is a 1/0 Boolean value from the ARACNE filtering step: 1 indicates ",
                 "a direct link and 0 an indirect link. At least one direct link is required."),
        shiny::p("An optional sixth column, ", shiny::code("MI_wogaps"), ", gives the MI score calculated without ",
                 "gaps and may be provided to display additional information."),
        shiny::br(),

        shiny::h4("Tree-MSA files"),
        shiny::p(
            "The Tree-MSA plot requires a phylogenetic tree, a DNA multiple sequence alignment and a loci file. ",
            "Phenotype data may also be provided."
        ),

        shiny::h5("Phylogenetic tree (.nwk, .nex)"),
        shiny::p("The tree must be in Newick or Nexus format."),

        shiny::h5("Multiple sequence alignment (.fasta, .fa, .aln)"),
        shiny::p(
            "The alignment must be in FASTA format. All FASTA sequences must have the same length."
        ),

        shiny::h5("Loci file (.loci)"),
        shiny::p(
            "The file must contain one genomic position per line in the same order as the alignment columns."
        ),

        shiny::h5("Optional phenotype file (.csv, .txt)"),
        shiny::p(
            "The file must contain comma-separated values with a header row specifying the column names. Its first ",
            "column contains the sample identifiers and the remaining columns contain phenotype values."
        ),
        shiny::br(),

        shiny::h4("GFF3 file for the circular plot (.gff3)"),
        shiny::p(
            "The circular plot requires a GFF3 annotation for the bacterial reference genome corresponding to the ",
            "genomic positions in the SpydrPick results."
        ),
        shiny::p(
            "GWES-Explorer uses ", shiny::code("gene"), " features when present and ", shiny::code("CDS"),
            " features otherwise."
        ),
        shiny::p(
            "The ", shiny::code("attributes"), " field should contain a ", shiny::code("Name"),
            " tag for use as the feature label in GWES-Explorer."
        )
    )
}

# Creates the "Upload data" tab panel.
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
