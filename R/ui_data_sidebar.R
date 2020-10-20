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
