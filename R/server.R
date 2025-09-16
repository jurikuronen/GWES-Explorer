# Define server logic for the Shiny application.
.server <- function(input, output, session) {

    # Example data files for "Load example data" action.
    .example_outliers_file <- tibble::tibble(datapath = "example_data/maela_outliers.outliers",
                                             name = "maela_outliers.outliers")
    .example_tree_file <- tibble::tibble(datapath = "example_data/maela_tree.nex",
                                         name = "maela_tree.nex")
    .example_fasta_file <- tibble::tibble(datapath = "example_data/maela_fasta.fasta",
                                          name = "maela_fasta.fasta")
    .example_loci_file <- tibble::tibble(datapath = "example_data/maela_loci.loci",
                                         name = "maela_loci.loci")
    .example_phenotype_file <- tibble::tibble(datapath = "example_data/maela_phenotypes.csv",
                                              name = "maela_phenotypes.csv")
    .example_gff_file <- tibble::tibble(datapath = "example_data/maela_gff.gff3",
                                        name = "maela_gff.gff3")

    # Column names in outliers table.
    .default_outlier_columns <- c("Pos_1", "Pos_2", "MI", "MI_wogaps", "Distance")
    .extended_outlier_columns <- c(.default_outlier_columns, "Pos_1_gene", "Pos_2_gene")

    # Currently used column names.
    .outlier_columns <- .default_outlier_columns

    # Reactive values used for zooming in the Manhattan GWES plot.
    .mh_gwes_ranges <- shiny::reactiveValues(x = NULL, y = NULL)

    # Reactive values used to check whether some file has been uploaded.
    .file_uploaded <- shiny::reactiveValues(outliers = 0,
                                           tree = 0,
                                           fasta = 0,
                                           loci = 0,
                                           phenotype = 0,
                                           gff = 0)

    # Flag to prevent events from occurring when creating/updating tables.
    .phenotype_selections_updated <- 0

    # Update phenotype selections for this session.
    .update_select_phenotype_input <- function() {
        .phenotype_selections_updated <<- 1
        if (!is.null(.data$phenotype)) {
            n <- as.list(0:ncol(.data$phenotype))
            names(n) <- c("No phenotype selected",
                          colnames(.data$phenotype))
            shiny::updateSelectInput(session,
                                     inputId = "select_phenotype",
                                     choices = n)
        } else {
            shiny::updateSelectInput(session,
                                     inputId = "select_phenotype",
                                     choices = c("No phenotype selected"))
        }
    }

    # Helper function to render file input buttons.
    .render_ui_file_input <- function(input_id, label, accept) {
        shiny::renderUI({
            shiny::fileInput(inputId = input_id,
                             label = label,
                             accept = accept)
        })
    }

    # Download handlers for saving plots.
    .download_handler <- function(input, key, plot) {
        dpi <- 96
        download_name <- paste0("GWES-Explorer_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", key, "_plot")
        shiny::downloadHandler(
            filename = function() {
                paste0(download_name, ".", input[[paste0(key, "_plot_type")]])
            },
            content = function(file) {
                ggsave(
                    filename = file,
                    plot = plot,
                    device = input[[paste0(key, "_plot_type")]],
                    width = input[[paste0(key, "_plot_width")]] * input[[paste0(key, "_plot_dpi")]] / dpi,
                    height = input[[paste0(key, "_plot_height")]] * input[[paste0(key, "_plot_dpi")]] / dpi,
                    dpi = dpi,
                    units = input[[paste0(key, "_plot_unit")]]
                )
            }
        )
    }

    # Helper function to read in and load data from the provided files.
    .read_and_load_data <- function(outliers_file,
                                    tree_file,
                                    fasta_file,
                                    loci_file,
                                    phenotype_file,
                                    gff_file)
    {
        result <- .read_data(outliers_file, tree_file, fasta_file, loci_file, phenotype_file, gff_file)
        if (result$success == .STATUS_SUCCESS) {
            output$data_load_result <- shiny::renderText({"Data loaded!"})
            .process_data()
        } else {
            output$data_load_result <- shiny::renderText({"Failed to load example data."})
        }
        output$data_load_status <- shiny::renderUI({ result$status })
    }

    # Helper function to return uploaded file data based on reactive file upload states.
    .get_file_data <- function(key) {
        if (.file_uploaded[[key]] == 1) {
            return(input[[paste0(key, "_file")]])
        }
        return(NULL)
    }

    # Helper function to generate the outliers table.
    .generate_outliers_table <- function(input, outlier_columns) {
        if (!is.null(.data$outliers)) {
            return(DT::renderDT(.data$outliers_direct[, outlier_columns],
                                server = FALSE,
                                options = list(pageLength = 25, scrollX = TRUE),
                                selection = input$select_row_type))
        }
        # Default table when there is no data.
        return(DT::renderDT(tibble::tibble(Pos_1 = integer(),
                                           Pos_2 = integer(),
                                           Distance = integer(),
                                           MI = numeric(),
                                           MI_wogaps = numeric()),
                            server = FALSE,
                            options = list(pageLength = 25, scrollX = TRUE)))
    }

    # Helper function to process uploaded data.
    .process_data <- function() {
        # Update Shiny SelectInput if phenotype data was read.
        .update_select_phenotype_input()

        # Update outlier columns based on what was read.
        if (is.null(.data$gff)) {
            .outlier_columns <<- .default_outlier_columns
        } else {
            .outlier_columns <<- .extended_outlier_columns
        }

        # Render plots after reading data was completed.
        output$outliers_table <- .generate_outliers_table(input, .outlier_columns)
        output$manhattan_plot <- .render_gwes_manhattan_plot(input, .mh_gwes_ranges)
        output$manhattan_plot_table <- .render_gwes_manhattan_plot_table(input, .outlier_columns)
        output$tree_plot <- .render_tree_plot(input)
        output$circular_plot <- .render_circular_plot()
    }

    # Set reactive file upload states.
    shiny::observeEvent(input$outliers_file, { .file_uploaded$outliers = 1 })
    shiny::observeEvent(input$tree_file, { .file_uploaded$tree = 1 })
    shiny::observeEvent(input$fasta_file, { .file_uploaded$fasta = 1 })
    shiny::observeEvent(input$loci_file, { .file_uploaded$loci = 1 })
    shiny::observeEvent(input$phenotype_file, { .file_uploaded$phenotype = 1 })
    shiny::observeEvent(input$gff_file, { .file_uploaded$gff = 1 })

    # Handle load example data event.
    shiny::observeEvent(input$read_example_data_button, {
        .read_and_load_data(outliers_file = .example_outliers_file,
                            tree_file = .example_tree_file,
                            fasta_file = .example_fasta_file,
                            loci_file = .example_loci_file,
                            phenotype_file = .example_phenotype_file,
                            gff_file = .example_gff_file)
    })

    # Handle load data event.
    shiny::observeEvent(input$read_data_button, {
        .read_and_load_data(outliers_file = .get_file_data("outliers"),
                            tree_file = .get_file_data("tree"),
                            fasta_file = .get_file_data("fasta"),
                            loci_file = .get_file_data("loci"),
                            phenotype_file = .get_file_data("phenotype"),
                            gff_file = .get_file_data("gff"))
    })

    # Handle clear data event.
    shiny::observeEvent(input$clear_data_button, {
        # Clear result and status.
        output$data_load_result <- shiny::renderText({"Clearing data..."})
        output$data_load_status <- shiny::renderUI({""})

        result <- .clear_data()
        .outlier_columns <<- .default_outlier_columns
        .update_select_phenotype_input()
        .process_data()

        output$data_load_result <- shiny::renderText({"Cleared data."})
        output$data_load_status <- shiny::renderUI({ result$status })
    })

    # Handle reset uploaded files event.
    shiny::observeEvent(input$reset_uploaded_files_button, {
        # Clear result and status.
        output$data_load_result <- shiny::renderText({""})
        output$data_load_status <- shiny::renderUI({""})

        .file_uploaded$outliers = 0
        .file_uploaded$tree = 0
        .file_uploaded$fasta = 0
        .file_uploaded$loci = 0
        .file_uploaded$phenotype = 0
        .file_uploaded$gff = 0

        # Re-render file input buttons with blank state.
        output$outliers_file_input <- .render_ui_file_input("outliers_file",
                                                            "SpydrPick outliers file (.outliers, .txt):",
                                                            c(".outliers",".txt"))
        output$tree_file_input <- .render_ui_file_input("tree_file",
                                                        "Tree file (Newick [.nwk] or Nexus [.nex]):",
                                                        c(".nwk",".nex"))
        output$fasta_file_input <- .render_ui_file_input("fasta_file",
                                                         "Fasta file (.fasta,
                                                     .fa or .aln):", c(".fasta", ".fa", ".aln"))
        output$loci_file_input <- .render_ui_file_input("loci_file",
                                                        "Loci file (.loci):",
                                                        ".loci")
        output$phenotype_file_input <- .render_ui_file_input("phenotype_file",
                                                             "Phenotypic data file (.csv,
                                                         .txt):", c(".csv", ".txt"))
        output$gff_file_input <- .render_ui_file_input("gff_file",
                                                       "Gff file (.gff3):",
                                                       ".gff3")

        result <- .reset_uploaded_files()
        output$data_load_result <- shiny::renderText({"Reset uploaded files."})
        output$data_load_status <- shiny::renderUI({ result$status })
    })

    # Update table selection type.
    shiny::observeEvent(input$select_row_type,
                        { output$outliers_table <- .generate_outliers_table(input, .outlier_columns) })

    # When double-clicking the GWES Manhattan plot, zoom onto the brush bounds, otherwise reset the zoom.
    shiny::observeEvent(input$manhattan_plot_double_click, {
        brush <- input$manhattan_plot_brush
        if (!is.null(brush)) {
            .mh_gwes_ranges$x <- c(brush$xmin, brush$xmax)
            .mh_gwes_ranges$y <- c(brush$ymin, brush$ymax)
        } else {
            .mh_gwes_ranges$x <- NULL
            .mh_gwes_ranges$y <- NULL
        }
    })

    # Handle row selection event.
    shiny::observeEvent(input$outliers_table_rows_selected, {
        output$tree_plot <- .render_tree_plot(input)
        selected_rows <- input$outliers_table_rows_selected
        if (!is.null(.data$gff) && length(selected_rows) > 0) {
            .set_circular_plot_signals(selected_rows[1])
        }
    })

    # Handle phenotype selection event.
    shiny::observeEvent(input$select_phenotype, {
        # Trigger event only if the selections weren't just updated.
        if (.phenotype_selections_updated == 0) {
            output$tree_plot <- .render_tree_plot(input)
        }
        .phenotype_selections_updated <<- 0
    })

    # Render file input buttons.
    output$outliers_file_input <- .render_ui_file_input("outliers_file",
                                                        "SpydrPick outliers file (.outliers, .txt):",
                                                        c(".outliers",".txt"))
    output$tree_file_input <- .render_ui_file_input("tree_file",
                                                    "Tree file (Newick [.nwk] or Nexus [.nex]):",
                                                    c(".nwk",".nex"))
    output$fasta_file_input <- .render_ui_file_input("fasta_file",
                                                     "Fasta file (.fasta,
                                                     .fa or .aln):", c(".fasta", ".fa", ".aln"))
    output$loci_file_input <- .render_ui_file_input("loci_file",
                                                    "Loci file (.loci):",
                                                    ".loci")
    output$phenotype_file_input <- .render_ui_file_input("phenotype_file",
                                                         "Phenotypic data file (.csv,
                                                         .txt):", c(".csv", ".txt"))
    output$gff_file_input <- .render_ui_file_input("gff_file",
                                                   "Gff file (.gff3):",
                                                   ".gff3")

    # Render UI output for tree plot.
    output$tree_plot_ui_output <- shiny::renderUI({
        shiny::plotOutput("tree_plot",
                          width = paste0(input$tree_plot_width, "cm"),
                          height = paste0(input$tree_plot_height, "cm"))
    })

    # Set download handlers for Manhattan and phylogenetic tree plots.
    output$gwes_manhattan_plot_download <- .download_handler(input,
                                                             key = "gwes_manhattan",
                                                             plot = .gwes_manhattan_plot(input, .mh_gwes_ranges))
    output$phylogenetic_tree_plot_download <- .download_handler(input,
                                                                key = "phylogenetic_tree",
                                                                plot = .tree_plot(input))

    # Setup modifying circular plot signals from Shiny UI.
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "radius",
                                    value = input$circular_plot_radius)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "radius_gene_view_1",
                                    value = input$circular_plot_radius_gene_view_1)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "radius_gene_view_2",
                                    value = input$circular_plot_radius_gene_view_2)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "rotate",
                                    value = input$circular_plot_rotate)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "rotate_gene_view_1",
                                    value = input$circular_plot_rotate_gene_view_1)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "rotate_gene_view_2",
                                    value = input$circular_plot_rotate_gene_view_2)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "gene_arc_angle_1",
                                    value = input$circular_plot_gene_arc_angle_1)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "gene_arc_angle_2",
                                    value = input$circular_plot_gene_arc_angle_2)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "flip_gene_view_1",
                                    value = input$circular_plot_flip_gene_view_1)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "flip_gene_view_2",
                                    value = input$circular_plot_flip_gene_view_2)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "text_size_region",
                                    value = input$circular_plot_text_size_region)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "text_size_gene",
                                    value = input$circular_plot_text_size_gene)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "text_size_tooltip",
                                    value = input$circular_plot_text_size_tooltip)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "show_region_links",
                                    value = input$circular_plot_show_region_links)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "show_gene_links",
                                    value = input$circular_plot_show_gene_links)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "opacity_region_link_adjustment",
                                    value = input$circular_plot_opacity_region_link_adjustment)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "opacity_gene_link_adjustment",
                                    value = input$circular_plot_opacity_gene_link_adjustment)
}
