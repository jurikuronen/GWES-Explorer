# Defines the server logic for the Shiny application.
.server <- function(input, output, session) {

    # Application data for this session.
    data <- new.env(parent = emptyenv())

    # Creates file information for an example data file.
    .example_file <- function(filename) {
        tibble::tibble(datapath = system.file("extdata",
                                              filename,
                                              package = "GWESExplorer",
                                              mustWork = TRUE),
                       name = filename)
    }

    # Example data files for the "Load example data" action.
    .example_outliers_file <- .example_file("maela_outliers.outliers")
    .example_tree_file <- .example_file("maela_tree.nex")
    .example_fasta_file <- .example_file("maela_fasta.fasta")
    .example_loci_file <- .example_file("maela_loci.loci")
    .example_phenotype_file <- .example_file("maela_phenotypes.csv")
    .example_gff_file <- .example_file("maela_gff.gff3")

    # Default columns for the outliers tables.
    .default_outlier_columns <- c("Pos_1", "Pos_2", "MI", "MI_wogaps", "Distance")

    # Columns currently shown in the outliers tables.
    .outlier_columns <- .default_outlier_columns

    # Zoom ranges for the GWES Manhattan plot.
    .manhattan_plot_ranges <- shiny::reactiveValues(x = NULL, y = NULL)

    # Tracks whether each file has been uploaded.
    .file_uploaded <- shiny::reactiveValues(outliers = FALSE,
                                            tree = FALSE,
                                            fasta = FALSE,
                                            loci = FALSE,
                                            phenotype = FALSE,
                                            gff = FALSE)

    # Updates the phenotype choices.
    .update_select_phenotype_input <- function() {
        if (!is.null(data$phenotype)) {
            # Zero represents the special value "No phenotype selected".
            phenotype_choices <- as.list(0:ncol(data$phenotype))
            names(phenotype_choices) <- c("No phenotype selected", colnames(data$phenotype))
        } else {
            phenotype_choices <- c("No phenotype selected" = 0)
        }

        shiny::updateSelectInput(session,
                                 inputId = "select_phenotype",
                                 choices = phenotype_choices)
    }

    # Renders a file upload button.
    .render_ui_file_input <- function(input_id, label, accept) {
        shiny::renderUI({
            shiny::fileInput(inputId = input_id,
                             label = label,
                             accept = accept)
        })
    }

    # Renders the file upload buttons.
    .render_file_upload_buttons <- function() {
        output$outliers_file_input <- .render_ui_file_input("outliers_file",
                                                            "SpydrPick outliers file (.outliers, .txt):",
                                                            c(".outliers", ".txt"))
        output$tree_file_input <- .render_ui_file_input("tree_file",
                                                        "Tree file (Newick [.nwk] or Nexus [.nex]):",
                                                        c(".nwk", ".nex"))
        output$fasta_file_input <- .render_ui_file_input("fasta_file",
                                                         "FASTA file (.fasta, .fa, .aln):",
                                                         c(".fasta", ".fa", ".aln"))
        output$loci_file_input <- .render_ui_file_input("loci_file",
                                                        "Loci file (.loci):",
                                                        ".loci")
        output$phenotype_file_input <- .render_ui_file_input("phenotype_file",
                                                             "Phenotype data file (.csv, .txt):",
                                                             c(".csv", ".txt"))
        output$gff_file_input <- .render_ui_file_input("gff_file",
                                                       "GFF3 file (.gff3):",
                                                       ".gff3")
    }

    # Render the initial file upload buttons. "Clear file selections" re-renders them.
    .render_file_upload_buttons()

    # Creates a download handler for saving a plot.
    .download_handler <- function(prefix, plot_function) {
        shiny::downloadHandler(
            filename = function() {
                paste0(
                    "GWES-Explorer_",
                    format(Sys.time(), "%Y%m%d_%H%M%S"),
                    "_",
                    prefix,
                    ".",
                    input[[paste0(prefix, "_type")]]
                )
            },
            content = function(file) {
                device <- input[[paste0(prefix, "_type")]]
                width <- input[[paste0(prefix, "_width")]]
                height <- input[[paste0(prefix, "_height")]]
                dpi <- input[[paste0(prefix, "_dpi")]]
                units <- input[[paste0(prefix, "_unit")]]

                if (device %in% c("png", "tiff", "jpeg")) {
                    width_inches <- if (units == "cm") width / 2.54 else width
                    height_inches <- if (units == "cm") height / 2.54 else height
                    pixel_count <- width_inches * dpi * height_inches * dpi

                    if (pixel_count > 10000000) {
                        shiny::showNotification(
                            "Image size must not exceed 10 million pixels. Reduce the width, height or DPI.",
                            type = "error"
                        )
                        shiny::req(FALSE)
                    }
                }

                # Generate the plot here so each download includes the current selections and settings.
                plot <- plot_function()

                if (is.null(plot)) {
                    shiny::showNotification(
                        "No plot to download.",
                        type = "error"
                    )
                    shiny::req(FALSE)
                }

                ggsave(
                    filename = file,
                    plot = plot,
                    device = device,
                    width = width,
                    height = height,
                    dpi = dpi,
                    units = units
                )
            }
        )
    }

    # Reads and loads data from the provided files.
    .read_and_load_data <- function(outliers_file,
                                    tree_file,
                                    fasta_file,
                                    loci_file,
                                    phenotype_file,
                                    gff_file,
                                    failure_message)
    {
        load_status <- .read_data(data = data,
                                  outliers_file = outliers_file,
                                  tree_file = tree_file,
                                  fasta_file = fasta_file,
                                  loci_file = loci_file,
                                  phenotype_file = phenotype_file,
                                  gff_file = gff_file)
        if (load_status$success == .STATUS_SUCCESS) {
            output$data_load_result <- shiny::renderText({"Data loaded!"})
            .update_data_ui()
        } else {
            output$data_load_result <- shiny::renderText({failure_message})
        }
        output$data_load_status <- shiny::renderUI({ load_status$status })
    }

    # Returns uploaded file data based on the file's reactive upload state.
    .get_file_data <- function(file_key) {
        if (!.file_uploaded[[file_key]]) {
            return(NULL)
        }

        return(input[[paste0(file_key, "_file")]])
    }

    # Renders an interactive table of outliers using DT.
    .render_outliers_table <- function(outlier_columns) {
        if (is.null(data$outliers)) {
            # Default table when there is no data.
            return(DT::renderDT(tibble::tibble(Pos_1 = integer(),
                                               Pos_2 = integer(),
                                               MI = numeric(),
                                               Distance = integer()),
                                server = FALSE,
                                options = list(pageLength = 25, scrollX = TRUE)))
        }

        return(DT::renderDT(data$outliers_direct[, outlier_columns],
                            server = FALSE,
                            options = list(pageLength = 25, scrollX = TRUE),
                            selection = input$select_row_type))
    }

    # Render the initial outliers table. It's a reactive DT table, so an event handler is not needed.
    output$outliers_table <- .render_outliers_table(.outlier_columns)

    # Updates the UI to reflect the loaded data.
    .update_data_ui <- function() {
        # Update the phenotype selector for the loaded data.
        .update_select_phenotype_input()

        # Keep MI_wogaps out of the tables when the optional sixth column was not provided.
        .outlier_columns <<- intersect(.default_outlier_columns, names(data$outliers))

        # Add feature columns when GFF3 data was provided.
        if (!is.null(data$gff)) {
            .outlier_columns <<- c(.outlier_columns, "Pos_1_feature", "Pos_2_feature")
        }

        # Render the tables and plots for the loaded data.
        output$outliers_table <- .render_outliers_table(.outlier_columns)
        output$manhattan_plot <- .render_gwes_manhattan_plot(data, input, .manhattan_plot_ranges)
        output$manhattan_plot_table <- .render_gwes_manhattan_plot_table(data, input, .outlier_columns)

        # Session data is not reactive, so recreate the renderer here.
        # Shiny reruns it when phenotype or row selection inputs change.
        output$tree_plot <- .render_phylogenetic_tree_plot(data, input)
        output$circular_plot <- .render_circular_plot(data$circular_plot_spec)
    }

    # Handle file upload events: mark the corresponding files as uploaded.
    shiny::observeEvent(input$outliers_file, { .file_uploaded$outliers <- TRUE })
    shiny::observeEvent(input$tree_file, { .file_uploaded$tree <- TRUE })
    shiny::observeEvent(input$fasta_file, { .file_uploaded$fasta <- TRUE })
    shiny::observeEvent(input$loci_file, { .file_uploaded$loci <- TRUE })
    shiny::observeEvent(input$phenotype_file, { .file_uploaded$phenotype <- TRUE })
    shiny::observeEvent(input$gff_file, { .file_uploaded$gff <- TRUE })

    # Handle the "Load example data" event: load the included Maela example dataset.
    shiny::observeEvent(input$load_example_data_button, {
        .read_and_load_data(outliers_file = .example_outliers_file,
                            tree_file = .example_tree_file,
                            fasta_file = .example_fasta_file,
                            loci_file = .example_loci_file,
                            phenotype_file = .example_phenotype_file,
                            gff_file = .example_gff_file,
                            failure_message = "Failed to load example data.")
    })

    # Handle the "Load data" event: load the files currently selected in the upload controls.
    shiny::observeEvent(input$load_data_button, {
        .read_and_load_data(outliers_file = .get_file_data("outliers"),
                            tree_file = .get_file_data("tree"),
                            fasta_file = .get_file_data("fasta"),
                            loci_file = .get_file_data("loci"),
                            phenotype_file = .get_file_data("phenotype"),
                            gff_file = .get_file_data("gff"),
                            failure_message = "Failed to load uploaded data.")
    })

    # Handle the "Clear loaded data" event: clear already loaded data while preserving the file selections.
    shiny::observeEvent(input$clear_loaded_data_button, {
        clear_status <- .clear_data(data)
        .update_data_ui()

        output$data_load_result <- shiny::renderText({"Cleared data."})
        output$data_load_status <- shiny::renderUI({ clear_status$status })
    })

    # Handle the "Clear file selections" event: clear the file selections from the upload buttons.
    shiny::observeEvent(input$clear_file_selections_button, {
        .file_uploaded$outliers <- FALSE
        .file_uploaded$tree <- FALSE
        .file_uploaded$fasta <- FALSE
        .file_uploaded$loci <- FALSE
        .file_uploaded$phenotype <- FALSE
        .file_uploaded$gff <- FALSE

        # Recreate the file inputs so the browser no longer displays the selected filenames.
        .render_file_upload_buttons()

        clear_status <- .clear_file_selections()
        if (clear_status$success == .STATUS_FAILURE) {
            output$data_load_result <- shiny::renderText({"Failed to clear file selections."})
        } else {
            output$data_load_result <- shiny::renderText({"Cleared file selections."})
        }
        output$data_load_status <- shiny::renderUI({ clear_status$status })
    })

    # Handle a double-click event on the GWES Manhattan plot: zoom to the brushed area or reset the zoom.
    shiny::observeEvent(input$manhattan_plot_double_click, {
        brush <- input$manhattan_plot_brush
        if (is.null(brush)) {
            .manhattan_plot_ranges$x <- NULL
            .manhattan_plot_ranges$y <- NULL
            return()
        }

        .manhattan_plot_ranges$x <- c(brush$xmin, brush$xmax)
        .manhattan_plot_ranges$y <- c(brush$ymin, brush$ymax)
    })

    # Handle an outliers table row selection event: update the circular plot from the first selected row.
    shiny::observeEvent(input$outliers_table_rows_selected, {
        selected_rows <- input$outliers_table_rows_selected
        if (is.null(data$gff) || length(selected_rows) == 0) {
            return()
        }

        .set_circular_plot_signals(data, selected_rows[1])
    })

    # Render the Tree-MSA plot output reactively at the selected size.
    output$tree_plot_ui_output <- shiny::renderUI({
        shiny::plotOutput("tree_plot",
                          width = paste0(input$tree_plot_width, "cm"),
                          height = paste0(input$tree_plot_height, "cm"))
    })

    # Set download handlers for Manhattan and phylogenetic tree plots.
    output$gwes_manhattan_plot_download <- .download_handler(prefix = "gwes_manhattan_plot",
                                                             plot_function = function() {
                                                                 .gwes_manhattan_plot(data,
                                                                                      input,
                                                                                      .manhattan_plot_ranges)
                                                             })
    output$phylogenetic_tree_plot_download <- .download_handler(prefix = "phylogenetic_tree_plot",
                                                                plot_function = function() {
                                                                    .create_phylogenetic_tree_plot(data, input)
                                                                })

    # Set up circular plot signal updates from the Shiny UI.
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "radius",
                                    value = input$circular_plot_radius)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "feature_view_1_radius",
                                    value = input$circular_plot_feature_view_1_radius)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "feature_view_2_radius",
                                    value = input$circular_plot_feature_view_2_radius)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "rotate",
                                    value = input$circular_plot_rotate)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "feature_view_1_rotation",
                                    value = input$circular_plot_feature_view_1_rotation)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "feature_view_2_rotation",
                                    value = input$circular_plot_feature_view_2_rotation)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "feature_view_1_degrees",
                                    value = input$circular_plot_feature_view_1_degrees)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "feature_view_2_degrees",
                                    value = input$circular_plot_feature_view_2_degrees)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "feature_view_1_flip_inwards",
                                    value = input$circular_plot_feature_view_1_flip_inwards)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "feature_view_2_flip_inwards",
                                    value = input$circular_plot_feature_view_2_flip_inwards)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "text_size_region",
                                    value = input$circular_plot_text_size_region)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "feature_label_text_size",
                                    value = input$circular_plot_feature_label_text_size)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "text_size_tooltip",
                                    value = input$circular_plot_text_size_tooltip)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "show_region_links",
                                    value = input$circular_plot_show_region_links)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "show_position_links",
                                    value = input$circular_plot_show_position_links)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "opacity_region_link_adjustment",
                                    value = input$circular_plot_opacity_region_link_adjustment)
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    name = "position_link_opacity_adjustment",
                                    value = input$circular_plot_position_link_opacity_adjustment)
}
