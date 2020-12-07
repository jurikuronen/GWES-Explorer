# Define server logic for application.
.server <- function(input, output, session) {

    # Column names in outliers table.
    outlier_columns <- .default_outlier_columns()

    # Reactive values used for zooming in the Manhattan GWES plot.
    mh_gwes_ranges <- shiny::reactiveValues(x = NULL, y = NULL)

    # Prevent events from occurring when creating/updating tables.
    phenotype_selections_updated <- 0

    # Update phenotype selections for this session.
    .update_select_phenotype_input <- function() {
        phenotype_selections_updated <<- 1
        if (.phenotype_is_not_null()) {
            n <- as.list(0:ncol(.data$phenotype))
            names(n) <- c("No phenotype selected", colnames(.data$phenotype))
            shiny::updateSelectInput(session, inputId = "select_phenotype", choices = n)
        } else { shiny::updateSelectInput(session, inputId = "select_phenotype", choices = c("No phenotype selected")) }
    }

    # Render UI for tree plot.
    output$treeUI <- shiny::renderUI({
        shiny::plotOutput("tree_plot", width = paste0(input$tree_plot_width, "cm"), height = paste0(input$tree_plot_height, "cm"))
    })

    # Handle load example data event.
    shiny::observeEvent(input$read_example_data_button, {
        .read_data(file_outliers = tibble::tibble(datapath = "example_data/maela_outliers.outliers"),
                   file_tree = tibble::tibble(datapath = "example_data/maela_tree.nex"),
                   file_fasta = tibble::tibble(datapath = "example_data/maela_fasta.fasta"),
                   file_loci = tibble::tibble(datapath = "example_data/maela_loci.loci"),
                   file_phenotype = tibble::tibble(datapath = "example_data/maela_phenotypes.csv"),
                   file_gff = tibble::tibble(datapath = "example_data/maela_gff.gff3"))
        output$data_loaded <- shiny::renderText({"Example data loaded!"})
        .process_data()
    })

    # Handle uploaded data event.
    shiny::observeEvent(input$read_data_button, {
        .read_data(file_outliers = input$file_outliers,
                   file_tree = input$file_tree,
                   file_fasta = input$file_fasta,
                   file_loci = input$file_loci,
                   file_phenotype = input$file_phenotype,
                   file_gff = input$file_gff)
        output$data_loaded <- shiny::renderText({"Data loaded!"})
        .process_data()
    })

    # Process uploaded data.
    .process_data <- function() {
        # Update Shiny SelectInput if phenotype data was read.
        if (.phenotype_is_not_null()) .update_select_phenotype_input()

        # Update outlier columns based on what was read.
        outlier_columns <<- .default_outlier_columns()
        if (.gff_is_not_null()) outlier_columns <<- .extended_outlier_columns()

        # Render plots after reading data was completed.
        output$outliers_table <- .generate_outliers_table(input, outlier_columns)
        output$manhattan_plot <- .render_gwes_manhattan_plot(input, mh_gwes_ranges)
        output$manhattan_plot_table <- .render_gwes_manhattan_plot_table(input, outlier_columns)
        output$tree_plot <- .render_tree_plot(input)
        output$circular_plot <- .render_circular_plot()
    }

    # Update table selection type.
    shiny::observeEvent(input$select_row_type, { output$outliers_table <- .generate_outliers_table(input, outlier_columns) })

    # When double-clicking the GWES Manhattan plot, zoom onto the brush bounds, otherwise reset the zoom.
    shiny::observeEvent(input$manhattan_plot_double_click, {
        brush <- input$manhattan_plot_brush
        if (!is.null(brush)) {
            mh_gwes_ranges$x <- c(brush$xmin, brush$xmax)
            mh_gwes_ranges$y <- c(brush$ymin, brush$ymax)
        } else { mh_gwes_ranges$x <- mh_gwes_ranges$y <- NULL }
    })

    # Handle row selection event.
    shiny::observeEvent(input$outliers_table_rows_selected, {
        output$tree_plot <- .render_tree_plot(input)
        selected_rows <- input$outliers_table_rows_selected
        if (.gff_is_not_null() && length(selected_rows) > 0) .set_circular_plot_signals(selected_rows[1])
    })

    shiny::observeEvent(input$select_phenotype, {
        if (phenotype_selections_updated != 0) { # Don't trigger event if the selections were just updated.
            phenotype_selections_updated <<- 0
        } else {
            output$tree_plot <- .render_tree_plot(input)
        }
    })

    # Hide data read applied text when reading in new data.
    shiny::observeEvent(c(input$file_outliers, input$file_tree, input$file_fasta, input$file_loci, input$file_phenotype, input$file_gff), {
        output$data_loaded <- shiny::renderText({""})
    })

    # Modify circular plot signals from Shiny UI.
    vegawidget::vw_shiny_set_signal("circular_plot", name = "radius", value = input$circular_plot_radius)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "radius_offset_1", value = input$circular_plot_radius_offset)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "radius_offset_adjustment", value = input$circular_plot_radius_offset_adjustment)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "rotate", value = input$circular_plot_rotate)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "gene_arc_angle_1", value = input$circular_plot_gene_arc_angle)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "text_size_region", value = input$circular_plot_text_size_region)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "text_size_gene", value = input$circular_plot_text_size_gene)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "text_size_tooltip", value = input$circular_plot_text_size_tooltip)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "show_region_links", value = input$circular_plot_show_region_links)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "show_gene_links", value = input$circular_plot_show_gene_links)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "opacity_region_link_adjustment", value = input$circular_plot_opacity_region_link_adjustment)
    vegawidget::vw_shiny_set_signal("circular_plot", name = "opacity_gene_link_adjustment", value = input$circular_plot_opacity_gene_link_adjustment)

    # Download handlers for saving plots.
    output$gwes_manhattan_plot_download <- .gwes_manhattan_plot_download_handler(input, mh_gwes_ranges)
    output$phylogenetic_tree_plot_download <- .phylogenetic_tree_plot_download_handler(input)
    output$circular_plot_download <- .circular_plot_download_handler(input, output)

}
