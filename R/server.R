# Define server logic for application.
.server <- function(input, output, session) {

    # Column names in outlier table.
    outlier_columns <- c("Pos_1", "Pos_2", "Distance", "MI", "MI_wogaps")

    # Prevent events from occurring when creating/updating tables.
    phenotype_selections_updated <- 0

    # Outliers table generation logic.
    .generate_outliers_table <- function(input) {
        if (.outliers_is_not_null()) {
            output$outliers_table <- DT::renderDT(.data$outliers_direct[, outlier_columns],
                                         server = FALSE,
                                         options = list(pageLength = 25, scrollX = TRUE),
                                         selection = input$select_row_type)
        } else { output$outliers_table <- .generate_default_outliers_table() }
    }

    # Generate default table when there is no input.
    .generate_default_outliers_table <- function() {
        DT::renderDT(tibble::tibble(Pos_1 = integer(), Pos_2 = integer(), Distance = integer(), MI = numeric(),
             MI_wogaps = numeric()), server = FALSE, options = list(pageLength = 25, scrollX = TRUE))
    }

    # Update table selection type.
    shiny::observeEvent(input$select_row_type, { .generate_outliers_table(input) })

    # Update phenotype selections.
    .update_select_phenotype_input <- function() {
        phenotype_selections_updated <<- 1
        if (.phenotype_is_not_null()) {
            n <- as.list(0:ncol(.data$phenotype))
            names(n) <- c("No phenotype selected", colnames(.data$phenotype))
            shiny::updateSelectInput(session, inputId = "select_phenotype", choices = n)
        } else { shiny::updateSelectInput(session, inputId = "select_phenotype", choices = c("No phenotype selected")) }
    }


    .render_manhattan_plot <- function(input) {
        if (.outliers_is_not_null()) {
            Distance = MI = Direct = fontsize = NULL  # R CMD check hack.
            selected_rows <- input$outliers_table_rows_selected
            min_mi <- min(.data$outliers$MI)
            max_mi <- max(.data$outliers$MI)
            max_distance <- max(.data$outliers$Distance)
            output$manhattan_plot <- shiny::renderPlot({
                selected_rows <- input$outliers_table_rows_selected
                ggplot(data = .data$outliers, mapping = aes(x = Distance, y = MI, group = Direct, fontsize(12))) +
                    geom_point(aes(color = Direct), size = 0.5) +
                    geom_point(data = .data$outliers_direct[selected_rows, ], size = 5, color = "red", shape = 1) +
                    scale_colour_manual(values=c("grey", "black")) +
                    geom_line(aes(y = min_mi), color = "black") +
                    scale_x_continuous(limits = c(0, max_distance), expand = c(0, 0)) +
                    scale_y_continuous(limits = c(min_mi, max_mi + 0.05), expand = c(0, 0)) +
                    theme(text = element_text(size = 14)) +
                    coord_cartesian(xlim = mh_gwes_ranges$x, ylim = mh_gwes_ranges$y, expand = FALSE)
            })
            output$manhattan_plot_table <- shiny::renderTable({
                shiny::nearPoints(.data$outliers_direct, input$manhattan_plot_click, addDist = TRUE)[, outlier_columns]
            })
        } else { output$manhattan_plot <- NULL; output$manhattan_plot_table <- NULL }
    }

    # Reactive values used for zooming in the Manhattan GWES plot.
    mh_gwes_ranges <- shiny::reactiveValues(x = NULL, y = NULL)

    # When double-clicking the GWES Manhattan plot, zoom onto the brush bounds, otherwise reset the zoom.
    shiny::observeEvent(input$manhattan_plot_double_click, {
        brush <- input$manhattan_plot_brush
        if (!is.null(brush)) {
            mh_gwes_ranges$x <- c(brush$xmin, brush$xmax)
            mh_gwes_ranges$y <- c(brush$ymin, brush$ymax)
        } else { mh_gwes_ranges$x <- mh_gwes_ranges$y <- NULL }
    })

    .render_circular_plot <- function(input) { output$circular_plot <- vegawidget::renderVegawidget(.data$edges) }


    # Render UI for tree plot.
    output$treeUI <- shiny::renderUI({
       shiny::plotOutput("tree_plot", width = paste0(input$width, "cm"), height = paste0(input$height, "cm"))
    })


    shiny::observeEvent(input$outliers_table_rows_selected, {
        .render_tree_plot(input)
        selected_rows <- input$outliers_table_rows_selected
        if (.gff_is_not_null() && length(selected_rows) > 0) {
            vegawidget::vw_shiny_set_signal("circular_plot", "selected_region_1", .data$pos1_regions[selected_rows[1]])
            vegawidget::vw_shiny_set_signal("circular_plot", "selected_gene_1", .data$pos1_genes[selected_rows[1]])
            vegawidget::vw_shiny_set_signal("circular_plot", "selected_position_1", .data$outliers_direct$Pos_1[selected_rows[1]])
            vegawidget::vw_shiny_set_signal("circular_plot", "selected_region_2", .data$pos2_regions[selected_rows[1]])
            vegawidget::vw_shiny_set_signal("circular_plot", "selected_gene_2", .data$pos2_genes[selected_rows[1]])
            vegawidget::vw_shiny_set_signal("circular_plot", "selected_position_2", .data$outliers_direct$Pos_2[selected_rows[1]])
        }
    })

    .read_phenotype_idx <- function(input) {
        phenotype_idx <- as.numeric(input$select_phenotype)
        if (is.na(phenotype_idx)) {
            phenotype_idx <- 0
        }
        return(phenotype_idx)
    }

    .generate_phylogenetic_object <- function(input) {
        phylo_object <- NULL
        if (.tree_is_not_null()) {
            phenotype_idx <- .read_phenotype_idx(input)
            phylo_object <- ggtree::ggtree(.data$tree)
            if (phenotype_idx != 0) {
                phylo_object <- ggtree::gheatmap(
                    p = phylo_object,
                    data = .data$phenotype[, phenotype_idx, drop = FALSE],
                    offset = 0,
                    width = 0.1,
                    color = NA,
                    colnames_angle = input$tree_label_angle,
                    colnames_offset_y = -input$tree_label_offset_y,
                    colnames_offset_x = -input$tree_label_offset_x,
                    font.size = input$tree_label_fs) +
                    theme(legend.text = element_text(size = input$tree_legend_fs),
                          legend.key.size = unit(input$tree_legend_size, "cm"))
            }
        }
        return(phylo_object)
    }

    .render_tree_plot <- function(input) {
        if (.tree_is_not_null()) {
            phylo_object <- .generate_phylogenetic_object(input)
            selected_rows <- input$outliers_table_rows_selected
            if (is.null(selected_rows)) {
                output$tree_plot <- shiny::renderPlot(phylo_object)
            } else {
                # Append a heatmap of a matrix to the right side of the phylogenetic tree.
                ind <- as.character(sort(unique(as.vector(.data$outliers_direct[selected_rows, c("Pos_1", "Pos_2")]))))
                select <- c("-", "A", "C", "G", "N", "T") %in% unique(as.vector(.data$msa[, ind]))
                col <- c("white", viridis::viridis(4)[1:3], "white", viridis::viridis(4)[4])[select]
                output$tree_plot <- shiny::renderPlot({ggtree::gheatmap(
                    p = phylo_object + ggnewscale::new_scale_fill(),
                    data = .data$msa[, ind],
                    offset = 0.25,
                    width = 0.2,
                    color = NA,
                    colnames_angle = input$tree_label_angle,
                    colnames_offset_y = -input$tree_label_offset_y,
                    colnames_offset_x = -input$tree_label_offset_x,
                    font.size = input$tree_label_fs
                ) + scale_fill_manual(values = col) +
                        theme(legend.text = element_text(size = input$tree_legend_fs),
                              legend.key.size = unit(input$tree_legend_size, "cm"))
                })
            }
        } else { output$tree_plot <- NULL; }
    }

    shiny::observeEvent(input$select_phenotype, {
        if (phenotype_selections_updated != 0) { # Don't trigger event if the selections were just updated.
            phenotype_selections_updated <<- 0
        } else {
            .render_tree_plot(input)
        }
    })


    # Handle data reading.
    shiny::observeEvent(input$read_data_button, {
        .read_data(file_outliers = input$file_outliers,
                    file_tree = input$file_tree,
                    file_fasta = input$file_fasta,
                    file_loci = input$file_loci,
                    file_phenotype = input$file_phenotype,
                    file_gff = input$file_gff)
        shinyjs::show("reading_data_div")
        output$data_loaded <- shiny::renderText({"Data loaded!"})

        # Update Shiny SelectInput if phenotype data was read.
        if (.phenotype_is_not_null()) { .update_select_phenotype_input() }

        # Update outlier columns based on what was read.
        outlier_columns <- c("Pos_1", "Pos_2", "Distance", "MI", "MI_wogaps")
        if (.gff_is_not_null()) { outlier_columns <<- append(outlier_columns, c("Pos_1_gene_name", "Pos_2_gene_name")) }

        # Render plots after reading data was completed.
        .generate_outliers_table(input)
        .render_tree_plot(input)
        .render_manhattan_plot(input)
        .render_circular_plot(input)
    })

    # Handle new settings.
    shiny::observeEvent(input$apply_settings_button, {
        .settings$circular_plot_size <- input$circular_plot_size
        .settings$circular_plot_n_groups <- input$circular_plot_n_groups
        .settings$circular_plot_n_regions <- input$circular_plot_n_regions
        .settings$circular_plot_tension <- input$circular_plot_tension

        if (.gff_is_not_null()) {
            .precompute_circular_plot_data()
            .add_gene_info_to_outliers()
        }

        .generate_outliers_table(input)
        .render_tree_plot(input)
        .render_manhattan_plot(input)
        .render_circular_plot(input)

        shinyjs::show("settings_div")
        output$settings_applied <- shiny::renderText({"Done!"})
    })


    # Hide data read / settings applied text when moving away from the tab.
    # (This doesn't work.)
    shiny::observeEvent(input$tabs, {
        shinyjs::hide("settings_div")
        shinyjs::hide("reading_data_div")
    })

}
