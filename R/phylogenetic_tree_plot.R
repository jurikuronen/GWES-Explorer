# Creates a Shiny renderer for the phylogenetic tree plot.
# The plot is reactive and redraws when its settings, phenotype selection or selected outliers change.
.render_phylogenetic_tree_plot <- function(data, input) {
    shiny::renderPlot({
        .create_phylogenetic_tree_plot(data, input)
    })
}

# Creates the base tree plot.
# If a phenotype is selected, additionally draws a phenotype heatmap on the right of the base tree plot.
.create_base_tree_plot <- function(data, input) {
    base_tree_plot <- ggtree::ggtree(data$tree)

    # Convert the plot data to a data frame to prevent invalid-tree warnings from gheatmap().
    base_tree_plot$data <- as.data.frame(base_tree_plot$data)

    selected_phenotype_column <- as.numeric(input$select_phenotype)

    # Zero represents the special value "No phenotype selected".
    if (selected_phenotype_column != 0) {
        base_tree_plot <- ggtree::gheatmap(
            p = base_tree_plot,
            # R would turn the selected column into a vector without drop = FALSE; gheatmap() requires a data frame.
            data = data$phenotype[, selected_phenotype_column, drop = FALSE],
            # Draw the column to the right of the tree with no gap.
            offset = 0,
            width = 0.1,
            # Remove the default white cell outlines that make the heatmap colors look faded.
            color = NA,
            # Position the phenotype name text below the column.
            colnames_angle = input$tree_heatmap_column_label_angle,
            colnames_offset_x = -input$tree_heatmap_column_label_offset_x,
            colnames_offset_y = -input$tree_heatmap_column_label_offset_y,
            font.size = input$tree_heatmap_column_label_font_size,
            # Remove the unnecessary default "value" title.
            legend_title = NULL)

        # Set the phenotype legend text and key sizes.
        base_tree_plot <- base_tree_plot + theme(legend.text = element_text(size = input$tree_legend_text_size),
                                                 legend.key.size = unit(input$tree_legend_key_size, "cm"))
    }

    return(base_tree_plot)
}

# Creates the complete phylogenetic tree plot.
# If rows are selected in the outlier table, also draws their Pos_1 and Pos_2 MSA columns as a heatmap.
.create_phylogenetic_tree_plot <- function(data, input) {
    # The plot requires both a tree and an MSA.
    if (is.null(data$tree) || is.null(data$msa)) {
        return(NULL)
    }

    tree_plot <- .create_base_tree_plot(data, input)
    selected_outlier_rows <- input$outliers_table_rows_selected
    msa_positions_selected <- length(selected_outlier_rows) > 0

    if (msa_positions_selected) {
        selected_outlier_positions <- data$outliers_direct[selected_outlier_rows, c("Pos_1", "Pos_2")]
        selected_msa_positions <- as.character(sort(unique(unlist(selected_outlier_positions, use.names = FALSE))))

        nucleotide_colors <- viridis::viridis(4)
        msa_color_by_symbol <- c(
            "-" = "white",
            "A" = nucleotide_colors[[1L]],
            "C" = nucleotide_colors[[2L]],
            "G" = nucleotide_colors[[3L]],
            "N" = "white",
            "T" = nucleotide_colors[[4L]]
        )

        # Start a new fill scale so the MSA colors do not replace the phenotype colors.
        tree_plot <- ggtree::gheatmap(
            p = tree_plot + ggnewscale::new_scale_fill(),
            # Specifying drop = FALSE is not strictly necessary, since Pos_1 and Pos_2 should not be identical, but keep
            # it as defensive programming.
            data = data$msa[, selected_msa_positions, drop = FALSE],
            # Draw the MSA columns to the right of the tree with a small gap.
            offset = 0.25,
            width = 0.2,
            # Remove the default white cell outlines that make the heatmap colors look faded.
            color = NA,
            # Position the MSA position labels below their columns.
            colnames_angle = input$tree_heatmap_column_label_angle,
            colnames_offset_x = -input$tree_heatmap_column_label_offset_x,
            colnames_offset_y = -input$tree_heatmap_column_label_offset_y,
            font.size = input$tree_heatmap_column_label_font_size,
            # Remove the unnecessary default "value" title.
            legend_title = NULL
        )

        # Apply the MSA symbol colors.
        # Suppress also the fill-scale warning when replacing gheatmap()'s default fill scale.
        tree_plot <- suppressMessages(
            tree_plot + scale_fill_manual(values = msa_color_by_symbol,
                                          # Draw other MSA symbols in grey.
                                          na.value = "grey50")
        )

        # Set the MSA legend text and key sizes.
        tree_plot <- tree_plot + theme(legend.text = element_text(size = input$tree_legend_text_size),
                                       legend.key.size = unit(input$tree_legend_key_size, "cm"))
    }

    # Zero represents the special value "No phenotype selected".
    phenotype_selected <- as.numeric(input$select_phenotype) != 0

    # Since the rotated heatmap column labels extend beyond the plotting area, disable clipping and increase the bottom
    # margin so that the labels fit.
    if (phenotype_selected || msa_positions_selected) {
        tree_plot <- tree_plot +
            # The margins are ordered top, right, bottom and left.
            theme(plot.margin = theme_get()$plot.margin *
                      c(1, 1, input$tree_plot_bottom_margin_multiplier, 1)) +
            coord_cartesian(clip = "off")
    }

    return(tree_plot)
}
