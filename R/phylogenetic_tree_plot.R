.render_tree_plot <- function(input) {
    shiny::renderPlot({
        if (is.null(.data$tree) || is.null(.data$msa)) {
            return(NULL)
        }
        .tree_plot(input)
    })
}

.generate_phylogenetic_object <- function(input) {
    phylo_object <- NULL
    if (!is.null(.data$tree)) {
        phenotype_idx <- ifelse(is.na(as.numeric(input$select_phenotype)), 0, as.numeric(input$select_phenotype))
        phylo_object <- ggtree::ggtree(.data$tree)
        if (phenotype_idx != 0) {
            phylo_object <- ggtree::gheatmap(
                p = phylo_object,
                data = .data$phenotype[, phenotype_idx, drop = FALSE],
                offset = 0,
                width = 0.1,
                color = NA,
                colnames_angle = input$tree_label_angle,
                colnames_offset_x = -input$tree_label_offset_x,
                colnames_offset_y = -input$tree_label_offset_y,
                font.size = input$tree_label_fs) +
                theme(legend.text = element_text(size = input$tree_legend_fs),
                      legend.key.size = unit(input$tree_legend_size, "cm"))
        }
    }
    return(phylo_object)
}

.tree_plot <- function(input) {
    if (!is.null(.data$tree) && !is.null(.data$msa)) {
        phylo_object <- .generate_phylogenetic_object(input)
        selected_rows <- input$outliers_table_rows_selected
        if (is.null(selected_rows)) {
            return(phylo_object)
        } else {
            # Append a heatmap of a matrix to the right side of the phylogenetic tree.
            ind <- as.character(sort(unique(as.vector(unlist(.data$outliers_direct[selected_rows, c("Pos_1", "Pos_2")])))))
            select <- c("-", "A", "C", "G", "N", "T") %in% unique(as.vector(.data$msa[, ind]))
            col <- c("white", viridis::viridis(4)[1:3], "white", viridis::viridis(4)[4])[select]
            return(ggtree::gheatmap(
                p = phylo_object + ggnewscale::new_scale_fill(),
                data = .data$msa[, ind],
                offset = 0.25,
                width = 0.2,
                color = NA,
                colnames_angle = input$tree_label_angle,
                colnames_offset_x = -input$tree_label_offset_x,
                colnames_offset_y = -input$tree_label_offset_y,
                font.size = input$tree_label_fs
                ) + scale_fill_manual(values = col) +
                    theme(legend.text = element_text(size = input$tree_legend_fs),
                          legend.key.size = unit(input$tree_legend_size, "cm"),
                          plot.margin = theme_get()$plot.margin * c(1, 1, input$tree_plot_margin_multiplier, 1)) +
                    coord_cartesian(clip = "off")
            )
        }
    }
    return(NULL)
}
