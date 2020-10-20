.read_phenotype_idx <- function(select_phenotype) {
    phenotype_idx <- as.numeric(select_phenotype)
    if (is.na(phenotype_idx)) phenotype_idx <- 0
    return(phenotype_idx)
}

.valid_phenotype_idx <- function(phenotype_idx) { return(phenotype_idx != 0) }

.generate_phylogenetic_object <- function(input) {
    phylo_object <- NULL
    if (.tree_is_not_null()) {
        phenotype_idx <- .read_phenotype_idx(input$select_phenotype)
        phylo_object <- ggtree::ggtree(.data$tree)
        if (.valid_phenotype_idx(phenotype_idx)) {
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
            return(shiny::renderPlot(phylo_object))
        } else {
            # Append a heatmap of a matrix to the right side of the phylogenetic tree.
            ind <- as.character(sort(unique(as.vector(unlist(.data$outliers_direct[selected_rows, c("Pos_1", "Pos_2")])))))
            select <- c("-", "A", "C", "G", "N", "T") %in% unique(as.vector(.data$msa[, ind]))
            col <- c("white", viridis::viridis(4)[1:3], "white", viridis::viridis(4)[4])[select]
            return(shiny::renderPlot({ggtree::gheatmap(
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
            }))
        }
    } else {
        return(NULL)
    }
}