.tree_plot_test_data <- function() {
    sample_names <- c("6673_8#6", "6631_2#6")

    data <- new.env(parent = emptyenv())
    data$tree <- ape::read.tree(text = paste0("(", paste0(sample_names, ":1", collapse = ","), ");"))
    data$msa <- matrix(
        c("A", "C", "G", "T"),
        nrow = 2L,
        dimnames = list(sample_names, c("1601843", "1891918"))
    )
    data$phenotype <- data.frame(
        Resistance = c("Non-susceptible", "Susceptible"),
        row.names = sample_names
    )
    data$outliers_direct <- data.frame(Pos_1 = 1601843L, Pos_2 = 1891918L)
    data
}

.tree_plot_test_input <- function(phenotype_column = 0L, selected_outlier_rows = integer()) {
    list(
        select_phenotype = phenotype_column,
        outliers_table_rows_selected = selected_outlier_rows,
        tree_heatmap_column_label_angle = .settings$tree_heatmap_column_label_angle,
        tree_heatmap_column_label_offset_x = .settings$tree_heatmap_column_label_offset_x,
        tree_heatmap_column_label_offset_y = .settings$tree_heatmap_column_label_offset_y,
        tree_heatmap_column_label_font_size = .settings$tree_heatmap_column_label_font_size,
        tree_legend_text_size = .settings$tree_legend_text_size,
        tree_legend_key_size = .settings$tree_legend_key_size,
        tree_plot_bottom_margin_multiplier = .settings$tree_plot_bottom_margin_multiplier
    )
}

# gheatmap() adds one tile layer for each heatmap. Count those layers in the finished plot.
.count_tree_plot_heatmaps <- function(plot) {
    sum(vapply(
        plot$layers,
        function(layer) inherits(layer$geom, "GeomTile"),
        logical(1L)
    ))
}

test_that(".create_phylogenetic_tree_plot returns the base tree when no rows are selected", {
    data <- .tree_plot_test_data()
    input <- .tree_plot_test_input()

    result <- .create_phylogenetic_tree_plot(data, input)

    expect_false(is.null(result))
})

test_that(".create_phylogenetic_tree_plot draws the phenotype heatmap without warnings", {
    data <- .tree_plot_test_data()
    input <- .tree_plot_test_input(phenotype_column = 1L)

    expect_silent({
        result <- .create_phylogenetic_tree_plot(data, input)
        ggplot2::ggplot_build(result)
    })

    expect_identical(.count_tree_plot_heatmaps(result), 1L)
})

test_that(".create_phylogenetic_tree_plot draws the MSA heatmap without warnings", {
    data <- .tree_plot_test_data()
    input <- .tree_plot_test_input(selected_outlier_rows = 1L)

    expect_silent({
        result <- .create_phylogenetic_tree_plot(data, input)
        ggplot2::ggplot_build(result)
    })

    expect_identical(.count_tree_plot_heatmaps(result), 1L)
})

test_that(".create_phylogenetic_tree_plot draws other MSA symbols in grey", {
    data <- .tree_plot_test_data()
    # Specify any unlisted MSA symbol.
    data$msa[1L, 1L] <- "R"
    input <- .tree_plot_test_input(selected_outlier_rows = 1L)

    result <- .create_phylogenetic_tree_plot(data, input)
    built_plot <- ggplot2::ggplot_build(result)
    heatmap_layer <- which(vapply(
        result$layers,
        function(layer) inherits(layer$geom, "GeomTile"),
        logical(1L)
    ))

    expect_true("grey50" %in% built_plot$data[[heatmap_layer]]$fill)
})

test_that(".create_phylogenetic_tree_plot draws both heatmaps without warnings", {
    data <- .tree_plot_test_data()
    input <- .tree_plot_test_input(
        phenotype_column = 1L,
        selected_outlier_rows = 1L
    )

    expect_silent({
        result <- .create_phylogenetic_tree_plot(data, input)
        ggplot2::ggplot_build(result)
    })

    expect_identical(.count_tree_plot_heatmaps(result), 2L)
})
