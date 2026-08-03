test_that(".read_data loads the Maela example files and precomputes circular plot data", {
    maela_file <- function(name) {
        data.frame(
            datapath = system.file("extdata",
                                   name,
                                   package = "GWESExplorer",
                                   mustWork = TRUE),
            name = name
        )
    }

    data <- new.env(parent = emptyenv())
    result <- .read_data(
        data = data,
        outliers_file = maela_file("maela_outliers.outliers"),
        tree_file = maela_file("maela_tree.nex"),
        fasta_file = maela_file("maela_fasta.fasta"),
        loci_file = maela_file("maela_loci.loci"),
        phenotype_file = maela_file("maela_phenotypes.csv"),
        gff_file = maela_file("maela_gff.gff3")
    )

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_identical(nrow(data$outliers), 21627L)
    expect_identical(nrow(data$outliers_direct), 172L)
    expect_false(is.null(data$tree))
    expect_identical(dim(data$msa), c(3042L, 304L))
    expect_identical(dim(data$phenotype), c(3085L, 2L))
    expect_identical(data$outliers_direct$Pos_1_feature[[1L]], "SPN23F_RS08380")
    expect_identical(data$outliers_direct$Pos_2_feature[[1L]], "SPN23F_RS09935")

    dataset_names <- vapply(data$edges$data, `[[`, character(1), "name")
    expect_true(all(c("feature_data", "position_data", "position_links") %in% dataset_names))

    feature_data <- data$edges$data[[match("feature_data", dataset_names)]]$values
    position_data <- data$edges$data[[match("position_data", dataset_names)]]$values
    position_links <- data$edges$data[[match("position_links", dataset_names)]]$values

    expect_identical(nrow(feature_data), nrow(data$gff))
    expect_identical(feature_data$feature_row, seq_len(nrow(data$gff)))
    expect_true(all(c("features_linked_to", "n_features_linked_to", "n_outliers", "n_self_links") %in%
                    names(feature_data)))
    expect_identical(nrow(position_data), 272L)
    expect_identical(nrow(position_links), 2L * nrow(data$outliers_direct))

    direct_link_rows <- seq.int(1L, nrow(position_links), by = 2L)
    reverse_link_rows <- direct_link_rows + 1L

    expect_identical(position_links$feature_row_1[direct_link_rows],
                     data$outliers_direct$Pos_1_feature_row)
    expect_identical(position_links$feature_row_2[direct_link_rows],
                     data$outliers_direct$Pos_2_feature_row)
    expect_identical(position_links$feature_row_1[reverse_link_rows],
                     data$outliers_direct$Pos_2_feature_row)
    expect_identical(position_links$feature_row_2[reverse_link_rows],
                     data$outliers_direct$Pos_1_feature_row)

    # Vega indices are 0-based, so add one before using them to select R rows.
    expect_identical(position_data$position[position_links$position_data_index_1[direct_link_rows] + 1L],
                     data$outliers_direct$Pos_1)
    expect_identical(position_data$position[position_links$position_data_index_2[direct_link_rows] + 1L],
                     data$outliers_direct$Pos_2)
    expect_identical(position_data$position[position_links$position_data_index_1[reverse_link_rows] + 1L],
                     data$outliers_direct$Pos_2)
    expect_identical(position_data$position[position_links$position_data_index_2[reverse_link_rows] + 1L],
                     data$outliers_direct$Pos_1)

    expected_self_links <- sum(data$outliers_direct$Pos_1_feature_row ==
                               data$outliers_direct$Pos_2_feature_row)
    expect_equal(sum(feature_data$n_self_links), expected_self_links)
    expect_equal(sum(feature_data$n_outliers),
                 2L * nrow(data$outliers_direct) - expected_self_links)

    mark_names <- vapply(data$edges$marks,
                         function(mark) if (is.null(mark$name)) "" else mark$name,
                         character(1))
    expect_true(all(c("feature_text_1", "feature_text_2", "position_symbol_1", "position_symbol_2") %in%
                    mark_names))

    signal_names <- vapply(data$edges$signals, `[[`, character(1), "name")
    expect_true(all(c("active_feature", "selected_feature_1", "selected_feature_2",
                      "selected_position_1", "selected_position_2") %in% signal_names))
})

