test_that(".tree_plot returns the base tree when no rows are selected", {
    data <- new.env(parent = emptyenv())
    data$tree <- ape::read.tree(text = "(A:1,B:1);")
    data$msa <- matrix(c("A", "C"), nrow = 2)
    input <- list(
        select_phenotype = 0,
        outliers_table_rows_selected = integer()
    )

    result <- .tree_plot(data, input)

    expect_false(is.null(result))
})
