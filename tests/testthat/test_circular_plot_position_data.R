test_that(".create_position_data keeps the highest-MI row for each position", {
    data <- new.env(parent = emptyenv())
    data$gff <- data.frame(
        start = c(1, 102, 203),
        end = c(101, 202, 303),
        Name = c("gene1", "gene2", "gene3")
    )
    data$outliers_direct <- data.frame(
        Pos_1 = c(11L, 11L, 96L),
        Pos_2 = c(152L, 253L, 208L),
        Pos_1_feature_row = c(1L, 1L, 1L),
        Pos_2_feature_row = c(2L, 3L, 3L),
        Pos_1_region = c(1L, 1L, 1L),
        Pos_2_region = c(2L, 3L, 3L),
        MI = c(0.4, 0.9, 0.6)
    )

    result <- .create_position_data(data$outliers_direct, data$gff)

    # Position 11 occurs in rows 1 and 2. Row 2 is retained because its MI is higher.
    # The remaining positions are sorted by their circular-plot region.
    expect_identical(result$position, c(11L, 96L, 152L, 253L, 208L))
    expect_identical(result$feature_row, c(1L, 1L, 2L, 3L, 3L))
    expect_identical(result$region, c(1L, 1L, 2L, 3L, 3L))

    # Positions within each feature are rescaled from [0, 1] to [0.1, 0.9].
    expect_equal(result$position_in_feature, c(0.18, 0.86, 0.5, 0.5, 0.14))
})
