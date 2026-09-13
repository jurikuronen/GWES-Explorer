test_that(".add_link_info_to_feature_data builds tooltips for links between different features", {
    feature_data <- data.frame(
        feature_row = 1:4,
        start = c(1, 101, 201, 301),
        end = c(100, 200, 300, 400),
        feature = c("alpha", "beta", "gamma", "delta")
    )
    position_links <- data.frame(
        feature_row_1 = c(1L, 2L, 1L, 3L, 1L, 2L),
        feature_row_2 = c(2L, 1L, 3L, 1L, 2L, 1L),
        MI = c(0.8, 0.8, 0.9, 0.9, 0.4, 0.4)
    )

    result <- .add_link_info_to_feature_data(feature_data, position_links)

    # Alpha's gamma links are listed before its beta links because gamma has the higher maximum MI.
    expect_identical(
        result$features_linked_to[[1]],
        c("Linked to:", "gamma (201-300)", "0.9",
          "beta (101-200)", "0.8", "0.4")
    )
    expect_identical(
        result$features_linked_to[[2]],
        c("Linked to:", "alpha (1-100)", "0.8", "0.4")
    )
    expect_identical(
        result$features_linked_to[[3]],
        c("Linked to:", "alpha (1-100)", "0.9")
    )

    # Delta has no links, so it retains an empty tooltip and zero counts.
    expect_null(result$features_linked_to[[4]])
    expect_identical(result$linked_feature_count, c(2L, 1L, 1L, 0L))
    expect_identical(result$outlier_count, c(3L, 2L, 1L, 0L))
    expect_identical(result$self_link_count, c(0L, 0L, 0L, 0L))
    expect_identical(result$features_linked_to_line_count, c(6L, 4L, 3L, 0L))
})

test_that(".add_link_info_to_feature_data records self-links separately", {
    feature_data <- data.frame(
        feature_row = 1:2,
        start = c(1, 101),
        end = c(100, 200),
        feature = c("alpha", "beta")
    )
    position_links <- data.frame(
        feature_row_1 = c(1L, 1L, 1L, 1L, 1L, 2L),
        feature_row_2 = c(1L, 1L, 1L, 1L, 2L, 1L),
        MI = c(0.9, 0.9, 0.4, 0.4, 0.7, 0.7)
    )

    result <- .add_link_info_to_feature_data(feature_data, position_links)

    expect_identical(result$features_linked_to[[1]],
                     c("Linked to:", "beta (101-200)", "0.7"))
    expect_identical(result$features_linked_to[[2]],
                     c("Linked to:", "alpha (1-100)", "0.7"))
    expect_identical(result$linked_feature_count, c(1L, 1L))
    expect_identical(result$outlier_count, c(3L, 1L))
    expect_identical(result$self_link_count, c(2L, 0L))
    expect_identical(result$features_linked_to_line_count, c(3L, 3L))
})
