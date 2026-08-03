test_that(".add_link_info_to_feature_data builds tooltips for links between different features", {
    data <- new.env(parent = emptyenv())
    data$gff <- data.frame(
        start = c(1, 101, 201, 301),
        end = c(100, 200, 300, 400),
        Name = c("alpha", "beta", "gamma", "delta")
    )
    data$outliers_direct <- data.frame(
        Pos_1_feature_row = c(1L, 1L, 1L),
        Pos_2_feature_row = c(2L, 3L, 2L)
    )
    feature_data <- data.frame(feature_row = 1:4)
    position_links <- data.frame(
        feature_row_1 = c(1L, 2L, 1L, 3L, 1L, 2L),
        feature_row_2 = c(2L, 1L, 3L, 1L, 2L, 1L),
        MI = c(0.8, 0.8, 0.9, 0.9, 0.4, 0.4)
    )

    result <- .add_link_info_to_feature_data(data, feature_data, position_links)

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
    expect_identical(result$n_features_linked_to, c(2L, 1L, 1L, 0L))
    expect_identical(result$n_outliers, c(3L, 2L, 1L, 0L))
    expect_identical(result$n_self_links, c(0L, 0L, 0L, 0L))
    expect_identical(result$length, c(6L, 4L, 3L, 0L))
})

test_that(".add_link_info_to_feature_data records self-links separately", {
    data <- new.env(parent = emptyenv())
    data$gff <- data.frame(
        start = c(1, 101),
        end = c(100, 200),
        Name = c("alpha", "beta")
    )
    data$outliers_direct <- data.frame(
        Pos_1_feature_row = c(1L, 1L, 1L),
        Pos_2_feature_row = c(1L, 1L, 2L)
    )
    feature_data <- data.frame(feature_row = 1:2)
    position_links <- data.frame(
        feature_row_1 = c(1L, 1L, 1L, 1L, 1L, 2L),
        feature_row_2 = c(1L, 1L, 1L, 1L, 2L, 1L),
        MI = c(0.9, 0.9, 0.4, 0.4, 0.7, 0.7)
    )

    result <- .add_link_info_to_feature_data(data, feature_data, position_links)

    expect_identical(result$features_linked_to[[1]],
                     c("Linked to:", "beta (101-200)", "0.7"))
    expect_identical(result$features_linked_to[[2]],
                     c("Linked to:", "alpha (1-100)", "0.7"))
    expect_identical(result$n_features_linked_to, c(1L, 1L))
    expect_identical(result$n_outliers, c(3L, 1L))
    expect_identical(result$n_self_links, c(2L, 0L))
    expect_identical(result$length, c(3L, 3L))
})

