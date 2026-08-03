# Builds a small circular-plot test data set.
.make_precomputed_circular_plot_test_data <- function() {
    # The circular plot normally has 120 regions. Use three so each test GFF row
    # occupies its own region, then restore the global settings when this helper returns.
    previous_n_groups <- .settings$circular_plot_n_groups
    previous_n_regions_per_group <- .settings$circular_plot_n_regions_per_group
    on.exit({
        .settings$circular_plot_n_groups <- previous_n_groups
        .settings$circular_plot_n_regions_per_group <- previous_n_regions_per_group
    })

    .settings$circular_plot_n_groups <- 1L
    .settings$circular_plot_n_regions_per_group <- 3L

    # This represents GFF data after loading has inserted an IGR between two CDS rows.
    data <- new.env(parent = emptyenv())
    data$gff <- data.frame(
        start = c(1, 101, 201),
        end = c(100, 200, 300),
        Name = c("cds1", "IGR_0k", "cds2")
    )

    # Link a position in the first CDS to a position in the generated IGR.
    data$outliers_direct <- data.frame(
        Pos_1 = 50L,
        Pos_2 = 150L,
        MI = 0.8
    )
    data$edges <- NULL

    # This mutates data by mapping the endpoints to GFF rows and building the Vega specification.
    .precompute_circular_plot_data(data)

    return(data)
}

# Finds a Vega data set by name.
.get_vega_dataset <- function(spec, dataset_name) {
    matching_datasets <- which(vapply(spec$data,
                                      function(dataset) identical(dataset$name, dataset_name),
                                      logical(1)))
    expect_length(matching_datasets, 1L)
    return(spec$data[[matching_datasets[[1L]]]])
}

# Finds the formula that writes a given field.
.get_vega_formula_expression <- function(dataset, output_field) {
    matching_formulas <- which(vapply(dataset$transform,
                                      function(transform) identical(transform$as, output_field),
                                      logical(1)))
    expect_length(matching_formulas, 1L)
    return(dataset$transform[[matching_formulas[[1L]]]]$expr)
}

test_that(".rescale_weights scales varying weights", {
    expect_equal(.rescale_weights(c(2, 4, 6), 0.5, 1), c(0.5, 0.75, 1))
})

test_that(".rescale_weights scales equal weights", {
    expect_equal(.rescale_weights(rep(4, 3), 0.5, 1), rep(0.75, 3))
})

test_that(".precompute_circular_plot_data maps each outlier to its 1-based feature row", {
    data <- .make_precomputed_circular_plot_test_data()

    expect_identical(data$gff$feature_regions, c(1L, 2L, 3L))
    expect_identical(data$outliers_direct$Pos_1_feature_row, 1L)
    expect_identical(data$outliers_direct$Pos_2_feature_row, 2L)
    expect_identical(data$outliers_direct$Pos_1_feature, "cds1")
    expect_identical(data$outliers_direct$Pos_2_feature, "IGR_0k")
    expect_identical(data$outliers_direct$Pos_1_region, 1L)
    expect_identical(data$outliers_direct$Pos_2_region, 2L)
})

test_that(".precompute_circular_plot_data creates feature and position data", {
    data <- .make_precomputed_circular_plot_test_data()
    feature_data <- .get_vega_dataset(data$edges, "feature_data")$values
    position_data <- .get_vega_dataset(data$edges, "position_data")$values

    expect_named(feature_data,
                 c("feature_row", "feature", "region", "angle_step", "step_size", "start", "end",
                   "features_linked_to", "n_features_linked_to", "n_outliers",
                   "n_self_links", "length"))
    expect_identical(feature_data$feature_row, c(1L, 2L, 3L))
    expect_identical(as.character(feature_data$feature), c("cds1", "IGR_0k", "cds2"))
    expect_identical(feature_data$region, c(1L, 2L, 3L))
    expect_identical(feature_data$start, c(1, 101, 201))
    expect_identical(feature_data$end, c(100, 200, 300))

    expect_identical(feature_data$features_linked_to[[1]],
                     c("Linked to:", "IGR_0k (101-200)", "0.8"))
    expect_identical(feature_data$features_linked_to[[2]],
                     c("Linked to:", "cds1 (1-100)", "0.8"))
    expect_null(feature_data$features_linked_to[[3]])
    expect_identical(feature_data$n_features_linked_to, c(1L, 1L, 0L))
    expect_identical(feature_data$n_outliers, c(1L, 1L, 0L))
    expect_identical(feature_data$n_self_links, c(0L, 0L, 0L))
    expect_identical(feature_data$length, c(3L, 3L, 0L))

    expected_position_fields <- c("position", "feature_row", "region", "weight", "position_in_feature")
    expect_named(position_data[expected_position_fields], expected_position_fields)
    expect_identical(position_data$position, c(50L, 150L))
    expect_identical(position_data$feature_row, c(1L, 2L))
    expect_identical(position_data$region, c(1L, 2L))
    expect_equal(position_data$weight, c(0.75, 0.75))
    expect_equal(position_data$position_in_feature, c(49 / 99, 49 / 99))
})

test_that(".precompute_circular_plot_data creates links with 1-based feature rows and 0-based position-data indices", {
    data <- .make_precomputed_circular_plot_test_data()
    position_links <- .get_vega_dataset(data$edges, "position_links")$values

    expect_named(position_links,
                 c("region_1", "region_2", "feature_row_1", "feature_row_2",
                   "position_data_index_1", "position_data_index_2", "MI", "weight"))
    expect_identical(position_links$region_1, c(1L, 2L))
    expect_identical(position_links$region_2, c(2L, 1L))
    # These values are 1-based rows in data$gff.
    expect_identical(position_links$feature_row_1, c(1L, 2L))
    expect_identical(position_links$feature_row_2, c(2L, 1L))
    # These values are 0-based indices into Vega's position data.
    expect_identical(position_links$position_data_index_1, c(0L, 1L))
    expect_identical(position_links$position_data_index_2, c(1L, 0L))
    expect_identical(position_links$MI, c(0.8, 0.8))
    expect_equal(position_links$weight, c(0.75, 0.75))
})

test_that(".precompute_circular_plot_data creates Vega lookups from 1-based feature rows and 0-based position-data indices", {
    data <- .make_precomputed_circular_plot_test_data()
    position_data <- .get_vega_dataset(data$edges, "position_data")
    position_links <- .get_vega_dataset(data$edges, "position_links")

    expect_identical(
        .get_vega_formula_expression(position_data, "feature"),
        "data('feature_data')[datum.feature_row - 1].feature"
    )
    expect_identical(
        .get_vega_formula_expression(position_links, "x"),
        "data('position_data')[datum.position_data_index_1].x_1"
    )
    expect_identical(
        .get_vega_formula_expression(position_links, "x2"),
        "data('position_data')[datum.position_data_index_2].x_2"
    )
})
