# Builds a small circular-plot test data set.
.make_precomputed_circular_plot_test_data <- function() {
    # The circular plot normally has 120 regions. Use three so each test GFF row
    # occupies its own region, then restore the global settings when this helper returns.
    previous_region_group_count <- .settings$circular_plot_region_group_count
    previous_regions_per_group_count <- .settings$circular_plot_regions_per_group_count
    previous_region_count <- .settings$circular_plot_region_count
    on.exit({
        .settings$circular_plot_region_group_count <- previous_region_group_count
        .settings$circular_plot_regions_per_group_count <- previous_regions_per_group_count
        .settings$circular_plot_region_count <- previous_region_count
    })

    .settings$circular_plot_region_group_count <- 1L
    .settings$circular_plot_regions_per_group_count <- 3L
    .settings$circular_plot_region_count <- 3L

    # This represents GFF data after loading has inserted an IGR between two CDS rows.
    data <- new.env(parent = emptyenv())
    data$gff <- data.frame(
        start = c(1, 101, 201),
        end = c(100, 200, 300),
        Name = c("cds1", "IGR_0k", "cds2")
    )

    # The MI range for circular plot weights comes from all the outliers.
    # The only direct outlier links a position in the first CDS to a position in the generated IGR.
    data$outliers <- data.frame(
        Pos_1 = c(250L, 50L, 100L),
        Pos_2 = c(300L, 150L, 200L),
        MI = c(1, 0.8, 0.2),
        Direct = c(FALSE, TRUE, FALSE)
    )
    data$outliers_direct <- data$outliers[data$outliers$Direct, ]
    data$circular_plot_spec <- NULL

    # This mutates data by mapping the endpoints to GFF rows and building the Vega specification.
    .precompute_circular_plot_data(data)

    return(data)
}

# Finds a Vega data set by name.
.get_vega_dataset <- function(spec, dataset_name) {
    matching_datasets <- which(vapply(spec$data,
                                      function(dataset) identical(dataset$name, dataset_name),
                                      logical(1)))
    testthat::expect_length(matching_datasets, 1L)
    return(spec$data[[matching_datasets[[1L]]]])
}

# Finds the formula that writes a given field.
.get_vega_formula_expression <- function(dataset, output_field) {
    matching_formulas <- which(vapply(dataset$transform,
                                      function(transform) identical(transform$as, output_field),
                                      logical(1)))
    testthat::expect_length(matching_formulas, 1L)
    return(dataset$transform[[matching_formulas[[1L]]]]$expr)
}

test_that(".rescale_values maps values to the target range", {
    expect_equal(.rescale_values(c(2, 4, 6), 0.5, 1, 2, 6), c(0.5, 0.75, 1))
    expect_equal(.rescale_values(c(0, 0.25, 0.5, 0.75, 1), 0.1, 0.9, 0, 1),
                 c(0.1, 0.3, 0.5, 0.7, 0.9))
    expect_equal(.rescale_values(rep(4, 3), 0.5, 1, 0, 10), rep(0.7, 3))
})

test_that(".rescale_values rejects equal, reversed and non-finite range limits", {
    invalid_ranges <- list(c(1, 1), c(2, 1), c(NA_real_, 1), c(0, Inf), c(-Inf, 1))
    for (limits in invalid_ranges) {
        expect_error(.rescale_values(0.5, 0.5, 1, limits[1], limits[2]), "Source range")
        expect_error(.rescale_values(0.5, limits[1], limits[2], 0, 1), "Target range")
    }
})

test_that(".precompute_circular_plot_data maps each outlier to its 1-based feature row", {
    data <- .make_precomputed_circular_plot_test_data()

    expect_identical(data$gff$feature_region_ids, c(1L, 2L, 3L))
    expect_identical(data$outliers_direct$Pos_1_feature_row, 1L)
    expect_identical(data$outliers_direct$Pos_2_feature_row, 2L)
    expect_identical(data$outliers_direct$Pos_1_feature, "cds1")
    expect_identical(data$outliers_direct$Pos_2_feature, "IGR_0k")
    expect_identical(data$outliers_direct$Pos_1_region, 1L)
    expect_identical(data$outliers_direct$Pos_2_region, 2L)
})

test_that(".precompute_circular_plot_data creates feature and position data", {
    data <- .make_precomputed_circular_plot_test_data()
    feature_data <- .get_vega_dataset(data$circular_plot_spec, "feature_data")$values
    position_data <- .get_vega_dataset(data$circular_plot_spec, "position_data")$values
    region_links <- .get_vega_dataset(data$circular_plot_spec, "region_links")$values

    expect_named(feature_data,
                 c("feature_row", "feature", "region", "position_fraction", "position_step_size", "start", "end",
                   "features_linked_to", "linked_feature_count", "outlier_count",
                   "self_link_count", "features_linked_to_line_count"))
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
    expect_identical(feature_data$linked_feature_count, c(1L, 1L, 0L))
    expect_identical(feature_data$outlier_count, c(1L, 1L, 0L))
    expect_identical(feature_data$self_link_count, c(0L, 0L, 0L))
    expect_identical(feature_data$features_linked_to_line_count, c(3L, 3L, 0L))

    expected_position_fields <- c("position", "feature_row", "region", "weight", "position_in_feature")
    expect_named(position_data[expected_position_fields], expected_position_fields)
    expect_identical(position_data$position, c(50L, 150L))
    expect_identical(position_data$feature_row, c(1L, 2L))
    expect_identical(position_data$region, c(1L, 2L))
    # The MI 0.8 of the direct link must be reweighted according to the full outliers MI range.
    expect_equal(position_data$weight, c(0.875, 0.875))
    expect_equal(region_links$weight, 0.9375)
    expect_equal(position_data$position_in_feature, c(491 / 990, 491 / 990))
})

test_that(".precompute_circular_plot_data creates links with 1-based feature rows and 0-based position-data indices", {
    data <- .make_precomputed_circular_plot_test_data()
    position_links <- .get_vega_dataset(data$circular_plot_spec, "position_links")$values

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
    expect_equal(position_links$weight, c(0.875, 0.875))
})

test_that(paste(
    ".precompute_circular_plot_data creates Vega lookups from 1-based feature rows",
    "and 0-based position-data indices"
), {
    data <- .make_precomputed_circular_plot_test_data()
    position_data <- .get_vega_dataset(data$circular_plot_spec, "position_data")
    position_links <- .get_vega_dataset(data$circular_plot_spec, "position_links")

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
