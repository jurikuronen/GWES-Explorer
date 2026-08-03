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

test_that(".cpp_create_bidirectional_position_links creates both directions for every link", {
    outliers_direct <- data.frame(
        Pos_1 = c(300L, 100L),
        Pos_2 = c(200L, 400L),
        Pos_1_region = c(3L, 1L),
        Pos_2_region = c(2L, 4L),
        Pos_1_gene = c(30L, 10L),
        Pos_2_gene = c(20L, 40L),
        MI = c(0.9, 0.4)
    )
    position_data <- data.frame(name = c(400L, 200L, 300L, 100L))

    result <- .cpp_create_bidirectional_position_links(outliers_direct, position_data)

    expect_named(result, c("region_1", "region_2", "gene_1", "gene_2",
                           "pos_data_idx_1", "pos_data_idx_2", "MI"))

    # Each input row is followed by the same link with its two endpoints reversed.
    expect_identical(result$region_1, c(3L, 2L, 1L, 4L))
    expect_identical(result$region_2, c(2L, 3L, 4L, 1L))
    expect_identical(result$gene_1, c(30L, 20L, 10L, 40L))
    expect_identical(result$gene_2, c(20L, 30L, 40L, 10L))
    expect_identical(result$MI, c(0.9, 0.9, 0.4, 0.4))

    # The position-data rows are 400, 200, 300, 100, which Vega addresses as rows 0, 1, 2, 3.
    expect_identical(result$pos_data_idx_1, c(2L, 1L, 3L, 0L))
    expect_identical(result$pos_data_idx_2, c(1L, 2L, 0L, 3L))
})

test_that(".cpp_create_bidirectional_position_links rejects unequal outlier column lengths", {
    outliers_direct <- data.frame(
        Pos_1 = c(100L, 300L),
        Pos_2 = c(200L, 400L),
        Pos_1_region = c(1L, 3L),
        Pos_2_region = c(2L, 4L),
        Pos_1_gene = c(10L, 30L),
        Pos_2_gene = c(20L, 40L),
        MI = c(0.9, 0.4)
    )

    for (column_name in setdiff(names(outliers_direct), "Pos_1")) {
        # Remove the data-frame class while shortening a column to avoid R recycling the value.
        unequal_outliers <- unclass(outliers_direct)
        unequal_outliers[[column_name]] <- unequal_outliers[[column_name]][1]
        class(unequal_outliers) <- "data.frame"

        expect_error(
            .cpp_create_bidirectional_position_links(
                unequal_outliers,
                data.frame(name = c(100L, 200L, 300L, 400L))
            ),
            "Circular plot outlier columns must have equal lengths.",
            fixed = TRUE,
            info = paste("short column", column_name)
        )
    }
})

test_that(".cpp_create_bidirectional_position_links rejects an empty outlier table", {
    outliers_direct <- data.frame(
        Pos_1 = integer(),
        Pos_2 = integer(),
        Pos_1_region = integer(),
        Pos_2_region = integer(),
        Pos_1_gene = integer(),
        Pos_2_gene = integer(),
        MI = numeric()
    )

    expect_error(
        .cpp_create_bidirectional_position_links(outliers_direct, data.frame(name = integer())),
        "Circular plot data must contain at least one direct outlier link.",
        fixed = TRUE
    )
})

test_that(".cpp_create_bidirectional_position_links rejects non-positive or missing position-data values", {
    outliers_direct <- data.frame(
        Pos_1 = 100L,
        Pos_2 = 200L,
        Pos_1_region = 1L,
        Pos_2_region = 2L,
        Pos_1_gene = 10L,
        Pos_2_gene = 20L,
        MI = 0.9
    )
    invalid_positions <- list(
        "zero" = 0L,
        "negative" = -1L,
        "missing" = NA_integer_
    )

    for (case_name in names(invalid_positions)) {
        position_data <- data.frame(name = c(invalid_positions[[case_name]], 200L))

        expect_error(
            .cpp_create_bidirectional_position_links(outliers_direct, position_data),
            "Position data values must be positive integers.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".cpp_create_bidirectional_position_links rejects duplicate position-data values", {
    outliers_direct <- data.frame(
        Pos_1 = 100L,
        Pos_2 = 200L,
        Pos_1_region = 1L,
        Pos_2_region = 2L,
        Pos_1_gene = 10L,
        Pos_2_gene = 20L,
        MI = 0.9
    )

    expect_error(
        .cpp_create_bidirectional_position_links(
            outliers_direct,
            data.frame(name = c(100L, 200L, 100L))
        ),
        "Circular plot position data must contain each position only once.",
        fixed = TRUE
    )
})

test_that(".cpp_create_bidirectional_position_links rejects non-positive or missing outlier positions", {
    valid_outliers <- data.frame(
        Pos_1 = 100L,
        Pos_2 = 200L,
        Pos_1_region = 1L,
        Pos_2_region = 2L,
        Pos_1_gene = 10L,
        Pos_2_gene = 20L,
        MI = 0.9
    )
    cases <- list(
        "first position is zero" = list(column = "Pos_1", value = 0L),
        "first position is negative" = list(column = "Pos_1", value = -1L),
        "first position is missing" = list(column = "Pos_1", value = NA_integer_),
        "second position is zero" = list(column = "Pos_2", value = 0L),
        "second position is negative" = list(column = "Pos_2", value = -1L),
        "second position is missing" = list(column = "Pos_2", value = NA_integer_)
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        outliers_direct <- valid_outliers
        outliers_direct[[case$column]] <- case$value

        expect_error(
            .cpp_create_bidirectional_position_links(
                outliers_direct,
                data.frame(name = c(100L, 200L))
            ),
            "Outlier positions must be positive integers.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".cpp_create_bidirectional_position_links rejects non-positive or missing region indices", {
    valid_outliers <- data.frame(
        Pos_1 = 100L,
        Pos_2 = 200L,
        Pos_1_region = 1L,
        Pos_2_region = 2L,
        Pos_1_gene = 10L,
        Pos_2_gene = 20L,
        MI = 0.9
    )
    cases <- list(
        "first region is zero" = list(column = "Pos_1_region", value = 0L),
        "first region is negative" = list(column = "Pos_1_region", value = -1L),
        "first region is missing" = list(column = "Pos_1_region", value = NA_integer_),
        "second region is zero" = list(column = "Pos_2_region", value = 0L),
        "second region is negative" = list(column = "Pos_2_region", value = -1L),
        "second region is missing" = list(column = "Pos_2_region", value = NA_integer_)
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        outliers_direct <- valid_outliers
        outliers_direct[[case$column]] <- case$value

        expect_error(
            .cpp_create_bidirectional_position_links(
                outliers_direct,
                data.frame(name = c(100L, 200L))
            ),
            "Region indices must be positive integers.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".cpp_create_bidirectional_position_links rejects non-positive or missing gene indices", {
    valid_outliers <- data.frame(
        Pos_1 = 100L,
        Pos_2 = 200L,
        Pos_1_region = 1L,
        Pos_2_region = 2L,
        Pos_1_gene = 10L,
        Pos_2_gene = 20L,
        MI = 0.9
    )
    cases <- list(
        "first gene is zero" = list(column = "Pos_1_gene", value = 0L),
        "first gene is negative" = list(column = "Pos_1_gene", value = -1L),
        "first gene is missing" = list(column = "Pos_1_gene", value = NA_integer_),
        "second gene is zero" = list(column = "Pos_2_gene", value = 0L),
        "second gene is negative" = list(column = "Pos_2_gene", value = -1L),
        "second gene is missing" = list(column = "Pos_2_gene", value = NA_integer_)
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        outliers_direct <- valid_outliers
        outliers_direct[[case$column]] <- case$value

        expect_error(
            .cpp_create_bidirectional_position_links(
                outliers_direct,
                data.frame(name = c(100L, 200L))
            ),
            "Gene indices must be positive integers.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".cpp_create_bidirectional_position_links rejects non-finite MI values", {
    valid_outliers <- data.frame(
        Pos_1 = 100L,
        Pos_2 = 200L,
        Pos_1_region = 1L,
        Pos_2_region = 2L,
        Pos_1_gene = 10L,
        Pos_2_gene = 20L,
        MI = 0.9
    )
    invalid_mi_values <- list(
        "missing" = NA_real_,
        "not a number" = NaN,
        "positive infinity" = Inf,
        "negative infinity" = -Inf
    )

    for (case_name in names(invalid_mi_values)) {
        outliers_direct <- valid_outliers
        outliers_direct$MI <- invalid_mi_values[[case_name]]

        expect_error(
            .cpp_create_bidirectional_position_links(
                outliers_direct,
                data.frame(name = c(100L, 200L))
            ),
            "MI values must be finite.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".cpp_create_bidirectional_position_links requires both endpoints in position data", {
    outliers_direct <- data.frame(
        Pos_1 = 100L,
        Pos_2 = 200L,
        Pos_1_region = 1L,
        Pos_2_region = 2L,
        Pos_1_gene = 10L,
        Pos_2_gene = 20L,
        MI = 0.9
    )
    cases <- list(
        "position table is empty" = integer(),
        "first endpoint is absent" = 200L,
        "second endpoint is absent" = 100L
    )

    for (case_name in names(cases)) {
        expect_error(
            .cpp_create_bidirectional_position_links(
                outliers_direct,
                data.frame(name = cases[[case_name]])
            ),
            "Every outlier position must be present in the circular plot position data.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".cpp_sort_gene_links_for_tooltips groups and sorts tooltip links", {
    position_links <- data.frame(
        gene_1 = c(2L, 1L, 1L, 2L, 1L, 1L, 1L, 3L, 2L),
        gene_2 = c(3L, 4L, 2L, 1L, 4L, 2L, 2L, 1L, 3L),
        MI = c(0.4, 0.5, 0.7, 0.9, 0.8, 0.8, 0.3, 0.2, 0.95)
    )

    result <- .cpp_sort_gene_links_for_tooltips(position_links)

    expect_named(result, c("gene_1", "gene_2", "MI"))

    # Source gene 1 comes first. Its target genes tie at a highest MI of 0.8,
    # so target gene 2 comes before target gene 4 and each target's links stay together.
    expect_identical(result$gene_1, c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L))
    expect_identical(result$gene_2, c(2L, 2L, 2L, 4L, 4L, 3L, 3L, 1L, 1L))

    # For source gene 2, target gene 3 stays grouped by its group-leading MI of 0.95;
    # its 0.4 link therefore remains before target gene 1's 0.9 link.
    expect_identical(result$MI, c(0.8, 0.7, 0.3, 0.8, 0.5, 0.95, 0.4, 0.9, 0.2))
})

test_that(".cpp_sort_gene_links_for_tooltips rejects unequal column lengths", {
    position_links <- data.frame(
        gene_1 = c(1L, 2L),
        gene_2 = c(2L, 1L),
        MI = c(0.9, 0.9)
    )

    for (column_name in c("gene_2", "MI")) {
        # Remove the data-frame class while shortening a column to avoid R recycling the value.
        unequal_position_links <- unclass(position_links)
        unequal_position_links[[column_name]] <- unequal_position_links[[column_name]][1]
        class(unequal_position_links) <- "data.frame"

        expect_error(
            .cpp_sort_gene_links_for_tooltips(unequal_position_links),
            "Circular plot gene-link columns must have equal lengths.",
            fixed = TRUE,
            info = paste("short column", column_name)
        )
    }
})

test_that(".cpp_sort_gene_links_for_tooltips rejects an empty link table", {
    expect_error(
        .cpp_sort_gene_links_for_tooltips(data.frame(
            gene_1 = integer(),
            gene_2 = integer(),
            MI = numeric()
        )),
        "Circular plot position links must contain at least one row.",
        fixed = TRUE
    )
})

test_that(".cpp_sort_gene_links_for_tooltips rejects non-positive or missing gene indices", {
    valid_position_links <- data.frame(gene_1 = 1L, gene_2 = 2L, MI = 0.9)
    cases <- list(
        "source gene is zero" = list(column = "gene_1", value = 0L),
        "source gene is negative" = list(column = "gene_1", value = -1L),
        "source gene is missing" = list(column = "gene_1", value = NA_integer_),
        "target gene is zero" = list(column = "gene_2", value = 0L),
        "target gene is negative" = list(column = "gene_2", value = -1L),
        "target gene is missing" = list(column = "gene_2", value = NA_integer_)
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        position_links <- valid_position_links
        position_links[[case$column]] <- case$value

        expect_error(
            .cpp_sort_gene_links_for_tooltips(position_links),
            "Gene indices must be positive integers.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".cpp_sort_gene_links_for_tooltips rejects non-finite MI values", {
    invalid_mi_values <- list(
        "missing" = NA_real_,
        "not a number" = NaN,
        "positive infinity" = Inf,
        "negative infinity" = -Inf
    )

    for (case_name in names(invalid_mi_values)) {
        position_links <- data.frame(
            gene_1 = 1L,
            gene_2 = 2L,
            MI = invalid_mi_values[[case_name]]
        )

        expect_error(
            .cpp_sort_gene_links_for_tooltips(position_links),
            "MI values must be finite.",
            fixed = TRUE,
            info = case_name
        )
    }
})

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
        Pos_1_gene = c(1L, 1L, 1L),
        Pos_2_gene = c(2L, 3L, 3L),
        Pos_1_region = c(1L, 1L, 1L),
        Pos_2_region = c(2L, 3L, 3L),
        MI = c(0.4, 0.9, 0.6)
    )

    result <- .create_position_data(data)

    # Position 11 occurs in rows 1 and 2. Row 2 is retained because its MI is higher.
    # The remaining positions are sorted by their circular-plot region.
    expect_identical(result$name, c(11L, 96L, 152L, 253L, 208L))
    expect_identical(result$parent, c(1L, 1L, 2L, 3L, 3L))
    expect_identical(result$idx, c(2L, 3L, 1L, 2L, 3L))
    expect_identical(result$region, c(1L, 1L, 2L, 3L, 3L))

    # Selected MI values range from 0.4 to 0.9 and are rescaled to the range 0.5 to 1.
    expect_equal(result$weight, c(1, 0.7, 0.5, 1, 0.7))

    # Map positions within each gene to [0, 1], clamping them to [0.1, 0.9] for display.
    expect_equal(result$pos_in_gene, c(0.1, 0.9, 0.5, 0.5, 0.1))
})

test_that(".add_link_info_to_gene_data builds tooltips for links between different genes", {
    data <- new.env(parent = emptyenv())
    data$gff <- data.frame(
        start = c(1, 101, 201, 301),
        end = c(100, 200, 300, 400),
        Name = c("alpha", "beta", "gamma", "delta")
    )
    gene_data <- data.frame(id = 1:4)
    position_links <- data.frame(
        gene_1 = c(1L, 2L, 1L, 3L, 1L, 2L),
        gene_2 = c(2L, 1L, 3L, 1L, 2L, 1L),
        MI = c(0.8, 0.8, 0.9, 0.9, 0.4, 0.4)
    )

    result <- .add_link_info_to_gene_data(data, gene_data, position_links)

    # Alpha's gamma links are listed before its beta links because gamma has the higher maximum MI.
    expect_identical(
        result$genes_linked_to[[1]],
        c("Linked to the following genes:", "gamma (201-300)", "0.9",
          "beta (101-200)", "0.8", "0.4")
    )
    expect_identical(
        result$genes_linked_to[[2]],
        c("Linked to the following genes:", "alpha (1-100)", "0.8", "0.4")
    )
    expect_identical(
        result$genes_linked_to[[3]],
        c("Linked to the following genes:", "alpha (1-100)", "0.9")
    )

    # Delta has no links, so it retains an empty tooltip and zero counts.
    expect_null(result$genes_linked_to[[4]])
    expect_identical(result$n_genes_linked_to, c(2L, 1L, 1L, 0L))
    expect_identical(result$n_outliers, c(3L, 2L, 1L, 0L))
    expect_identical(result$length, c(6L, 4L, 3L, 0L))
})

test_that(".precompute_circular_plot_data finds the GFF row containing each outlier position", {
    data <- .make_precomputed_circular_plot_test_data()

    expect_identical(data$gff$gene_regions, c(1L, 2L, 3L))
    expect_identical(data$outliers_direct$Pos_1_gene, 1L)
    expect_identical(data$outliers_direct$Pos_2_gene, 2L)
    expect_identical(data$outliers_direct$Pos_1_gene_name, "cds1")
    expect_identical(data$outliers_direct$Pos_2_gene_name, "IGR_0k")
    expect_identical(data$outliers_direct$Pos_1_region, 1L)
    expect_identical(data$outliers_direct$Pos_2_region, 2L)
})

test_that(".precompute_circular_plot_data creates feature and position data", {
    data <- .make_precomputed_circular_plot_test_data()
    gene_data <- .get_vega_dataset(data$edges, "gene_data")$values
    position_data <- .get_vega_dataset(data$edges, "pos_data")$values

    expect_named(gene_data,
                 c("id", "name", "region", "angle_step", "step_size", "start", "end",
                   "genes_linked_to", "n_genes_linked_to", "n_outliers", "length"))
    expect_identical(gene_data$id, c(1L, 2L, 3L))
    expect_identical(as.character(gene_data$name), c("cds1", "IGR_0k", "cds2"))
    expect_identical(gene_data$region, c(1L, 2L, 3L))
    expect_identical(gene_data$start, c(1, 101, 201))
    expect_identical(gene_data$end, c(100, 200, 300))

    expect_identical(gene_data$genes_linked_to[[1]],
                     c("Linked to the following genes:", "IGR_0k (101-200)", "0.8"))
    expect_identical(gene_data$genes_linked_to[[2]],
                     c("Linked to the following genes:", "cds1 (1-100)", "0.8"))
    expect_null(gene_data$genes_linked_to[[3]])
    expect_identical(gene_data$n_genes_linked_to, c(1L, 1L, 0L))
    expect_identical(gene_data$n_outliers, c(1L, 1L, 0L))
    expect_identical(gene_data$length, c(3L, 3L, 0L))

    expected_position_fields <- c("name", "parent", "region", "weight", "pos_in_gene")
    expect_named(position_data[expected_position_fields], expected_position_fields)
    expect_identical(position_data$name, c(50L, 150L))
    expect_identical(position_data$parent, c(1L, 2L))
    expect_identical(position_data$region, c(1L, 2L))
    expect_equal(position_data$weight, c(0.75, 0.75))
    expect_equal(position_data$pos_in_gene, c(49 / 99, 49 / 99))
})

test_that(".precompute_circular_plot_data creates links with 1-based GFF rows and 0-based Vega position-data indices", {
    data <- .make_precomputed_circular_plot_test_data()
    position_links <- .get_vega_dataset(data$edges, "pos_links")$values

    expect_named(position_links,
                 c("region_1", "region_2", "gene_1", "gene_2",
                   "pos_data_idx_1", "pos_data_idx_2", "MI", "weight"))
    expect_identical(position_links$region_1, c(1L, 2L))
    expect_identical(position_links$region_2, c(2L, 1L))
    # These values are 1-based rows in data$gff.
    expect_identical(position_links$gene_1, c(1L, 2L))
    expect_identical(position_links$gene_2, c(2L, 1L))
    # These values are 0-based indices into Vega's position data.
    expect_identical(position_links$pos_data_idx_1, c(0L, 1L))
    expect_identical(position_links$pos_data_idx_2, c(1L, 0L))
    expect_identical(position_links$MI, c(0.8, 0.8))
    expect_equal(position_links$weight, c(0.75, 0.75))
})

test_that(".precompute_circular_plot_data creates Vega lookups for 1-based GFF rows and 0-based position-data indices", {
    data <- .make_precomputed_circular_plot_test_data()
    position_data <- .get_vega_dataset(data$edges, "pos_data")
    position_links <- .get_vega_dataset(data$edges, "pos_links")

    expect_identical(
        .get_vega_formula_expression(position_data, "parent_gene"),
        "data('gene_data')[datum.parent - 1].name"
    )
    expect_identical(
        .get_vega_formula_expression(position_links, "x"),
        "data('pos_data')[datum.pos_data_idx_1].x_1"
    )
    expect_identical(
        .get_vega_formula_expression(position_links, "x2"),
        "data('pos_data')[datum.pos_data_idx_2].x_2"
    )
})
