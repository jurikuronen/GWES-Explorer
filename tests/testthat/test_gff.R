test_that(".cpp_find_igrs_with_outliers does not create IGRs inside overlapping genes", {
    result <- .cpp_find_igrs_with_outliers(
        gene_start_positions = c(1, 10, 40),
        gene_end_positions = c(30, 20, 100),
        outliers_direct = data.frame(Pos_1 = 25, Pos_2 = 50),
        region_ranges = c(1, 100)
    )

    # An incorrect implementation considering only the previous gene's end would add an IGR at 21-39.
    expect_equal(nrow(result), 0)
})

test_that(".cpp_find_igrs_with_outliers treats the origin IGR as circular", {
    cases <- list(
        "region start" = data.frame(Pos_1 = 1, Pos_2 = 15),
        "position before the first gene" = data.frame(Pos_1 = 9, Pos_2 = 15),
        "position after the last gene" = data.frame(Pos_1 = 15, Pos_2 = 41),
        "region end" = data.frame(Pos_1 = 15, Pos_2 = 100)
    )

    for (case_name in names(cases)) {
        result <- .cpp_find_igrs_with_outliers(
            gene_start_positions = c(10, 30),
            gene_end_positions = c(20, 40),
            outliers_direct = cases[[case_name]],
            region_ranges = c(1, 100)
        )
        result <- result[order(result$start), ]

        # Verify that [1, 9] and [41, 100] became IGRs named "IGR_origin".
        expect_equal(result$start, c(1, 41), info = case_name)
        expect_equal(result$end, c(9, 100), info = case_name)
        expect_identical(as.character(result$Name), rep("IGR_origin", 2), info = case_name)
    }
})

test_that(".cpp_find_igrs_with_outliers excludes gene boundaries from the origin IGR", {
    for (gene_boundary_position in c(10, 40)) {
        result <- .cpp_find_igrs_with_outliers(
            gene_start_positions = c(10, 30),
            gene_end_positions = c(20, 40),
            outliers_direct = data.frame(
                Pos_1 = min(15, gene_boundary_position),
                Pos_2 = max(15, gene_boundary_position)
            ),
            region_ranges = c(1, 100)
        )

        expect_equal(nrow(result), 0, info = paste("gene boundary position", gene_boundary_position))
    }
})

test_that(".cpp_find_igrs_with_outliers returns no IGR when the gap contains no outlier", {
    result <- .cpp_find_igrs_with_outliers(
        gene_start_positions = c(1, 20),
        gene_end_positions = c(10, 100),
        outliers_direct = data.frame(Pos_1 = 50, Pos_2 = 60),
        region_ranges = c(1, 100)
    )

    # The only gap is 11-19, but both outliers are inside the second gene, so no IGR should be returned.
    expect_equal(nrow(result), 0)
})

test_that(".cpp_find_igrs_with_outliers uses IGR midpoints in their names", {
    cases <- data.frame(
        midpoint = c(999, 1000, 1001, 1999, 2000, 2001),
        igr_start = c(998, 999, 1000, 1998, 1999, 2000),
        igr_end = c(1000, 1001, 1002, 2000, 2001, 2002),
        outlier_position = c(1000, 999, 1002, 2000, 1999, 2002),
        expected_name = c("IGR_0k", "IGR_1k", "IGR_1k", "IGR_1k", "IGR_2k", "IGR_2k")
    )

    for (i in seq_len(nrow(cases))) {
        case <- cases[i, ]
        result <- .cpp_find_igrs_with_outliers(
            gene_start_positions = c(1, case$igr_end + 1),
            gene_end_positions = c(case$igr_start - 1, case$igr_end + 100),
            outliers_direct = data.frame(Pos_1 = case$outlier_position, Pos_2 = case$igr_end + 50),
            region_ranges = c(1, case$igr_end + 100)
        )

        expect_equal(result$start, case$igr_start, info = paste("midpoint", case$midpoint))
        expect_equal(result$end, case$igr_end, info = paste("midpoint", case$midpoint))
        expect_identical(as.character(result$Name), case$expected_name, info = paste("midpoint", case$midpoint))
    }
})

test_that(".cpp_find_igrs_with_outliers returns no IGRs when there are no outliers", {
    result <- .cpp_find_igrs_with_outliers(
        gene_start_positions = c(1, 20),
        gene_end_positions = c(10, 100),
        outliers_direct = data.frame(Pos_1 = numeric(), Pos_2 = numeric()),
        region_ranges = c(1, 100)
    )

    expect_named(result, c("start", "end", "Name"))
    expect_equal(nrow(result), 0)
})

test_that(".cpp_find_igrs_with_outliers omits empty sides of the origin IGR", {
    cases <- list(
        "first gene starts at the region start" = list(
            gene_start_positions = c(1, 30),
            gene_end_positions = c(20, 40),
            outliers_direct = data.frame(Pos_1 = 15, Pos_2 = 50),
            expected_start = 41,
            expected_end = 100
        ),
        "last gene ends at the region end" = list(
            gene_start_positions = c(10, 30),
            gene_end_positions = c(20, 100),
            outliers_direct = data.frame(Pos_1 = 5, Pos_2 = 15),
            expected_start = 1,
            expected_end = 9
        )
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        result <- .cpp_find_igrs_with_outliers(
            case$gene_start_positions,
            case$gene_end_positions,
            case$outliers_direct,
            c(1, 100)
        )

        expect_equal(nrow(result), 1, info = case_name)
        expect_equal(result$start, case$expected_start, info = case_name)
        expect_equal(result$end, case$expected_end, info = case_name)
        expect_identical(as.character(result$Name), "IGR_origin", info = case_name)
    }
})

test_that(".cpp_find_igrs_with_outliers rejects invalid GFF3 data", {
    cases <- list(
        "no genes" = list(
            starts = numeric(),
            ends = numeric(),
            region = c(1, 100),
            message = "GFF3 data must contain at least one gene."
        ),
        "unequal start and end lengths" = list(
            starts = c(1, 20),
            ends = 10,
            region = c(1, 100),
            message = "GFF3 start and end columns must have equal lengths."
        ),
        "region with the wrong length" = list(
            starts = 1,
            ends = 10,
            region = 1,
            message = "The GFF3 region must contain a valid start and end."
        ),
        "region with too many positions" = list(
            starts = 1,
            ends = 10,
            region = c(1, 100, 200),
            message = "The GFF3 region must contain a valid start and end."
        ),
        "region starting before position one" = list(
            starts = 1,
            ends = 10,
            region = c(0, 100),
            message = "The GFF3 region must contain a valid start and end."
        ),
        "region starting after its end" = list(
            starts = 1,
            ends = 10,
            region = c(100, 1),
            message = "The GFF3 region must contain a valid start and end."
        ),
        "gene starting after its end" = list(
            starts = 20,
            ends = 10,
            region = c(1, 100),
            message = "A GFF3 gene start must not be after its end."
        ),
        "gene starting before the region" = list(
            starts = 0,
            ends = 10,
            region = c(1, 100),
            message = "GFF3 genes must be within the GFF3 region."
        ),
        "gene ending after the region" = list(
            starts = 90,
            ends = 101,
            region = c(1, 100),
            message = "GFF3 genes must be within the GFF3 region."
        ),
        "unsorted genes" = list(
            starts = c(20, 10),
            ends = c(30, 15),
            region = c(1, 100),
            message = "GFF3 genes must be sorted by start position."
        )
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]

        expect_error(
            .cpp_find_igrs_with_outliers(
                case$starts,
                case$ends,
                data.frame(Pos_1 = 40, Pos_2 = 50),
                case$region
            ),
            case$message,
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".cpp_find_igrs_with_outliers retains a one-position IGR", {
    result <- .cpp_find_igrs_with_outliers(
        gene_start_positions = c(1, 12),
        gene_end_positions = c(10, 100),
        outliers_direct = data.frame(Pos_1 = 11, Pos_2 = 50),
        region_ranges = c(1, 100)
    )

    expect_equal(result$start, 11)
    expect_equal(result$end, 11)
    expect_identical(as.character(result$Name), "IGR_0k")
})

test_that(".cpp_find_outlier_gene_or_igr_indices selects the latest-starting gene containing each outlier", {
    result <- .cpp_find_outlier_gene_or_igr_indices(
        gene_or_igr_start_positions = c(1, 100, 200, 300),
        gene_or_igr_end_positions = c(1000, 500, 250, 350),
        outlier_positions_1 = c(225, 275, 600),
        outlier_positions_2 = c(325, 400, 700)
    )

    expect_equal(result$pos1_gene_or_igr_index, c(3, 2, 1))
    expect_equal(result$pos2_gene_or_igr_index, c(4, 2, 1))
})

test_that(".cpp_find_outlier_gene_or_igr_indices maps positions at interval boundaries", {
    result <- .cpp_find_outlier_gene_or_igr_indices(
        gene_or_igr_start_positions = c(10, 21),
        gene_or_igr_end_positions = c(20, 30),
        outlier_positions_1 = c(10, 20),
        outlier_positions_2 = c(21, 30)
    )

    expect_equal(result$pos1_gene_or_igr_index, c(1, 1))
    expect_equal(result$pos2_gene_or_igr_index, c(2, 2))
})

test_that(".cpp_find_outlier_gene_or_igr_indices returns no indices when there are no outliers", {
    result <- .cpp_find_outlier_gene_or_igr_indices(
        gene_or_igr_start_positions = 1,
        gene_or_igr_end_positions = 100,
        outlier_positions_1 = numeric(),
        outlier_positions_2 = numeric()
    )

    expect_named(result, c("pos1_gene_or_igr_index", "pos2_gene_or_igr_index"))
    expect_equal(nrow(result), 0)
})

test_that(".cpp_find_outlier_gene_or_igr_indices preserves input order", {
    result <- .cpp_find_outlier_gene_or_igr_indices(
        gene_or_igr_start_positions = c(1, 101, 201),
        gene_or_igr_end_positions = c(100, 200, 300),
        outlier_positions_1 = c(115, 225, 5),
        outlier_positions_2 = c(150, 250, 50)
    )

    # Verify that the indices remain aligned with their original outlier links.
    expect_equal(result$pos1_gene_or_igr_index, c(2, 3, 1))
    expect_equal(result$pos2_gene_or_igr_index, c(2, 3, 1))
})

test_that(".cpp_find_outlier_gene_or_igr_indices rejects invalid gene and IGR data", {
    expect_error(
        .cpp_find_outlier_gene_or_igr_indices(numeric(), numeric(), 10, 20),
        "Gene and IGR data must contain at least one row.",
        fixed = TRUE
    )

    expect_error(
        .cpp_find_outlier_gene_or_igr_indices(c(1, 20), 10, 5, 25),
        "Gene and IGR start and end columns must have equal lengths.",
        fixed = TRUE
    )
})

test_that(".cpp_find_outlier_gene_or_igr_indices rejects outliers not contained in a gene or IGR", {
    cases <- list(
        "first position before the first row" = list(
            starts = 10,
            ends = 20,
            outlier_positions_1 = 5,
            outlier_positions_2 = 15
        ),
        "first position between rows" = list(
            starts = c(10, 30),
            ends = c(20, 40),
            outlier_positions_1 = 25,
            outlier_positions_2 = 35
        ),
        "second position after the last row" = list(
            starts = 10,
            ends = 20,
            outlier_positions_1 = 15,
            outlier_positions_2 = 25
        ),
        "second position between rows" = list(
            starts = c(10, 30),
            ends = c(20, 40),
            outlier_positions_1 = 15,
            outlier_positions_2 = 25
        )
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]

        expect_error(
            .cpp_find_outlier_gene_or_igr_indices(
                case$starts,
                case$ends,
                case$outlier_positions_1,
                case$outlier_positions_2
            ),
            "Outlier position is not within a GFF3 gene or intergenic region.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".cpp_get_gff_name_from_attributes extracts exact Name attributes", {
    attributes <- c(
        "Name=test1;ID=gene1",
        "ID=gene2;Name=test2;Note=x",
        "ID=gene3;Note=x;Name=test3",
        "ID=gene4;OtherName=testx;Name=test4",
        "ID=gene5;OtherName=testx;AnotherName=testy;Name=test5",
        "ID=gene6;OtherName=testx"
    )

    result <- .cpp_get_gff_name_from_attributes(attributes)

    expect_identical(as.character(result), c("test1", "test2", "test3", "test4", "test5", ""))
})

test_that(".cpp_get_gff_name_from_attributes returns no names for empty input", {
    result <- .cpp_get_gff_name_from_attributes(character())

    expect_identical(as.character(result), character())
})

test_that(".cpp_get_gff_name_from_attributes rejects missing attributes", {
    expect_error(
        .cpp_get_gff_name_from_attributes(c("Name=test1", NA_character_)),
        "GFF3 attributes must not contain missing values.",
        fixed = TRUE
    )
})
