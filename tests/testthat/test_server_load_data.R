.expect_outliers_failure <- function(lines, message = "Failed to read outliers file.") {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    writeLines(lines, outliers_path)

    data <- new.env(parent = emptyenv())
    result <- suppressWarnings(.read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "test.outliers")
    ))

    expect_identical(result$success, .STATUS_FAILURE, info = paste(lines, collapse = "\n"))
    expect_match(as.character(result$status), message, fixed = TRUE)
    expect_null(data$outliers)
}

.minimum_circular_plot_range_length <- function() {
    .settings$circular_plot_region_count * 1000L
}

.minimum_circular_plot_range_error <- function() {
    paste0("GFF3 region must span at least ",
           format(.minimum_circular_plot_range_length(), big.mark = ",", scientific = FALSE, trim = TRUE),
           " bases.")
}

test_that(".clear_data reports when there is no data to clear", {
    data <- new.env(parent = emptyenv())

    result <- .clear_data(data)

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_identical(as.character(result$status), "There was no data to clear.")
})

test_that(".read_data rejects incomplete tree data", {
    tree_file <- data.frame(datapath = "unused", name = "tree")
    fasta_file <- data.frame(datapath = "unused", name = "fasta")
    loci_file <- data.frame(datapath = "unused", name = "loci")

    cases <- list(
        "tree only" = list(tree_file = tree_file, fasta_file = NULL, loci_file = NULL),
        "fasta only" = list(tree_file = NULL, fasta_file = fasta_file, loci_file = NULL),
        "loci only" = list(tree_file = NULL, fasta_file = NULL, loci_file = loci_file),
        "tree and fasta" = list(tree_file = tree_file, fasta_file = fasta_file, loci_file = NULL),
        "tree and loci" = list(tree_file = tree_file, fasta_file = NULL, loci_file = loci_file),
        "fasta and loci" = list(tree_file = NULL, fasta_file = fasta_file, loci_file = loci_file)
    )

    for (case_name in names(cases)) {
        files <- cases[[case_name]]
        data <- new.env(parent = emptyenv())

        result <- .read_data(
            data = data,
            outliers_file = data.frame(datapath = "unused", name = "outliers"),
            tree_file = files$tree_file,
            fasta_file = files$fasta_file,
            loci_file = files$loci_file,
            phenotype_file = NULL,
            gff_file = NULL
        )

        expect_identical(result$success, .STATUS_FAILURE, info = case_name)
        expect_match(as.character(result$status),
                     "requires all three files",
                     fixed = TRUE,
                     info = case_name)
    }
})

test_that(".read_data leaves existing session data unchanged when loading fails", {
    data <- new.env(parent = emptyenv())

    # Set any existing data.
    previous_outliers <- data.frame(Pos_1 = 1L)
    previous_circular_plot_spec <- list(name = "previous")
    data$outliers <- previous_outliers
    data$circular_plot_spec <- previous_circular_plot_spec

    result <- .read_data(
        data = data,
        outliers_file = data.frame(datapath = tempfile("missing-outliers-"), name = "missing.outliers"),
        tree_file = NULL,
        fasta_file = NULL,
        loci_file = NULL,
        phenotype_file = NULL,
        gff_file = NULL
    )

    expect_identical(result$success, .STATUS_FAILURE)
    expect_identical(data$outliers, previous_outliers)
    expect_identical(data$circular_plot_spec, previous_circular_plot_spec)
})

test_that(".has_outliers_header detects headers", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    cases <- c(
        "ab cde fghi jklmn opqrst" = TRUE,
        "1 Pos_2 Distance Direct MI" = TRUE,
        "10 20 10 1 0.5" = FALSE,
        "TRUE 10 20 0.5 1e-5" = FALSE,
        "1e-5 TRUE 10 20 0.5" = FALSE,
        "0.5 1e-5 TRUE 10 20" = FALSE,
        "20 0.5 1e-5 TRUE 10" = FALSE,
        "10 20 0.5 1e-5 TRUE" = FALSE,
        "10 20 10" = FALSE
    )

    for (row in names(cases)) {
        writeLines(row, outliers_path)
        expect_identical(.has_outliers_header(outliers_path), cases[[row]], info = row)
    }
})

test_that(".read_outliers reads required columns", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    writeLines("10 20 10 1 0.5", outliers_path)

    data <- new.env(parent = emptyenv())
    result <- .read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "required-columns.outliers")
    )

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_identical(
        names(data$outliers),
        c("Pos_1", "Pos_2", "Distance", "Direct", "MI")
    )
})

test_that(".read_outliers ignores a header", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    writeLines(c("ab cde fghi jklmn opqrst",
                 "10 20 10 1 0.5",
                 "30 40 10 1 0.8"), outliers_path)

    data <- new.env(parent = emptyenv())
    result <- .read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "header.outliers")
    )

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_named(data$outliers, c("Pos_1", "Pos_2", "Distance", "Direct", "MI"))
    expect_identical(data$outliers$Pos_1, c(30L, 10L))
    expect_identical(data$outliers$Pos_2, c(40L, 20L))
    expect_equal(data$outliers$MI, c(0.8, 0.5))
})

test_that(".read_outliers accepts logical notation data", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    writeLines(c("10 20 10 TRUE 0.8",
                 "30 40 10 FALSE 0.7",
                 "50 60 10 T 0.6",
                 "70 80 10 F 0.5"), outliers_path)

    data <- new.env(parent = emptyenv())
    result <- .read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "logical.outliers")
    )

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_identical(data$outliers$Pos_1, c(10L, 30L, 50L, 70L))
    expect_identical(data$outliers$Direct, c(TRUE, FALSE, TRUE, FALSE))
    expect_identical(data$outliers_direct$Pos_1, c(10L, 50L))
})

test_that(".read_outliers accepts scientific notation data", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    writeLines(c("10 20 10 1 1e-5",
                 "30 40 10 1 5E-6"), outliers_path)

    data <- new.env(parent = emptyenv())
    result <- .read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "scientific.outliers")
    )

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_identical(data$outliers$Pos_1, c(10L, 30L))
    expect_equal(data$outliers$MI, c(1e-5, 5e-6))
})

test_that(".read_outliers fails on a file containing only a header", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    writeLines("Pos_1 Pos_2 Distance Direct MI", outliers_path)

    data <- new.env(parent = emptyenv())
    result <- .read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "only-header.outliers")
    )

    expect_identical(result$success, .STATUS_FAILURE)
})

test_that(".read_outliers sorts outliers by MI descending", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    writeLines(c("10 20 10 0 0.6",
                 "30 40 10 1 0.3",
                 "50 60 10 0 0.9",
                 "70 80 10 1 0.8",
                 "90 100 10 1 0.5"), outliers_path)

    data <- new.env(parent = emptyenv())
    result <- .read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "unsorted.outliers")
    )

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_identical(data$outliers$Pos_1, c(50L, 70L, 10L, 90L, 30L))
    expect_equal(data$outliers$MI, c(0.9, 0.8, 0.6, 0.5, 0.3))
    expect_identical(data$outliers$Direct, c(FALSE, TRUE, FALSE, TRUE, TRUE))
    expect_identical(data$outliers_direct$Pos_1, c(70L, 90L, 30L))
    expect_equal(data$outliers_direct$MI, c(0.8, 0.5, 0.3))
})

test_that(".read_outliers reads the optional MI_wogaps column", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    writeLines("10 20 10 1 0.5 0.4", outliers_path)

    data <- new.env(parent = emptyenv())
    result <- .read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "mi-without-gaps.outliers")
    )

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_identical(
        names(data$outliers),
        c("Pos_1", "Pos_2", "Distance", "Direct", "MI", "MI_wogaps")
    )
})

test_that(".read_outliers accepts additional columns", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    additional_values <- seq_len(20)
    writeLines(
        paste(c(10, 20, 10, 1, 0.5, 0.4, additional_values), collapse = " "),
        outliers_path
    )

    data <- new.env(parent = emptyenv())
    result <- .read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "additional-columns.outliers")
    )

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_identical(
        names(data$outliers),
        c("Pos_1", "Pos_2", "Distance", "Direct", "MI", "MI_wogaps")
    )
})

test_that(".read_outliers rejects invalid data", {
    valid_row <- "10 20 10 1 0.5 0.4"
    invalid_rows <- c(
        "30.5 40 10 1 0.8 0.7",
        "30 TRUE 10 1 0.8 0.7",
        "30 40 10.5 1 0.8 0.7",
        "30 40 10 2 0.8 0.7",
        "30 40 10 1 invalid 0.7",
        "30 40 10 1 0.8 invalid",
        "30 40 10"
    )

    for (row in invalid_rows) {
        .expect_outliers_failure(c(valid_row, row))
    }

    .expect_outliers_failure("10 20 10")
})

test_that(".read_outliers rejects missing required values", {
    for (input in c("NA", "")) {
        for (column in seq_len(5L)) {
            fields <- c("10", "20", "10", "1", "0.5", "0.4")
            fields[column] <- input

            .expect_outliers_failure(
                c("30 40 10 1 0.8 0.7", paste(fields, collapse = " ")),
                "Required outlier columns must not contain missing values (NA)."
            )
        }
    }
})

test_that(".read_outliers rejects files without direct outlier links", {
    outliers_path <- tempfile(fileext = ".outliers")
    on.exit(unlink(outliers_path))
    writeLines("10 20 10 0 0.5 0.4 0.1 0", outliers_path)

    data <- new.env(parent = emptyenv())
    result <- .read_outliers(
        data,
        data.frame(datapath = outliers_path, name = "indirect-only.outliers")
    )

    expect_identical(result$success, .STATUS_FAILURE)
    expect_identical(
        as.character(result$status),
        "Outliers file must contain at least one direct outlier link."
    )
    expect_equal(nrow(data$outliers_direct), 0)
})

test_that(".read_gff appends and sorts calculated IGRs", {
    gff_path <- tempfile(fileext = ".gff3")
    on.exit(unlink(gff_path))

    # Write the genes in reverse order to check that .read_gff sorts them by position.
    writeLines(
        c(
            "##gff-version 3",
            paste("##sequence-region chromosome 1", .minimum_circular_plot_range_length()),
            "chromosome\t.\tgene\t4998\t6000\t.\t+\t.\tID=gene2;Name=test2",
            "chromosome\t.\tgene\t1\t1000\t.\t+\t.\tID=gene1;Name=test1"
        ),
        gff_path
    )

    data <- new.env(parent = emptyenv())

    # Position 4500 is in the gap between the genes, while position 5500 is inside the second gene.
    data$outliers_direct <- data.frame(Pos_1 = 4500, Pos_2 = 5500)

    result <- .read_gff(data, data.frame(datapath = gff_path, name = "test.gff3"))

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_named(data$gff, c("start", "end", "Name"))

    # The sorted result should contain the first gene, the calculated IGR and then the second gene.
    expect_equal(data$gff$start, c(1, 1001, 4998))
    expect_equal(data$gff$end, c(1000, 4997, 6000))

    # The IGR is 1001-4997, so its midpoint 2999 gives it the name IGR_2k.
    expect_identical(as.character(data$gff$Name), c("test1", "IGR_2k", "test2"))
})

test_that(".read_gff uses CDS features when gene features are absent", {
    gff_path <- tempfile(fileext = ".gff3")
    on.exit(unlink(gff_path))

    writeLines(
        c(
            "##gff-version 3",
            paste("##sequence-region chromosome 1", .minimum_circular_plot_range_length()),
            "chromosome\t.\tCDS\t201\t300\t.\t+\t.\tID=cds2;Name=cds2",
            "chromosome\t.\tCDS\t1\t100\t.\t+\t.\tID=cds1;Name=cds1"
        ),
        gff_path
    )

    data <- new.env(parent = emptyenv())
    data$outliers_direct <- data.frame(Pos_1 = 50L, Pos_2 = 150L)

    result <- .read_gff(data, data.frame(datapath = gff_path, name = "features.gff3"))

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_equal(data$gff$start, c(1, 101, 201))
    expect_equal(data$gff$end, c(100, 200, 300))
    expect_identical(as.character(data$gff$Name), c("cds1", "IGR_0k", "cds2"))
})

test_that(".determine_ranges returns the same chromosome range from each supported source", {
    minimum_range_length <- .minimum_circular_plot_range_length()
    cases <- list(
        "GFF region row" = list(
            gff = data.frame(type = "region", start = 1, end = minimum_range_length),
            file_lines = "##gff-version 3"
        ),
        "sequence-region pragma" = list(
            gff = data.frame(type = "gene", start = 10, end = 20),
            file_lines = c("##gff-version 3", paste("##sequence-region chromosome 1", minimum_range_length))
        ),
        "matching region row and pragma" = list(
            gff = data.frame(type = "region", start = 1, end = minimum_range_length),
            file_lines = c("##gff-version 3", paste("##sequence-region chromosome 1", minimum_range_length))
        ),
        "feature-coordinate fallback" = list(
            gff = data.frame(type = "gene", start = c(10, 200), end = c(100, minimum_range_length)),
            file_lines = "##gff-version 3"
        )
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        gff_path <- tempfile(fileext = ".gff3")
        on.exit(unlink(gff_path), add = TRUE)
        writeLines(case$file_lines, gff_path)

        data <- new.env(parent = emptyenv())
        data$gff <- case$gff

        expect_equal(.determine_ranges(data, gff_path), c(1, minimum_range_length), info = case_name)
    }
})

test_that(".determine_ranges requires the region row and sequence-region pragma to match", {
    minimum_range_length <- .minimum_circular_plot_range_length()
    gff_path <- tempfile(fileext = ".gff3")
    on.exit(unlink(gff_path))
    writeLines(c("##gff-version 3",
                 paste("##sequence-region chromosome 1", minimum_range_length + 10000L)),
               gff_path)

    data <- new.env(parent = emptyenv())
    data$gff <- data.frame(type = "region", start = 1, end = minimum_range_length)

    expect_error(
        .determine_ranges(data, gff_path),
        "GFF3 region row and ##sequence-region pragma must have the same range.",
        fixed = TRUE
    )
})

test_that(".determine_ranges requires chromosome ranges to start at position 1", {
    minimum_range_length <- .minimum_circular_plot_range_length()
    range_start <- 2L
    range_end <- range_start + minimum_range_length - 1L
    cases <- list(
        "GFF region row" = list(
            gff = data.frame(type = "region", start = range_start, end = range_end),
            file_lines = "##gff-version 3"
        ),
        "sequence-region pragma" = list(
            gff = data.frame(type = "gene", start = 10L, end = 20L),
            file_lines = c("##gff-version 3",
                           paste("##sequence-region chromosome", range_start, range_end))
        )
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        gff_path <- tempfile(fileext = ".gff3")
        on.exit(unlink(gff_path), add = TRUE)
        writeLines(case$file_lines, gff_path)

        data <- new.env(parent = emptyenv())
        data$gff <- case$gff

        expect_error(
            .determine_ranges(data, gff_path),
            "GFF3 region must start at position 1.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".determine_ranges rejects chromosome ranges shorter than the circular plot minimum", {
    minimum_range_length <- .minimum_circular_plot_range_length()
    too_short_range_end <- minimum_range_length - 1L
    cases <- list(
        "GFF region row" = list(
            gff = data.frame(type = "region", start = 1L, end = too_short_range_end),
            file_lines = "##gff-version 3"
        ),
        "sequence-region pragma" = list(
            gff = data.frame(type = "gene", start = 10L, end = 20L),
            file_lines = c("##gff-version 3",
                           paste("##sequence-region chromosome 1", too_short_range_end))
        ),
        "feature-coordinate fallback" = list(
            gff = data.frame(type = "gene", start = 10L, end = too_short_range_end),
            file_lines = "##gff-version 3"
        )
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        gff_path <- tempfile(fileext = ".gff3")
        on.exit(unlink(gff_path), add = TRUE)
        writeLines(case$file_lines, gff_path)

        data <- new.env(parent = emptyenv())
        data$gff <- case$gff

        expect_error(
            .determine_ranges(data, gff_path),
            .minimum_circular_plot_range_error(),
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".determine_ranges uses the circular plot settings for the minimum chromosome range", {
    previous_region_group_count <- .settings$circular_plot_region_group_count
    previous_regions_per_group_count <- .settings$circular_plot_regions_per_group_count
    previous_region_count <- .settings$circular_plot_region_count
    on.exit({
        .settings$circular_plot_region_group_count <- previous_region_group_count
        .settings$circular_plot_regions_per_group_count <- previous_regions_per_group_count
        .settings$circular_plot_region_count <- previous_region_count
    })

    .settings$circular_plot_region_group_count <- 2L
    .settings$circular_plot_regions_per_group_count <- 3L
    .settings$circular_plot_region_count <- 6L
    minimum_range_length <- .minimum_circular_plot_range_length()

    gff_path <- tempfile(fileext = ".gff3")
    on.exit(unlink(gff_path), add = TRUE)
    writeLines(c("##gff-version 3", paste("##sequence-region chromosome 1", minimum_range_length)), gff_path)

    data <- new.env(parent = emptyenv())
    data$gff <- data.frame(type = "gene", start = 1, end = minimum_range_length)

    expect_equal(.determine_ranges(data, gff_path), c(1, minimum_range_length))

    writeLines(c("##gff-version 3", paste("##sequence-region chromosome 1", minimum_range_length - 1L)),
               gff_path)
    expect_error(
        .determine_ranges(data, gff_path),
        .minimum_circular_plot_range_error(),
        fixed = TRUE
    )
})

test_that(".determine_ranges rejects more than one chromosome region", {
    minimum_range_length <- .minimum_circular_plot_range_length()
    cases <- list(
        "multiple GFF region rows" = list(
            gff = data.frame(type = c("region", "region"),
                             start = c(1, 1),
                             end = c(minimum_range_length, minimum_range_length + 10000L)),
            file_lines = "##gff-version 3"
        ),
        "multiple sequence-region pragmas" = list(
            gff = data.frame(type = "gene", start = 10, end = 20),
            file_lines = c("##gff-version 3",
                           paste("##sequence-region chromosome 1", minimum_range_length),
                           paste("##sequence-region chromosome 1", minimum_range_length + 10000L))
        )
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        gff_path <- tempfile(fileext = ".gff3")
        on.exit(unlink(gff_path), add = TRUE)
        writeLines(case$file_lines, gff_path)

        data <- new.env(parent = emptyenv())
        data$gff <- case$gff

        expect_error(
            .determine_ranges(data, gff_path),
            "Only one GFF3 region is supported.",
            fixed = TRUE,
            info = case_name
        )
    }
})

test_that(".read_gff reports chromosome range errors", {
    minimum_range_length <- .minimum_circular_plot_range_length()
    gff_path <- tempfile(fileext = ".gff3")
    on.exit(unlink(gff_path))

    writeLines(
        c(
            "##gff-version 3",
            paste("##sequence-region chromosome 1", minimum_range_length),
            paste("##sequence-region chromosome 1", minimum_range_length + 10000L),
            paste0("chromosome\t.\tgene\t1\t",
                   minimum_range_length,
                   "\t.\t+\t.\tID=gene1;Name=gene1")
        ),
        gff_path
    )

    data <- new.env(parent = emptyenv())
    data$outliers_direct <- data.frame(Pos_1 = 50L, Pos_2 = 150L)

    result <- .read_gff(data, data.frame(datapath = gff_path, name = "ambiguous-range.gff3"))

    expect_identical(result$success, .STATUS_FAILURE)
    expect_null(data$gff)
    expect_match(as.character(result$status), "Failed to determine GFF3 region.", fixed = TRUE)
})

test_that(".read_tree detects the format from file names", {
    tree_paths <- c(newick = tempfile(), nexus = tempfile())
    on.exit(unlink(tree_paths))
    writeLines("(A:1,B:1);", tree_paths[["newick"]])
    writeLines(c("#NEXUS", "Begin trees;", "Tree tree_1 = (A:1,B:1);", "End;"), tree_paths[["nexus"]])

    cases <- list(
        "Newick" = data.frame(datapath = tree_paths[["newick"]], name = "tree.nwk"),
        "Nexus" = data.frame(datapath = tree_paths[["nexus"]], name = "tree.nex")
    )

    for (case_name in names(cases)) {
        tree_file <- cases[[case_name]]
        data <- new.env(parent = emptyenv())

        result <- .read_tree(data, tree_file)

        expect_identical(result$success, .STATUS_SUCCESS, info = case_name)
        expect_false(is.null(data$tree), info = case_name)
    }
})

test_that(".read_tree removes enclosing quotes", {
    tree_path <- tempfile()
    on.exit(unlink(tree_path))
    writeLines("(A:1,'B':1,\"C\":1);", tree_path)
    data <- new.env(parent = emptyenv())

    result <- .read_tree(data, data.frame(datapath = tree_path, name = "tree.nwk"))

    expect_identical(result$success, .STATUS_SUCCESS)
    expect_identical(data$tree$tip.label, c("A", "B", "C"))
})
