.minimum_circular_plot_range_length <- function() {
    .circular_plot_regions() * 1000L
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
    previous_n_groups <- .settings$circular_plot_n_groups
    previous_n_regions_per_group <- .settings$circular_plot_n_regions_per_group
    on.exit({
        .settings$circular_plot_n_groups <- previous_n_groups
        .settings$circular_plot_n_regions_per_group <- previous_n_regions_per_group
    })

    .settings$circular_plot_n_groups <- 2L
    .settings$circular_plot_n_regions_per_group <- 3L
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
