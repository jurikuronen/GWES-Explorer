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
    previous_edges <- list(name = "previous")
    data$outliers <- previous_outliers
    data$edges <- previous_edges

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
    expect_identical(data$edges, previous_edges)
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
