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
            "##sequence-region chromosome 1 6000",
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
