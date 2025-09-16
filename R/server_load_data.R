# Read in outlier lists.
.read_outliers <- function(outliers_file) {
    outliers_col_names <- c("Pos_1", "Pos_2", "Distance", "Direct", "MI", "MI_wogaps", "Gap_effect", "Extreme")
    outliers_col_types <- c("integer", "integer", "integer", "logical", "numeric", "numeric", "numeric", "logical")
    if ("datapath" %in% colnames(outliers_file)) {
        .data$outliers <- try({
            readr::read_delim(file = outliers_file$datapath,
                              delim = " ",
                              col_types = "iiildddl",
                              col_names = outliers_col_names)
        }, silent = TRUE)

        if (inherits(.data$outliers, "try-error")) {
            error_msg <-.data$outliers
            .data$outliers <- NULL
            return(.status(.STATUS_FAILURE, paste0("Failed to read outliers file.",
                                                   "<br><br>",
                                                   error_msg)))
        }

        .data$outliers <- .data$outliers[order(.data$outliers$Direct == FALSE), ]
        .data$outliers_direct <- .data$outliers[.data$outliers$Direct == TRUE, ]
    } else {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid outliers file data."))
    }

    return(.status(.STATUS_SUCCESS, ""))
}

# Read in tree.
.read_tree <- function(tree_file) {
    if ("datapath" %in% colnames(tree_file)) {
        filepath <- tree_file$datapath

        if (endsWith(filepath, ".nex")) {
            .data$tree <- try({ treeio::read.nexus(file = filepath) }, silent = TRUE)
        } else if (endsWith(filepath, ".nwk")) {
            .data$tree <- try({ treeio::read.newick(file = filepath) }, silent = TRUE)
        } else {
            return(.status(.STATUS_FAILURE, "Unknown format for tree file: file must end in .nex or .nwk."))
        }

        if (inherits(.data$tree, "try-error")) {
            error_msg <- .data$tree
            .data$tree <- NULL
            return(.status(.STATUS_FAILURE, paste0("Failed to read tree file.",
                                                   "<br><br>",
                                                   error_msg)))
        }
    } else {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid tree file data."))
    }

    return(.status(.STATUS_SUCCESS, ""))
}

# Read in MSA from fasta and loci files.
.read_msa <- function(fasta_file, loci_file) {
    if ("datapath" %in% colnames(fasta_file) && "datapath" %in% colnames(loci_file)) {
        # Read sequences.
        fa <- try({
            seqinr::read.fasta(file = fasta_file$datapath,
                               seqtype = "DNA",
                               set.attributes = FALSE)
        }, silent = TRUE)

        if (inherits(fa, "try-error")) {
            return(.status(.STATUS_FAILURE, paste0("Failed to read fasta file.",
                                                   "<br><br>",
                                                   fa)))
        }

        # All fasta sequences must have the same length.
        unique_seq_lengths <- unique(sapply(fa, length))
        if (length(unique_seq_lengths) != 1) {
            return(.status(.STATUS_FAILURE, paste0("Fasta sequences must have the same length.")))
        }

        # Read SNP positions.
        snp_loci <- try({
            readr::read_delim(file = loci_file$datapath,
                              delim = " ",
                              col_names = "pos",
                              col_types = "i")
        }, silent = TRUE)

        if (inherits(snp_loci, "try-error")) {
            return(.status(.STATUS_FAILURE, paste0("Failed to read loci file.",
                                                   "<br><br>",
                                                   snp_loci)))
        }

        # Convert list of sequences to a matrix and upper case.
        .data$msa <- toupper(do.call(rbind, fa))

        if (ncol(.data$msa) != nrow(snp_loci)) {
            .data$msa <- NULL
            return(.status(.STATUS_FAILURE, paste0("Number of SNP loci does not match fasta sequence length.")))
        }
        rownames(.data$msa) <- names(fa)
        colnames(.data$msa) <- snp_loci$pos
    } else {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid fasta/loci file data."))
    }

    return(.status(.STATUS_SUCCESS, ""))
}

# Read in phenotype data file.
.read_phenotype <- function(phenotype_file) {
    if ("datapath" %in% colnames(phenotype_file)) {
        .data$phenotype <- try({ utils::read.csv(file = phenotype_file$datapath, row.names = 1) }, silent = TRUE)
        if (inherits(.data$phenotype, "try-error")) {
            error_msg <- .data$phenotype
            .data$phenotype <- NULL
            return(.status(.STATUS_FAILURE, paste0("Failed to read phenotypic data file.",
                                                   "<br><br>",
                                                   error_msg)))
        }
    } else {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid phenotypic data file data."))
    }

    return(.status(.STATUS_SUCCESS, ""))
}

# Determine ranges from a GFF3 file.
.determine_ranges <- function(gff_filepath) {
    ranges <- NULL

    # Region type explicitly given.
    if (any(.data$gff$type == "region")) {
        ranges <- as.numeric(dplyr::select(.data$gff[.data$gff$type == "region", ],
                                           "start",
                                           "end"))
    } else {
        # Check ##sequence-region pragma.
        input_file <- readLines(gff_filepath)
        found_str <- grep(pattern = "^##sequence-region",
                          x = input_file,
                          value = TRUE)
        if (length(found_str) > 0) {
            # Remove any tab characters.
            found_str <- gsub('(\t)', '', found_str)
            found_str_values <- strsplit(found_str, "\\s+")[[1]]
            ranges <- as.numeric(found_str_values[3:4])
        } else {
            # Get end from the maximum value.
            ranges <- c(1, max(.data$gff$end))
        }
    }
    return(ranges)
}

# Read in gff data and assign to global variables
.read_gff <- function(gff_file) {
    if ("datapath" %in% colnames(gff_file)) {
        .data$gff <- try({ ape::read.gff(file = gff_file$datapath, GFF3 = TRUE) }, silent = TRUE)
        if (inherits(.data$gff, "try-error")) {
            error_msg <- .data$gff
            .data$gff <- NULL
            return(.status(.STATUS_FAILURE, paste0("Failed to read GFF3 file.",
                                                   "<br><br>",
                                                   error_msg)))
        }
        ranges <- .determine_ranges(gff_file$datapath)

        # Determine which type to filter.
        gff_types <- unique(as.character(.data$gff$type))
        if ("gene" %in% gff_types) {
            gff_type_filter <- "gene"
        } else if ("CDS" %in% gff_types) {
            gff_type_filter <- "CDS"
        } else {
            .data$gff <- NULL
            return(.status(.STATUS_FAILURE, paste0("Failed to determine filter type for GFF3 file.",
                                                   "<br><br>",
                                                   "Expected types 'gene' or 'CDS' or not found.")))
        }
        .data$gff <- dplyr::select(.data$gff[.data$gff$type == gff_type_filter, ], "start", "end", "attributes")
        .data$gff$Name <- .cpp_get_gff_name_from_attributes(.data$gff$attributes)
        .data$gff$attributes <- NULL
        .data$gff <- .cpp_add_igrs_to_gff(.data$gff, .data$outliers_direct, ranges)
        .data$gff <- .data$gff[order(.data$gff$start), ]
    } else {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid GFF3 file data."))
    }

    return(.status(.STATUS_SUCCESS, ""))
}

# Read files uploaded from the Shiny UI.
.read_data <- function(outliers_file, tree_file, fasta_file, loci_file, phenotype_file, gff_file) {
    # Clear any previous data.
    .data$outliers <- NULL
    .data$outliers_direct <- NULL
    .data$tree <- NULL
    .data$msa <- NULL
    .data$phenotype <- NULL
    .data$gff <- NULL

    # Read outliers file.
    if (is.null(outliers_file)) {
        return(.status(.STATUS_FAILURE, "Outliers file must be provided."))
    }
    read_outliers_status <- .read_outliers(outliers_file)
    if (read_outliers_status$success == .STATUS_FAILURE) {
        return(read_outliers_status)
    }

    # Read tree, fasta and loci files if provided.
    if (!is.null(tree_file) && !is.null(fasta_file) && !is.null(loci_file)) {
        read_tree_status <- .read_tree(tree_file)
        if (read_tree_status$success == .STATUS_FAILURE) {
            return(read_tree_status)
        }
        read_msa_status <- .read_msa(fasta_file, loci_file)
        if (read_msa_status$success == .STATUS_FAILURE) {
            return(read_msa_status)
        }
    }

    # Read phenotype file if provided.
    if (!is.null(phenotype_file)) {
        read_phenotype_status <- .read_phenotype(phenotype_file)
        if (read_phenotype_status$success == .STATUS_FAILURE) {
            return(read_phenotype_status)
        }
    }

    # Read GFF3 file if provided.
    if (!is.null(gff_file)) {
        read_gff_status <- .read_gff(gff_file)
        if (read_gff_status$success == .STATUS_FAILURE) {
            return(read_gff_status)
        }
        .precompute_circular_plot_data()
    }
    # Compile status message listing successfully read files.
    status_msg <- paste0("Read in files:<br>- ", outliers_file$name)
    if (!is.null(tree_file) && !is.null(fasta_file) && !is.null(loci_file)) {
        status_msg <- paste0(status_msg, "<br>- ", tree_file$name)
        status_msg <- paste0(status_msg, "<br>- ", fasta_file$name)
        status_msg <- paste0(status_msg, "<br>- ", loci_file$name)
    }
    if (!is.null(phenotype_file)) {
        status_msg <- paste0(status_msg, "<br>- ", phenotype_file$name)
    }
    if (!is.null(gff_file)) {
        status_msg <- paste0(status_msg, "<br>- ", gff_file$name)
    }
    return(.status(.STATUS_SUCCESS, status_msg))
}

.clear_data <- function() {
    cleared_data <- ""
    # Clear outliers.
    if (!is.null(.data$outliers)) {
        cleared_data <- paste0(cleared_data, "<br>- Outliers")
    }
    # Clear tree and MSA.
    if (!is.null(.data$tree) && !is.null(.data$msa)) {
        cleared_data <- paste0(cleared_data, "<br>- Tree file",
                               "<br>- Fasta file",
                               "<br>- Loci file")
    }
    # Clear phenotype file.
    if (!is.null(.data$phenotype)) {
        cleared_data <- paste0(cleared_data, "<br>- Phenotypic data file")
    }
    # Clear GFF3 file.
    if (!is.null(.data$gff)) {
        cleared_data <- paste0(cleared_data, "<br>- GFF3 file")
    }

    .data$outliers <- NULL
    .data$outliers_direct <- NULL
    .data$tree <- NULL
    .data$msa <- NULL
    .data$phenotype <- NULL
    .data$gff <- NULL
    .data$edges <- NULL

    if (length(cleared_data) == 0) {
        cleared_data <- "There was no data to clear."
    } else {
        cleared_data <- paste0("Cleared the following data:<br>", cleared_data)
    }
    return(.status(.STATUS_SUCCESS, cleared_data))
}

.reset_uploaded_files <- function() {
    reset_result <- try({ shinyjs::reset("outliers_file")
                          shinyjs::reset("tree_file")
                          shinyjs::reset("fasta_file")
                          shinyjs::reset("loci_file")
                          shinyjs::reset("phenotype_file")
                          shinyjs::reset("gff_file") }, silent = TRUE)
    if (inherits(reset_result, "try-error")) {
        return(.status(.STATUS_FAILURE, paste0("Failed to reset uploaded files.",
                                               "<br><br>",
                                               reset_result)))
    }
    return(.status(.STATUS_SUCCESS, ""))
}
