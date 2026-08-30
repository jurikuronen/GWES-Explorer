# Reads a SpydrPick outliers file.
.read_outliers <- function(data, outliers_file) {
    if (!("datapath" %in% colnames(outliers_file))) {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid outliers file data."))
    }

    data$outliers <- try({
        column_names <- c("Pos_1", "Pos_2", "Distance", "Direct", "MI", "MI_wogaps")

        column_count <- readr::count_fields(
            file = outliers_file$datapath,
            tokenizer = readr::tokenizer_delim(delim = " "),
            n_max = 1
        )

        if (length(column_count) != 1L || is.na(column_count) || column_count < 5L) {
            stop("Outliers file must contain at least five columns.")
        }

        stats::setNames(
            readr::read_delim(
                file = outliers_file$datapath,
                delim = " ",
                col_names = FALSE,
                col_types = paste0(
                    substr("iiildd", 1L, min(column_count, 6L)),
                    # Skip columns after MI_wogaps.
                    strrep("_", max(column_count - 6L, 0L))
                )
            ),
            column_names[seq_len(min(column_count, 6L))]
        )
    }, silent = TRUE)

    if (inherits(data$outliers, "try-error")) {
        error_msg <- data$outliers
        data$outliers <- NULL
        return(.status(.STATUS_FAILURE, paste0("Failed to read outliers file.",
                                               "<br><br>",
                                               .escape_html(error_msg))))
    }

    data$outliers <- data$outliers[order(data$outliers$Direct == FALSE), ]
    data$outliers_direct <- data$outliers[data$outliers$Direct == TRUE, ]

    if (nrow(data$outliers_direct) == 0) {
        return(.status(.STATUS_FAILURE, "Outliers file must contain at least one direct outlier link."))
    }

    return(.status(.STATUS_SUCCESS, ""))
}

# Reads a phylogenetic tree.
.read_tree <- function(data, tree_file) {
    if (!all(c("datapath", "name") %in% colnames(tree_file))) {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid tree file data."))
    }

    filepath <- tree_file$datapath
    filename <- tree_file$name

    if (endsWith(filename, ".nex")) {
        data$tree <- try(treeio::read.nexus(file = filepath), silent = TRUE)
    } else if (endsWith(filename, ".nwk")) {
        data$tree <- try(treeio::read.newick(file = filepath), silent = TRUE)
    } else {
        return(.status(.STATUS_FAILURE, "Unknown format for tree file: file must end in .nex or .nwk."))
    }

    if (inherits(data$tree, "try-error")) {
        error_msg <- data$tree
        data$tree <- NULL
        return(.status(.STATUS_FAILURE, paste0("Failed to read tree file.",
                                               "<br><br>",
                                               .escape_html(error_msg))))
    }

    return(.status(.STATUS_SUCCESS, ""))
}

# Reads an MSA from FASTA and loci files.
.read_msa <- function(data, fasta_file, loci_file) {
    if (!("datapath" %in% colnames(fasta_file) && "datapath" %in% colnames(loci_file))) {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid fasta/loci file data."))
    }

    # Read sequences.
    sequences <- try(seqinr::read.fasta(file = fasta_file$datapath,
                                         seqtype = "DNA",
                                         set.attributes = FALSE),
                     silent = TRUE)

    if (inherits(sequences, "try-error")) {
        return(.status(.STATUS_FAILURE, paste0("Failed to read FASTA file.",
                                               "<br><br>",
                                               .escape_html(sequences))))
    }

    # All FASTA sequences must have the same length.
    unique_seq_lengths <- unique(sapply(sequences, length))
    if (length(unique_seq_lengths) != 1) {
        return(.status(.STATUS_FAILURE, "FASTA sequences must have the same length."))
    }

    # Read SNP positions.
    snp_loci <- try(readr::read_delim(file = loci_file$datapath,
                                      delim = " ",
                                      col_names = "pos",
                                      col_types = "i"),
                    silent = TRUE)

    if (inherits(snp_loci, "try-error")) {
        return(.status(.STATUS_FAILURE, paste0("Failed to read loci file.",
                                               "<br><br>",
                                               .escape_html(snp_loci))))
    }

    # Convert list of sequences to a matrix and upper case.
    data$msa <- toupper(do.call(rbind, sequences))

    if (ncol(data$msa) != nrow(snp_loci)) {
        data$msa <- NULL
        return(.status(.STATUS_FAILURE, "Number of SNP loci does not match FASTA sequence length."))
    }
    rownames(data$msa) <- names(sequences)
    colnames(data$msa) <- snp_loci$pos

    return(.status(.STATUS_SUCCESS, ""))
}

# Reads a phenotype data file.
.read_phenotype <- function(data, phenotype_file) {
    if (!("datapath" %in% colnames(phenotype_file))) {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid phenotype file data."))
    }

    data$phenotype <- try(utils::read.csv(file = phenotype_file$datapath, row.names = 1), silent = TRUE)
    if (inherits(data$phenotype, "try-error")) {
        error_msg <- data$phenotype
        data$phenotype <- NULL
        return(.status(.STATUS_FAILURE, paste0("Failed to read phenotype data file.",
                                               "<br><br>",
                                               .escape_html(error_msg))))
    }

    return(.status(.STATUS_SUCCESS, ""))
}

# Determines the range from a GFF3 file.
.determine_ranges <- function(data, gff_filepath) {
    # Check if a "region" row gives the range.
    region_rows <- data$gff[data$gff$type == "region", c("start", "end"), drop = FALSE]

    # Check if a ##sequence-region pragma gives the range.
    gff_lines <- readLines(gff_filepath)
    pragma_lines <- grep(pattern = "^##sequence-region", x = gff_lines, value = TRUE)

    if (nrow(region_rows) > 1L || length(pragma_lines) > 1L) {
        stop("Only one GFF3 region is supported.")
    }

    if (length(pragma_lines) == 1L) {
        pragma_fields <- strsplit(pragma_lines[[1L]], "\\s+")[[1L]]
        if (length(pragma_fields) != 4L) {
            stop("Invalid ##sequence-region pragma: expected a sequence ID, start and end.")
        }
        ranges <- as.numeric(pragma_fields[3:4])

        # If both are present, their ranges must match.
        if (nrow(region_rows) == 1L) {
            region_ranges <- as.numeric(c(region_rows$start[[1L]], region_rows$end[[1L]]))

            if (!identical(ranges, region_ranges)) {
                stop("GFF3 region row and ##sequence-region pragma must have the same range.")
            }
        }
    } else if (nrow(region_rows) == 1L) {
        ranges <- as.numeric(c(region_rows$start[[1L]], region_rows$end[[1L]]))
    } else {
        # Otherwise, use position 1 and the maximum feature end.
        ranges <- c(1, max(data$gff$end))
    }

    if (!isTRUE(ranges[[1L]] == 1)) {
        stop("GFF3 region must start at position 1.")
    }

    # Require at least a one kilobase span for each circular plot region.
    minimum_range_length <- .settings$circular_plot_region_count * 1000L
    if (ranges[[2L]] - ranges[[1L]] + 1L < minimum_range_length) {
        stop(paste0("GFF3 region must span at least ",
                    format(minimum_range_length, big.mark = ",", scientific = FALSE, trim = TRUE),
                    " bases."))
    }

    return(ranges)
}

# Reads a GFF3 file.
.read_gff <- function(data, gff_file) {
    if (!("datapath" %in% colnames(gff_file))) {
        return(.status(.STATUS_FAILURE, "Internal error: got invalid GFF3 file data."))
    }

    data$gff <- try(ape::read.gff(file = gff_file$datapath, GFF3 = TRUE), silent = TRUE)
    if (inherits(data$gff, "try-error")) {
        error_msg <- data$gff
        data$gff <- NULL
        return(.status(.STATUS_FAILURE, paste0("Failed to read GFF3 file.",
                                               "<br><br>",
                                               .escape_html(error_msg))))
    }
    ranges <- try(.determine_ranges(data, gff_file$datapath), silent = TRUE)
    if (inherits(ranges, "try-error")) {
        error_msg <- ranges
        data$gff <- NULL
        return(.status(.STATUS_FAILURE, paste0("Failed to determine GFF3 region.",
                                               "<br><br>",
                                               .escape_html(error_msg))))
    }

    # Use gene features when available, otherwise use CDS features.
    gff_types <- unique(as.character(data$gff$type))
    if ("gene" %in% gff_types) {
        gff_type_filter <- "gene"
    } else if ("CDS" %in% gff_types) {
        gff_type_filter <- "CDS"
    } else {
        data$gff <- NULL
        return(.status(.STATUS_FAILURE, "GFF3 file contains no 'gene' or 'CDS' features."))
    }

    # Keep the feature locations and attributes needed for the circular plot.
    data$gff <- dplyr::select(data$gff[data$gff$type == gff_type_filter, ], "start", "end", "attributes")

    # Extract feature names from the GFF3 attributes.
    data$gff$Name <- .cpp_get_gff_name_from_attributes(data$gff$attributes)

    # The full attribute strings are no longer needed.
    data$gff$attributes <- NULL

    # Sort the features in genomic order for intergenic region (IGR) calculation.
    data$gff <- data$gff[order(data$gff$start, data$gff$end), ]

    # Find all IGRs with at least one outlier position and append them to the GFF3 data.
    igrs <- .cpp_find_igrs_with_outliers(data$gff$start, data$gff$end, data$outliers_direct, ranges)
    data$gff <- rbind(data$gff, igrs)

    # The new IGR rows were appended, so the data must be sorted again.
    data$gff <- data$gff[order(data$gff$start, data$gff$end), ]

    return(.status(.STATUS_SUCCESS, ""))
}

# Reads files uploaded from the Shiny UI.
.read_data <- function(data, outliers_file, tree_file, fasta_file, loci_file, phenotype_file, gff_file) {
    if (is.null(outliers_file)) {
        return(.status(.STATUS_FAILURE, "Outliers file must be provided."))
    }

    tree_data_files_provided <- c(!is.null(tree_file),
                                  !is.null(fasta_file),
                                  !is.null(loci_file))
    if (any(tree_data_files_provided) && !all(tree_data_files_provided)) {
        return(.status(.STATUS_FAILURE,
                       "Tree plot requires all three files: tree, FASTA and loci."))
    }

    # Load data into a temporary environment so failures leave the current session data unchanged.
    loaded_data <- new.env(parent = emptyenv())
    loaded_data$outliers <- NULL
    loaded_data$outliers_direct <- NULL
    loaded_data$tree <- NULL
    loaded_data$msa <- NULL
    loaded_data$phenotype <- NULL
    loaded_data$gff <- NULL
    loaded_data$circular_plot_spec <- NULL

    # Read outliers file.
    read_outliers_status <- .read_outliers(loaded_data, outliers_file)
    if (read_outliers_status$success == .STATUS_FAILURE) {
        return(read_outliers_status)
    }

    # Read tree, FASTA and loci files if provided.
    if (all(tree_data_files_provided)) {
        read_tree_status <- .read_tree(loaded_data, tree_file)
        if (read_tree_status$success == .STATUS_FAILURE) {
            return(read_tree_status)
        }
        read_msa_status <- .read_msa(loaded_data, fasta_file, loci_file)
        if (read_msa_status$success == .STATUS_FAILURE) {
            return(read_msa_status)
        }
    }

    # Read phenotype file if provided.
    if (!is.null(phenotype_file)) {
        read_phenotype_status <- .read_phenotype(loaded_data, phenotype_file)
        if (read_phenotype_status$success == .STATUS_FAILURE) {
            return(read_phenotype_status)
        }
    }

    # Read GFF3 file if provided.
    if (!is.null(gff_file)) {
        read_gff_status <- .read_gff(loaded_data, gff_file)
        if (read_gff_status$success == .STATUS_FAILURE) {
            return(read_gff_status)
        }
        .precompute_circular_plot_data(loaded_data)
    }

    # List the files that were read.
    status_message <- paste0("Files read:<br>- ", .escape_html(outliers_file$name))
    if (all(tree_data_files_provided)) {
        status_message <- paste0(status_message, "<br>- ", .escape_html(tree_file$name))
        status_message <- paste0(status_message, "<br>- ", .escape_html(fasta_file$name))
        status_message <- paste0(status_message, "<br>- ", .escape_html(loci_file$name))
    }
    if (!is.null(phenotype_file)) {
        status_message <- paste0(status_message, "<br>- ", .escape_html(phenotype_file$name))
    }
    if (!is.null(gff_file)) {
        status_message <- paste0(status_message, "<br>- ", .escape_html(gff_file$name))
    }

    # Copy loaded data into the session. The temporary environment will be garbage collected.
    for (key in c("outliers", "outliers_direct", "tree", "msa", "phenotype", "gff", "circular_plot_spec")) {
        data[[key]] <- loaded_data[[key]]
    }

    return(.status(.STATUS_SUCCESS, status_message))
}

# Clears the loaded data.
.clear_data <- function(data) {
    status_message <- ""
    # Clear outliers.
    if (!is.null(data$outliers)) {
        status_message <- paste0(status_message, "<br>- Outliers")
    }
    # Clear tree and MSA.
    if (!is.null(data$tree) && !is.null(data$msa)) {
        status_message <- paste0(status_message, "<br>- Tree file",
                                 "<br>- FASTA file",
                                 "<br>- Loci file")
    }
    # Clear phenotype file.
    if (!is.null(data$phenotype)) {
        status_message <- paste0(status_message, "<br>- Phenotype data file")
    }
    # Clear GFF3 file.
    if (!is.null(data$gff)) {
        status_message <- paste0(status_message, "<br>- GFF3 file")
    }

    data$outliers <- NULL
    data$outliers_direct <- NULL
    data$tree <- NULL
    data$msa <- NULL
    data$phenotype <- NULL
    data$gff <- NULL
    data$circular_plot_spec <- NULL

    if (!nzchar(status_message)) {
        status_message <- "There was no data to clear."
    } else {
        status_message <- paste0("Cleared the following data:<br>", status_message)
    }

    return(.status(.STATUS_SUCCESS, status_message))
}

# Clears the file selections.
.clear_file_selections <- function() {
    reset_result <- try({
        shinyjs::reset("outliers_file")
        shinyjs::reset("tree_file")
        shinyjs::reset("fasta_file")
        shinyjs::reset("loci_file")
        shinyjs::reset("phenotype_file")
        shinyjs::reset("gff_file")
    }, silent = TRUE)
    if (inherits(reset_result, "try-error")) {
        return(.status(.STATUS_FAILURE, paste0("Failed to clear file selections.",
                                               "<br><br>",
                                               .escape_html(reset_result))))
    }

    return(.status(.STATUS_SUCCESS, ""))
}
