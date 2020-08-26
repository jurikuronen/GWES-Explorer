# Read in tree.
.read_tree <- function(tree_file) {
    if (!is.null(tree_file)) {
        if (endsWith(tree_file$datapath, ".nex")) {
            .data$tree <- treeio::read.nexus(file = tree_file$datapath)
        } else if (endsWith(tree_file$datapath, ".nwk")) {
            .data$tree <- treeio::read.newick(file = tree_file$datapath)
        } else {
            .data$tree <- NULL
            print("Unknown format for tree file: missing suffix .nex or .nwk.")
        }
    } else { .data$tree <- NULL }
}

# Read in MSA from fasta and loci files.
.read_msa <- function(fasta_file, loci_file) {
    if (!is.null(fasta_file) && !is.null(loci_file)) {
        fa <- seqinr::read.fasta(file = fasta_file$datapath, seqtype = "DNA", set.attributes = FALSE)
        snp_loci <- readr::read_delim(file = loci_file$datapath, delim = " ", col_names = c("pos"), col_types = "i")
        msa <- matrix(0, length(fa), length(fa[[1]]))
        for (i in 1:length(fa)) {
            msa[i,] <- fa[[i]]
        }
        msa <- toupper(msa)
        rownames(msa) <- names(fa)
        colnames(msa) <- snp_loci[["pos"]]
        .data$msa <- msa
    } else { .data$msa <- NULL }
}


# Read in outlier lists.
.read_outliers <- function(outliers_file) {
    if (!is.null(outliers_file)) {
        Direct <- NULL # R CMD check hack.
        .data$outliers <- dplyr::arrange(readr::read_delim(
            file = outliers_file$datapath,
            delim = " ",
            col_types = "iiildddl",
            col_names = c("Pos_1", "Pos_2", "Distance", "Direct", "MI", "MI_wogaps", "Gap_effect", "Extreme")), Direct)
        if (.outliers_is_not_null()) {
            .data$outliers_direct <- dplyr::filter(.data$outliers, Direct == TRUE)
        }
    } else { .data$outliers <- NULL; .data$outliers_direct <- NULL }
}

# Read in gff data and assign to global variables
.read_gff <- function(gff_file) {
    if (!is.null(gff_file)) {
        type <- NULL # R CMD check hack.
        gff_raw <- ape::read.gff(file = gff_file$datapath, GFF3 = TRUE)
        gff_raw <- .convert_factors_to_characters(gff_raw)
        gff_attribute_names <- .split_gff_attribute_names(gff_raw)
        .data$gff <- .add_gff_attributes_as_columns(gff_raw, gff_attribute_names)

        gff_genes <- dplyr::filter(.data$gff, type == "gene")
        # Allow max 7 character gene names.
        gff_genes$Name <- stringi::stri_reverse(substr(stringi::stri_reverse(gff_genes$Name), 0, 7))
        .data$gff_genes <- .add_igrs_to_gff(gff_genes)
    } else { .data$gff <- NULL; .data$gff_genes <- NULL }
}

# Returns true if an outlier position is between start and end inclusive.
.outlier_in_range <- function(start, end) {
    if (any(.data$outliers_direct$Pos_1 >= start & .data$outliers_direct$Pos_1 <= end) ||
        any(.data$outliers_direct$Pos_2 >= start & .data$outliers_direct$Pos_2 <= end)) {
        return(TRUE)
    }
    return(FALSE)
}

# Add IGRs (intergenic regions) to the gff data for those outlier positions that are not part of genes.
# For now, assumes the provided gff file is sorted.
.add_igrs_to_gff <- function(gff_genes) {
    igr_ranges <- tibble::tibble(start = numeric(), end = numeric())
    if (gff_genes$start[1] > .data$region_start) {
        if (.outlier_in_range(.data$region_start, gff_genes$start[1] - 1)) {
            igr_ranges <- tibble::add_row(igr_ranges, start = .data$region_start, end = gff_genes$start[1] - 1)
        }
    }
    n_genes <- nrow(gff_genes)
    for (i in 1:(n_genes-1)) {
        if (gff_genes$end[i] < gff_genes$start[i + 1] - 1) {
            if (.outlier_in_range(gff_genes$end[i] + 1, gff_genes$start[i + 1] - 1)) {
                igr_ranges <- tibble::add_row(igr_ranges, start = gff_genes$end[i] + 1, end = gff_genes$start[i + 1] - 1)
            }
        }
    }
    if (gff_genes$end[n_genes] < .data$region_end) {
        if (.outlier_in_range(gff_genes$end[n_genes] + 1, .data$region_end)) {
            igr_ranges <- tibble::add_row(igr_ranges, start = gff_genes$end[n_genes] + 1, end = .data$region_end)
        }
    }
    n_igrs <- nrow(igr_ranges)
    gff_genes[(n_genes+1):(n_genes+n_igrs), ] <- NA
    gff_genes$start[(n_genes+1):(n_genes+n_igrs)] <- igr_ranges$start
    gff_genes$end[(n_genes+1):(n_genes+n_igrs)] <- igr_ranges$end
    gff_genes$Name[(n_genes+1):(n_genes+n_igrs)] <- "IGR"
    gff_genes <- gff_genes[order(gff_genes$start), ]
    return(gff_genes)
}

# Parse the attributes field in the gff file.
.split_gff_attribute_names <- function(gff_raw) {
    gff_attribute_names <- character(0)
    for (i in 1:nrow(gff_raw)) {
        if (tolower(gff_raw$type[i]) == "region") { next } # Skip attributes for entire region.
        attributes <- strsplit(gff_raw$attributes[i], ";")[[1]]
        for (j in 1:length(attributes)) {
            attribute <- substring(attributes[j], 0, regexpr("=", attributes[j])[1] - 1)
            if (attribute %in% gff_attribute_names) { next }
            gff_attribute_names[length(gff_attribute_names) + 1] <- attribute
        }
    }
    return(gff_attribute_names)
}

# Parse tags from the attributes field in the gff file and add them as columns for the R data frame.
# Slow function in R.
.add_gff_attributes_as_columns <- function(gff_raw, gff_attribute_names) {
    # Replace original attributes column with individual attribute columns.
    #start_time <- Sys.time()
    gff <- data.frame(matrix(nrow = nrow(gff_raw), ncol = ncol(gff_raw) - 1 + length(gff_attribute_names)))
    raw_cols <- 1:(ncol(gff_raw) - 1)
    colnames(gff) <- c(colnames(gff_raw)[raw_cols], gff_attribute_names)
    new_idx <- 0
    for (i in 1:nrow(gff_raw)) {
        if (tolower(gff_raw$type[i]) == "region") {
            .data$region_start <- gff_raw$start[i]
            .data$region_end <- gff_raw$end[i]
            next # Skip attributes for entire region.
        }
        new_idx <- new_idx + 1
        gff[new_idx, raw_cols] <- gff_raw[i, raw_cols]
        # Parse attributes and values.
        attributes_raw <- strsplit(gff_raw$attributes[i], ";")[[1]]
        n_attributes <- length(attributes_raw); names <- character(); values <- character()
        for (i in 1:n_attributes) {
            names[i] <- substring(attributes_raw[i], 0, regexpr("=", attributes_raw[i])[1] - 1)
            values[i] <- substring(attributes_raw[i], regexpr("=", attributes_raw[i])[1] + 1)
        }
        gff[new_idx, names] <- values
    }
    gff <- gff[1:new_idx, ]
    #end_time <- Sys.time()
    #end_time - start_time
    unique_columns <- logical(ncol(gff))
    for (i in 1:ncol(gff)) {
        if (sum(!is.na(unique(gff[, i]))) > 1) unique_columns[i] <- TRUE
    }
    gff <- gff[, unique_columns]
    return(gff)
}

# Convert factor columns to character columns.
.convert_factors_to_characters <- function(gff_raw) {
    factor_column_indices <- sapply(gff_raw, is.factor)
    gff_raw[factor_column_indices] <- lapply(gff_raw[factor_column_indices], as.character)
    return(gff_raw)
}

# Read files uploaded from the Shiny UI.
.read_data <- function(file_tree = NULL, file_fasta = NULL, file_loci = NULL, file_outliers = NULL, file_gff = NULL) {
    .read_tree(file_tree)
    .read_msa(file_fasta, file_loci)
    .read_outliers(file_outliers)
    .read_gff(file_gff)
    if (.gff_is_not_null()) { # Might move these routines under .read_gff() function.
        .precompute_circular_plot_data()
        .add_gene_info_to_outliers()
    }
}
