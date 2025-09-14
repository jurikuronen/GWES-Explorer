# Read in tree.
.read_tree <- function(tree_file) {
    tree <- NULL

    if (!is.null(tree_file) && !is.null(tree_file$datapath)) {
        filepath <- tree_file$datapath

        if (endsWith(filepath, ".nex")) {
            tree <- treeio::read.nexus(file = filepath)
        } else if (endsWith(filepath, ".nwk")) {
            tree <- treeio::read.newick(file = filepath)
        } else {
            print("Unknown format for tree file: missing suffix .nex or .nwk.")
        }
    }

    .data$tree <- tree
}

# Read in MSA from fasta and loci files.
.read_msa <- function(fasta_file, loci_file) {
    msa <- NULL

    if (!is.null(fasta_file) &&
        !is.null(loci_file) &&
        !is.null(fasta_file$datapath) &&
        !is.null(loci_file$datapath))
    {
        # Read sequences.
        fa <- seqinr::read.fasta(file = fasta_file$datapath,
                                 seqtype = "DNA",
                                 set.attributes = FALSE)
        # Read SNP positions.
        snp_loci <- readr::read_delim(file = loci_file$datapath,
                                      delim = " ",
                                      col_names = "pos",
                                      col_types = "i")
        # Convert list of sequences to a matrix and upper case.
        msa <- toupper(do.call(rbind, fa))
        rownames(msa) <- names(fa)
        colnames(msa) <- snp_loci$pos
    }

    .data$msa <- msa
}

# Read in phenotype data file.
.read_phenotype <- function(phenotype_file) {
    if (!is.null(phenotype_file) && !is.null(phenotype_file$datapath)) {
        .data$phenotype <- utils::read.csv(file = phenotype_file$datapath,
                                           row.names = 1)
    } else {
        .data$phenotype <- NULL
    }
}

# Read in outlier lists.
.read_outliers <- function(outliers_file) {
    outliers <- NULL
    outliers_direct <- NULL

    if (!is.null(outliers_file) && !is.null(outliers_file$datapath)) {
        outliers <- readr::read_delim(file = outliers_file$datapath,
                                      delim = " ",
                                      col_types = "iiildddl",
                                      col_names = c("Pos_1",
                                                    "Pos_2",
                                                    "Distance",
                                                    "Direct",
                                                    "MI",
                                                    "MI_wogaps",
                                                    "Gap_effect",
                                                    "Extreme"))
        outliers <- outliers[order(outliers$Direct == FALSE), ]
        if (!is.null(outliers)) {
            outliers_direct <- outliers[outliers$Direct == TRUE, ]
        }
    }

    .data$outliers <- outliers
    .data$outliers_direct <- outliers_direct
}

# Determine ranges from a GFF3 file.
.determine_ranges <- function(gff, gff_filepath) {
    ranges <- NULL

    # Region type explicitly given.
    if (any(gff$type == "region")) {
        ranges <- as.numeric(dplyr::select(gff[gff$type == "region", ],
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
            ranges <- c(1, max(gff$end))
        }
    }
    return(ranges)
}

# Read in gff data and assign to global variables
.read_gff <- function(gff_file) {
    gff <- NULL

    if (!is.null(gff_file) && !is.null(gff_file$datapath)) {
        gff <- ape::read.gff(file = gff_file$datapath,
                             GFF3 = TRUE)
        ranges <- .determine_ranges(gff, gff_file$datapath)

        # Determine which type to filter.
        gff_types <- unique(as.character(gff$type))
        if ("gene" %in% gff_types) {
            gff_type_filter <- "gene"
        } else if ("CDS" %in% gff_types) {
            gff_type_filter <- "CDS"
        } else {
            stop("Type \"gene\" or \"CDS\" not found in GFF.")
        }
        gff <- dplyr::filter(dplyr::select(gff,
                                           "start",
                                           "end",
                                           "attributes"),
                             gff$type == gff_type_filter)
        gff$Name <- .cpp_get_gff_name_from_attributes(gff$attributes)
        gff$attributes <- NULL
        gff <- .cpp_add_igrs_to_gff(gff,
                                    .data$outliers_direct,
                                    ranges)
        gff <- gff[order(gff$start), ]
    }

    .data$gff <- gff
}

# Read files uploaded from the Shiny UI.
.read_data <- function(outliers_file = NULL, tree_file = NULL, fasta_file = NULL, loci_file = NULL, phenotype_file = NULL, gff_file = NULL) {
    .read_outliers(outliers_file)
    .read_tree(tree_file)
    .read_msa(fasta_file, loci_file)
    .read_phenotype(phenotype_file)
    .read_gff(gff_file)
    if (!is.null(.data$gff)) {
        .precompute_circular_plot_data()
    }
}
