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

# Read in phenotype data file.
.read_phenotype <- function(phenotype_file) {
    if (!is.null(phenotype_file)) {
        .data$phenotype <- utils::read.csv(file = phenotype_file$datapath, row.names = 1)
    } else {
        .data$phenotype <- NULL
    }
}

# Read in outlier lists.
.read_outliers <- function(outliers_file) {
    if (!is.null(outliers_file)) {
        .data$outliers <- readr::read_delim(
            file = outliers_file$datapath, delim = " ", col_types = "iiildddl",
            col_names = c("Pos_1", "Pos_2", "Distance", "Direct", "MI", "MI_wogaps", "Gap_effect", "Extreme"))
        .data$outliers <- .data$outliers[order(.data$outliers$Direct == FALSE), ]
        if (.outliers_is_not_null()) {
            .data$outliers_direct <- .data$outliers[.data$outliers$Direct == TRUE, ]
        }
    } else { .data$outliers <- NULL; .data$outliers_direct <- NULL }
}

# Determine ranges from a gff3 file.
.determine_ranges <- function(gff, gff_file) {
    # Region type explicitly given.
    if (sum(gff$type == "region") > 0) {
        ranges <- as.numeric(dplyr::select(gff[gff$type == "region", ], "start", "end"))
    } else {
        # Check ##sequence-region pragma.
        fileInput <- readLines(gff_file$datapath)
        found <- grep("##sequence-region", fileInput)
        if (length(found) > 0) {
            found_str <- fileInput[found]
            found_str <- gsub('(\t)', '', found_str) # Remove any tab characters.
            found_str_values <- strsplit(found_str, "\\s+")[[1]]
            ranges <- as.numeric(found_str_values[3:4])
        } else {
            # Get end from the maximum value.
            ranges <- c(1, max(gff$end))
        }
    }
    return(ranges);
}

# Read in gff data and assign to global variables
.read_gff <- function(gff_file) {
    if (!is.null(gff_file)) {
        gff <- ape::read.gff(file = gff_file$datapath, GFF3 = TRUE)
        ranges <- .determine_ranges(gff, gff_file)
        # In case type "gene" doesn't exist, filter CDS or stop.
        gff_types <- unique(as.character(gff$type))
        if ("gene" %in% gff_types) {
            gff_type_filter <- "gene"
        } else if ("CDS" %in% gff_types) {
            gff_type_filter <- "CDS"
        } else {
            stop("Type \"gene\" or \"CDS\" not found in GFF.")
        }
        gff <- dplyr::filter(dplyr::select(gff, "start", "end", "attributes"), gff$type == gff_type_filter)
        gff$Name <- .cpp_extract_gff_name_from_attributes(gff$attributes)
        gff$attributes <- NULL
        gff <- .cpp_add_igrs_to_gff(gff, .data$outliers_direct, ranges)
        gff <- gff[order(gff$start), ]
        .data$gff <- gff
    } else { .data$gff <- NULL }
}

# Read files uploaded from the Shiny UI.
.read_data <- function(file_outliers = NULL, file_tree = NULL, file_fasta = NULL, file_loci = NULL, file_phenotype = NULL, file_gff = NULL) {
    .read_outliers(file_outliers)
    .read_tree(file_tree)
    .read_msa(file_fasta, file_loci)
    .read_phenotype(file_phenotype)
    .read_gff(file_gff)
    if (.gff_is_not_null()) {
        .precompute_circular_plot_data()
    }
}
