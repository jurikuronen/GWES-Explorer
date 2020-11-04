# Checks for whether data has been read.
.gff_is_not_null <- function() { !is.null(.data$gff) }
.tree_is_not_null <- function() { !is.null(.data$tree) && !is.null(.data$msa) }
.phenotype_is_not_null <- function() { .tree_is_not_null() && !is.null(.data$phenotype) }
.outliers_is_not_null <- function() { !is.null(.data$outliers) }

# Initialize data keys.
.initialize_data_keys <- function() {
    .data$outliers <- NULL
    .data$outliers_direct <- NULL
    .data$msa <- NULL
    .data$tree <- NULL
    .data$phenotype <- NULL
    .data$gff <- NULL
}

.svg_lite_is_installed <- function() { requireNamespace("svg_lite", quietly = TRUE) }
