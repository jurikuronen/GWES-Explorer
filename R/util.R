# Checks for whether data has been read.
.gff_is_not_null <- function() { !is.null(.data$gff) }
.tree_is_not_null <- function() { !is.null(.data$tree) && !is.null(.data$msa) }
.phenotype_is_not_null <- function() { .tree_is_not_null() && !is.null(.data$phenotype) }
.outliers_is_not_null <- function() { !is.null(.data$outliers) }

# Column names in outliers table.
.default_outlier_columns <- function() { c("Pos_1", "Pos_2", "MI", "MI_wogaps", "Distance") }
.extended_outlier_columns <- function() { c("Pos_1", "Pos_2", "MI", "MI_wogaps", "Pos_1_gene_name", "Pos_2_gene_name", "Distance") }
