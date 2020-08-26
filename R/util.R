# Checks for whether data has been read.
.gff_is_not_null <- function() { !is.null(.data$gff) }
.tree_is_not_null <- function() { !is.null(.data$tree) && !is.null(.data$msa) }
.outliers_is_not_null <- function() { !is.null(.data$outliers) }

# Find gene that pos is part of. If not part of a gene, return -1.
# gff_genes must have been read to global namespace.
.get_gene <- function(pos) {
    n <- nrow(.data$gff_genes)
    for (i in 1:n) {
        if (pos >= .data$gff_genes$start[i] && (i == n || pos <= .data$gff_genes$start[i + 1])) { return(i) }
    }
    return(-1)
}

# Find region that gene is part of. Binary search.
.get_region_index <- function(region_indices, gene) {
    region_indices <- c(1, region_indices)
    a <- 2
    b <- length(region_indices)
    while (a <= b) {
        mid <- floor((a + b) / 2)
        if (gene >= region_indices[mid - 1] && gene <= region_indices[mid]) return(mid - 1)
        else if (gene < region_indices[mid - 1]) b <- mid - 1
        else if (gene > region_indices[mid]) a <- mid + 1
        else break
    }
    return(-1) # Should never return.
}

.get_region_indices <- function(n_regions) {
    n_genes <- nrow(.data$gff_genes)
    c(ceiling(1:(n_regions-1) * (n_genes / n_regions)), n_genes)
}

# Calculate region boundaries.
# Returns a string vector with "start-end" strings.
.get_region_boundaries <- function(region_indices) {
    region_boundaries <- c(paste(1, "-", region_indices[1], sep = ""))
    for (i in 2:length(region_indices)) { region_boundaries[i] <- c(paste(region_indices[i - 1] + 1, "-", region_indices[i], sep = "")) }
    return(region_boundaries)
}

# Precomputes necessary data for rendering the circular plot.
.precompute_circular_plot_data <- function() {
    n_groups <- .settings$circular_plot_n_groups
    n_regions_per_group <- .settings$circular_plot_n_regions
    # Precompute gene regions.
    region_indices <- .get_region_indices(n_groups * n_regions_per_group)
    n_genes <- nrow(.data$gff_genes)
    gene_regions <- numeric(n_genes)
    for (gene in 1:n_genes) { gene_regions[gene] <- .get_region_index(region_indices, gene) }
    .data$gene_regions <- gene_regions

    # Precompute pos regions.
    pos1_genes <- numeric(nrow(.data$outliers_direct))
    pos2_genes <- numeric(nrow(.data$outliers_direct))
    pos1_regions <- numeric(nrow(.data$outliers_direct))
    pos2_regions <- numeric(nrow(.data$outliers_direct))
    for (i in 1:nrow(.data$outliers_direct)) {
        pos1_genes[i] <- .get_gene(.data$outliers_direct$Pos_1[i])
        pos2_genes[i] <- .get_gene(.data$outliers_direct$Pos_2[i])
        pos1_regions[i] <- .get_region_index(region_indices, pos1_genes[i])
        pos2_regions[i] <- .get_region_index(region_indices, pos2_genes[i])
    }
    .data$pos1_genes <- pos1_genes
    .data$pos2_genes <- pos2_genes
    .data$pos1_regions <- pos1_regions
    .data$pos2_regions <- pos2_regions

    # Precompute main plot.
    circular_data <- .create_circular_data()
    top_level_dependencies <- .create_top_level_links(circular_data)
    edges <- .get_circular_spec(circular_data, top_level_dependencies)

    # Add gene data.
    gene_data <- .create_gene_data()
    pos_data <- .create_pos_data()
    edges$data <- append(edges$data, .get_gene_data(gene_data, pos_data))
    edges$marks <- append(edges$marks, .get_gene_marks())

    .data$edges <- edges
}

.add_gene_info_to_outliers <- function() {
    if ("Pos_1_gene" %in% names(.data$outliers_direct)) { # Update existing values
        .data$outliers_direct$Pos_1_gene = .data$gff_genes$Name[.data$pos1_genes]
        .data$outliers_direct$Pos_2_gene = .data$gff_genes$Name[.data$pos2_genes]
        .data$outliers_direct$Pos_1_region = .data$pos1_regions
        .data$outliers_direct$Pos_2_region = .data$pos2_regions
    } else {
        .data$outliers_direct <- cbind(.data$outliers_direct,
                                       Pos_1_gene = .data$gff_genes$Name[.data$pos1_genes],
                                       Pos_2_gene = .data$gff_genes$Name[.data$pos2_genes],
                                       Pos_1_region = .data$pos1_regions,
                                       Pos_2_region = .data$pos2_regions,
                                       stringsAsFactors = FALSE)
    }

    # The full outliers file only used in Manhattan GWES plot.
    # .data$outliers <- cbind(.data$outliers, Pos_1_gene = "NA", Pos_2_gene = "NA", stringsAsFactors = FALSE)
    # for (i in 1:nrow(.data$outliers_direct)) {
    #     idx <- which(.data$outliers$Pos_1 == .data$outliers_direct$Pos_1[i] & .data$outliers$Pos_2 == .data$outliers_direct$Pos_2[i])
    #     .data$outliers$Pos_1_gene[idx] <- .data$outliers_direct$Pos_1_gene[i]
    #     .data$outliers$Pos_2_gene[idx] <- .data$outliers_direct$Pos_2_gene[i]
    # }
}
