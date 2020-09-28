# Calculate region boundaries.
# Returns a string vector with "start-end" strings.
.get_region_boundaries <- function(region_indices) {
    region_boundaries <- c(paste(1, "-", region_indices[1], sep = ""))
    for (i in 2:length(region_indices)) { region_boundaries[i] <- c(paste(region_indices[i - 1] + 1, "-", region_indices[i], sep = "")) }
    return(region_boundaries)
}

.get_region_indices <- function(n_regions) {
    n_genes <- nrow(.data$gff)
    c(ceiling(1:(n_regions-1) * (n_genes / n_regions)), n_genes)
}

.compute_gene_regions <- function(n_regions) {
    region_indices <- .get_region_indices(n_regions)
    gene_regions <- numeric(max(region_indices))
    for (r in 1:n_regions) {
        if (r > 1) {
            start <- region_indices[r - 1] + 1
        } else { start <- 1 }
        end <- region_indices[r]
        gene_regions[start:end] <- r
    }
    return(gene_regions)
}

# Precomputes necessary data for rendering the circular plot.
.precompute_circular_plot_data <- function() {
    n_regions <- .settings$circular_plot_n_groups * .settings$circular_plot_n_regions
    .data$gff$gene_regions <- .compute_gene_regions(n_regions)

    # Precompute pos gene/region information.
    pos_genes <- .cpp_compute_outlier_genes(.data$gff, .data$outliers_direct)

    .data$outliers_direct$Pos_1_gene <- pos_genes$pos1_gene
    .data$outliers_direct$Pos_2_gene <- pos_genes$pos2_gene
    .data$outliers_direct$Pos_1_gene_name <- .data$gff$Name[pos_genes$pos1_gene]
    .data$outliers_direct$Pos_2_gene_name <- .data$gff$Name[pos_genes$pos2_gene]
    .data$outliers_direct$Pos_1_region <- .data$gff$gene_regions[pos_genes$pos1_gene]
    .data$outliers_direct$Pos_2_region <- .data$gff$gene_regions[pos_genes$pos2_gene]

    # Precompute main plot.
    circular_data <- .create_circular_data()
    top_level_dependencies <- .create_top_level_links(circular_data)
    edges <- .get_circular_vega_spec(circular_data, top_level_dependencies)

    # Add gene data.
    gene_data <- .create_gene_data()
    pos_data <- .create_pos_data()
    edges$data <- append(edges$data, .get_gene_data(gene_data, pos_data))
    edges$marks <- append(edges$marks, .get_gene_marks())

    .data$edges <- edges
}

.rescale_weights <- function(weights, a, b) {
    min_w <- min(weights)
    max_w <- max(weights)
    return((weights - min_w) * (b - a) / (max_w - min_w) + a)
}

.create_circular_data <- function() {
    n_genes <- nrow(.data$gff)
    n_groups <- .settings$circular_plot_n_groups
    n_regions_per_group <- .settings$circular_plot_n_regions
    n_regions <- n_groups * n_regions_per_group
    sz <- n_groups + n_groups * n_regions_per_group + 1

    group_names <- .get_region_boundaries(.data$gff$end[.get_region_indices(n_groups)])
    #region_boundaries <- get_region_boundaries(gff$end[get_region_indices(n_groups * n_regions_per_group)])

    circular_data <- data.frame(
        id = 1:sz,
        name = NA,
        size = NA,
        parent = NA,
        draw = NA,
        stringsAsFactors = FALSE
    )

    # Set parents for hidden levels
    for (i in (n_regions+1):(sz-1)) { circular_data$parent[i] <- sz }
    for (i in 1:n_regions) { circular_data$parent[i] <- floor((i - 1) / n_regions_per_group) + n_regions + 1 }

    # Set draw status for region slices.
    circular_data$draw[1:n_regions] <- TRUE
    circular_data$name[(seq(n_regions_per_group / 2, sz - n_groups - 1, n_regions_per_group))] <- group_names

    return(circular_data)
}

.create_top_level_links <- function(circular_data) {
    n_groups <- .settings$circular_plot_n_groups
    n_regions_per_group <- .settings$circular_plot_n_regions
    n_links <- nrow(.data$outliers_direct)
    dependencies <- data.frame(
        source = numeric(n_links),
        target = numeric(n_links),
        weight = numeric(n_links),
        count = numeric(n_links)
    )
    region_indices <- .get_region_indices(n_groups * n_regions_per_group)
    for (i in 1:n_links) {
        dependencies$source[i] <-  .data$outliers_direct$Pos_1_region[i]
        dependencies$target[i] <- .data$outliers_direct$Pos_2_region[i]
        dependencies$weight[i] <- .data$outliers_direct$MI[i]
    }
    for (i in 1:nrow(dependencies)) {
        dependencies$count[i] <- sum(dependencies[dependencies$source == dependencies$source[i], ]$target == dependencies$target[i])
    }
    dependencies$count <- 1 + log(log(dependencies$count + 2))
    dependencies <- dependencies[!duplicated(dependencies[, 1:2]), ]

    dependencies[, 3] <- .rescale_weights(dependencies[, 3], 0.75, 1)
    return(dependencies)
}

.create_gene_data <- function() {
    n_groups <- .settings$circular_plot_n_groups
    n_regions_per_group <- .settings$circular_plot_n_regions
    gene_data <- data.frame(
        id = numeric(0),
        name = numeric(0),
        region = numeric(0),
        angle_step = numeric(0),
        step_size = numeric(0),
        stringsAsFactors = FALSE
    )
    n_regions <- n_groups * n_regions_per_group
    for (region in 1:n_regions) {
        region_genes <- which(.data$gff$gene_regions == region)
        gene_data <- rbind(gene_data, data.frame(
            id = region_genes,
            name = .data$gff$Name[region_genes],
            region = region,
            angle_step = seq(1 / length(region_genes), 1, 1 / length(region_genes)),
            step_size = 1 / length(region_genes),
            stringsAsFactors = FALSE
        ))
    }
    return(gene_data)
}

.create_pos_data <- function() {
    n_groups <- .settings$circular_plot_n_groups
    n_regions_per_group <- .settings$circular_plot_n_regions
    pos_data <- data.frame(
        name = numeric(0),
        parent = numeric(0),
        idx = numeric(0),
        region = numeric(0),
        weight = numeric(0),
        stringsAsFactors = FALSE)
    n_regions <- n_groups * n_regions_per_group
    for (region in 1:n_regions) {
        region_pos <- which(.data$outliers_direct$Pos_1_region == region)
        if (!rlang::is_empty(region_pos)) {
            pos_data <- rbind(pos_data, data.frame(
                name = .data$outliers_direct$Pos_1[region_pos],
                parent = .data$outliers_direct$Pos_1_gene[region_pos],
                idx = region_pos,
                region = region,
                weight = .data$outliers_direct$MI[region_pos],
                stringsAsFactors = FALSE
            ))
        }

        region_pos <- which(.data$outliers_direct$Pos_2_region == region)
        if (!rlang::is_empty(region_pos)) {
            pos_data <- rbind(pos_data, data.frame(
                name = .data$outliers_direct$Pos_2[region_pos],
                parent = .data$outliers_direct$Pos_2_gene[region_pos],
                idx = region_pos,
                region = region,
                weight = .data$outliers_direct$MI[region_pos],
                stringsAsFactors = FALSE
            ))
        }
    }

    pos_data <- pos_data[order(pos_data$weight, decreasing = TRUE), ]
    pos_data <- pos_data[!duplicated(pos_data$name), ]
    pos_data$weight <- .rescale_weights(pos_data$weight, 0.5, 1)
    pos_data <- pos_data[order(pos_data$region), ]

    pos_in_gene <- numeric(nrow(pos_data))
    for (i in 1:nrow(pos_data)) {
        gene_id <- pos_data$parent[i]
        start <- .data$gff$start[gene_id]
        end <- .data$gff$end[gene_id]
        gene_length <- end - start
        pos_in_gene[i] <- max(0.1, min(0.9, (pos_data$name[i] - start) / gene_length))
    }
    pos_data <- cbind(pos_data, pos_in_gene)

    return(pos_data)
}

# Not used yet.
.create_pos_links <- function(pos_data) {
    pos_links <- data.frame(
        source = numeric(0),
        #source_gene_name = numeric(0),
        #source_region = numeric(0),
        #source_gene = numeric(0),
        target = numeric(0),
        target_gene_name = character(0),
        target_region = numeric(0),
        stringsAsFactors = FALSE
    )
    for (i in 1:nrow(pos_data)) {
        pos <- pos_data$name[i]
        matches_pos_1 <- which(.data$outliers_direct$Pos_1 == pos)
        matches_pos_2 <- which(.data$outliers_direct$Pos_2 == pos)
        if (!rlang::is_empty(matches_pos_1)) {
            pos_links <- rbind(pos_links, data.frame(
                source = pos,
                #source_gene_name = gff$Name[pos_data$parent[i]],
                #source_region = pos_data$region[i],
                #source_gene = pos_data$parent[i],
                target = .data$outliers_direct$Pos_2[matches_pos_1],
                target_gene_name = .data$outliers_direct$Pos_2_gene[matches_pos_1],
                target_region = .data$outliers_direct$Pos_2_region[matches_pos_1],
                stringsAsFactors = FALSE
            ))
        }
        if (!rlang::is_empty(matches_pos_2)) {
            pos_links <- rbind(pos_links, data.frame(
                source = pos,
                #source_gene_name = gff$Name[pos_data$parent[i]],
                #source_region = pos_data$region[i],
                #source_gene = pos_data$parent[i],
                target = .data$outliers_direct$Pos_1[matches_pos_2],
                target_gene_name = .data$outliers_direct$Pos_1_gene[matches_pos_2],
                target_region = .data$outliers_direct$Pos_1_region[matches_pos_2]
            ))
        }
    }
    target_gene <- numeric(nrow(pos_links))
    for (i in 1:nrow(pos_links)) {
        target_gene[i] <- pos_data$parent[which(pos_data$name == pos_links$target[i])]
    }
    pos_links <- cbind(pos_links, target_gene)
}

# get_idx <- function(gene_data, pos) {
#     gene_data[which(gene_data[, 2] == pos), 1]
# }
#
# create_region_links <- function(gene_data, map_min = 0.5, map_max = 0.9) {
#     n_links <- nrow(outliers_direct)
#     positional_data <- filter(gene_data, gene_data$parent > 0)
#     if (nrow(positional_data) > 0) {
#         region_dependencies <- data.frame(
#             source = positional_data$id,
#             target = positional_data$parent,
#             weight = outliers_direct$MI[positional_data$idx],
#             count = 1
#         )
#
#         # Remap to (0.1, 0.5)
#         min_mi <- min(region_dependencies[, 3])
#         max_mi <- max(region_dependencies[, 3])
#         region_dependencies[, 3] <- (region_dependencies[, 3] - min_mi) * (map_max - map_min) / (max_mi - min_mi) + map_min
#     } else {
#         region_dependencies <- data.frame(source = numeric(0), target = numeric(0), weight = numeric(0), count = numeric(0))
#     }
#     return(region_dependencies)
# }

