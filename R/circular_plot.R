.render_circular_plot <- function() {
    vegawidget::renderVegawidget({
        if (is.null(.data$edges)) {
            return(NULL)
        }
        .data$edges
    })
}

.set_circular_plot_signals <- function(selected_row) {
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_region_1",
                                    .data$outliers_direct$Pos_1_region[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_gene_1",
                                    .data$outliers_direct$Pos_1_gene[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_1",
                                    .data$outliers_direct$Pos_1[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_region_2",
                                    .data$outliers_direct$Pos_2_region[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_gene_2",
                                    .data$outliers_direct$Pos_2_gene[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_2",
                                    .data$outliers_direct$Pos_2[selected_row])
}

# Calculate region boundaries. Returns a string vector with "start-end" strings.
.get_region_boundaries <- function(region_indices) {
    paste0(c(1, head(region_indices, -1) + 1), "-", region_indices)
}

.get_region_indices <- function(n_regions) {
    n_genes <- nrow(.data$gff)
    c(ceiling(1:(n_regions-1) * (n_genes / n_regions)), n_genes)
}

.compute_gene_regions <- function(n_regions) {
    region_indices <- .get_region_indices(n_regions)
    gene_regions <- numeric(max(region_indices))
    for (r in 1:n_regions) {
        start <- ifelse(r > 1, region_indices[r - 1] + 1, 1)
        end <- region_indices[r]
        gene_regions[start:end] <- r
    }
    return(gene_regions)
}

.create_circular_data <- function() {
    n_groups <- .circular_plot_groups()
    n_regions_per_group <- .circular_plot_regions_per_group()
    n_regions <- n_groups * n_regions_per_group
    sz <- n_groups + n_groups * n_regions_per_group + 1

    group_names <- .get_region_boundaries(.data$gff$end[.get_region_indices(n_groups)])

    # Initialize circular data.
    circular_data <- data.frame(
        id = 1:sz,
        name = NA,
        size = NA,
        parent = NA,
        draw = NA,
        stringsAsFactors = FALSE
    )

    # Set parents for hidden levels
    circular_data$parent[(n_regions+1):(sz-1)] <- sz
    circular_data$parent[1:n_regions] <- floor((0:(n_regions-1)) / n_regions_per_group) + n_regions + 1

    # Set draw status for region slices.
    circular_data$draw[1:n_regions] <- TRUE
    circular_data$name[(seq(n_regions_per_group / 2, sz - n_groups - 1, n_regions_per_group))] <- group_names

    return(circular_data)
}

# Precomputes necessary data for rendering the circular plot.
.precompute_circular_plot_data <- function() {
    .data$gff$gene_regions <- .compute_gene_regions(.circular_plot_regions())

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
    edges <- .circular_plot_vega_spec(circular_data, top_level_dependencies)

    # Add gene data.
    gene_data <- .create_gene_data()
    pos_data <- .create_pos_data()
    pos_links <- .cpp_create_pos_links(.data$outliers_direct, pos_data)
    pos_links$weight <- .rescale_weights(pos_links$MI, 0.5, 1)
    gene_data <- .add_link_info_to_gene_data(gene_data, pos_links)
    edges$data <- append(edges$data, .circular_plot_vega_gene_data(gene_data))
    edges$data <- append(edges$data, .circular_plot_vega_pos_data_and_links(pos_data, pos_links))
    edges$marks <- append(edges$marks, .circular_plot_vega_gene_marks())
    edges$marks <- append(edges$marks, .circular_plot_vega_pos_marks())

    .data$edges <- edges
}

.rescale_weights <- function(weights, a, b) {
    min_w <- min(weights)
    max_w <- max(weights)
    return((weights - min_w) * (b - a) / (max_w - min_w) + a)
}

.create_top_level_links <- function(circular_data) {
    dependencies <- transform(.data$outliers_direct,
                              source = Pos_1_region,
                              target = Pos_2_region,
                              weight = MI
    )[c("source", "target", "weight")]
    dependencies$count <- ave(dependencies$target,
                              dependencies$source,
                              dependencies$target,
                              FUN = length)
    dependencies$count <- 1 + log(log(dependencies$count + 2))
    dependencies <- dependencies[!duplicated(dependencies[c("source", "target")]), ]
    dependencies[, 3] <- .rescale_weights(dependencies$weight, 0.75, 1)
    return(dependencies)
}

.create_gene_data <- function() {
    gene_data <- do.call(rbind, lapply(seq_len(.circular_plot_regions()), function(region) {
        region_genes <- which(.data$gff$gene_regions == region)
        data.frame(
            id = region_genes,
            name = .data$gff$Name[region_genes],
            region = region,
            angle_step = seq(0, 1, length.out = length(region_genes)),
            step_size = 1 / length(region_genes),
            start = .data$gff$start[region_genes],
            end = .data$gff$end[region_genes],
            stringsAsFactors = FALSE
        )
    }))
    return(gene_data)
}

.create_pos_data <- function() {
    pos_data <- do.call(rbind, lapply(seq_len(.circular_plot_regions()), function(region) {
        add_position_data <- function(key) {
            region_pos <- which(.data$outliers_direct[[paste0(key, "_region")]] == region)
            if (length(region_pos) == 0) {
                return(NULL)
            }
            data.frame(
                name = .data$outliers_direct[[key]][region_pos],
                parent = .data$outliers_direct[[paste0(key, "_gene")]][region_pos],
                idx = region_pos,
                region = region,
                weight = .data$outliers_direct$MI[region_pos],
                stringsAsFactors = FALSE
            )
        }
        rbind(add_position_data("Pos_1"), add_position_data("Pos_2"))
    }))

    pos_data <- pos_data[order(-pos_data$weight), ]
    pos_data <- pos_data[!duplicated(pos_data$name), ]
    pos_data$weight <- .rescale_weights(pos_data$weight, 0.5, 1)
    pos_data <- pos_data[order(pos_data$region), ]

    gene_start  <- .data$gff$start[pos_data$parent]
    gene_end    <- .data$gff$end[pos_data$parent]
    gene_length <- gene_end - gene_start
    pos_in_gene <- pmin(0.9, pmax(0.1, (pos_data$name - gene_start) / gene_length))
    pos_data <- cbind(pos_data, pos_in_gene)

    return(pos_data)
}

.add_link_info_to_gene_data <- function(gene_data, pos_links) {
    x <- .cpp_sorted_pos_links(pos_links)
    n <- nrow(gene_data)
    genes_linked_to <- sapply(character(n), function(x) NULL)
    n_genes_linked_to <- numeric(n)
    n_outliers <- numeric(n)
    length <- numeric(n)
    for (i in 1:nrow(x)) {
        gene1 <- x$gene_1[i]
        gene2 <- x$gene_2[i]
        mi <- x$MI[i]
        start_new_gene <- is.null(genes_linked_to[gene1][[1]])
        # Initialize.
        if (start_new_gene) {
            genes_linked_to[gene1][[1]] <- "Linked to the following genes:"
        }
        # Add new linked gene's info.
        if (start_new_gene || (x$gene_1[i - 1] == gene1 && x$gene_2[i - 1] != gene2)) {
            n_genes_linked_to[gene1] <- n_genes_linked_to[gene1] + 1
            gene_info <- sprintf("%s (%s-%s)", .data$gff$Name[gene2], .data$gff$start[gene2], .data$gff$end[gene2])
            genes_linked_to[gene1][[1]] <- append(genes_linked_to[gene1][[1]], gene_info)
        }
        genes_linked_to[gene1][[1]] <- append(genes_linked_to[gene1][[1]], mi)
        n_outliers[gene1] <- n_outliers[gene1] + 1
    }
    for (i in 1:nrow(gene_data)) length[i] <- length(genes_linked_to[i][[1]])
    gene_data$genes_linked_to <- genes_linked_to
    gene_data$n_genes_linked_to <- n_genes_linked_to
    gene_data$n_outliers <- n_outliers
    gene_data$length <- length
    return(gene_data)
}
