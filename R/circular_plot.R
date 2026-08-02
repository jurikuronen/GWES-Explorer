.render_circular_plot <- function(data) {
    vegawidget::renderVegawidget({
        if (is.null(data$edges)) {
            return(NULL)
        }
        data$edges
    })
}

.set_circular_plot_signals <- function(data, selected_row) {
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_region_1",
                                    data$outliers_direct$Pos_1_region[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_gene_1",
                                    data$outliers_direct$Pos_1_gene[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_1",
                                    data$outliers_direct$Pos_1[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_region_2",
                                    data$outliers_direct$Pos_2_region[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_gene_2",
                                    data$outliers_direct$Pos_2_gene[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_2",
                                    data$outliers_direct$Pos_2[selected_row])
}

# Calculate region boundaries. Returns a string vector with "start-end" strings.
.get_region_boundaries <- function(region_indices) {
    paste0(c(1, head(region_indices, -1) + 1), "-", region_indices)
}

.get_region_indices <- function(data, n_regions) {
    n_genes <- nrow(data$gff)
    as.integer(c(ceiling(seq_len(n_regions - 1L) * (n_genes / n_regions)), n_genes))
}

.compute_gene_regions <- function(data, n_regions) {
    region_indices <- .get_region_indices(data, n_regions)
    gene_regions <- integer(max(region_indices))

    for (region in seq_len(n_regions)) {
        start <- if (region > 1L) region_indices[region - 1L] + 1L else 1L
        end <- region_indices[region]
        gene_regions[start:end] <- region
    }

    return(gene_regions)
}

.create_circular_data <- function(data) {
    n_groups <- .circular_plot_groups()
    n_regions_per_group <- .circular_plot_regions_per_group()
    n_regions <- n_groups * n_regions_per_group
    sz <- n_groups + n_groups * n_regions_per_group + 1L

    group_names <- .get_region_boundaries(data$gff$end[.get_region_indices(data, n_groups)])

    # Initialize circular data.
    circular_data <- data.frame(
        id = seq_len(sz),
        name = rep(NA_character_, sz),
        size = rep(NA_real_, sz),
        parent = rep(NA_integer_, sz),
        draw = rep(NA, sz),
        stringsAsFactors = FALSE
    )

    # Set parents for hidden levels
    circular_data$parent[seq.int(n_regions + 1L, sz - 1L)] <- sz
    circular_data$parent[seq_len(n_regions)] <- (seq_len(n_regions) - 1L) %/% n_regions_per_group + n_regions + 1L

    # Set draw status for region slices.
    circular_data$draw[seq_len(n_regions)] <- TRUE
    group_label_indices <- seq.int(n_regions_per_group %/% 2L, sz - n_groups - 1L, n_regions_per_group)
    circular_data$name[group_label_indices] <- group_names

    return(circular_data)
}

# Precomputes necessary data for rendering the circular plot.
.precompute_circular_plot_data <- function(data) {
    data$gff$gene_regions <- .compute_gene_regions(data, .circular_plot_regions())

    # Find the gene or calculated IGR containing each outlier position.
    outlier_gene_or_igr_indices <- .cpp_find_outlier_gene_or_igr_indices(data$gff$start,
                                                                         data$gff$end,
                                                                         data$outliers_direct$Pos_1,
                                                                         data$outliers_direct$Pos_2)

    pos_1_gene_or_igr_indices <- outlier_gene_or_igr_indices$pos1_gene_or_igr_index
    pos_2_gene_or_igr_indices <- outlier_gene_or_igr_indices$pos2_gene_or_igr_index

    data$outliers_direct$Pos_1_gene <- pos_1_gene_or_igr_indices
    data$outliers_direct$Pos_2_gene <- pos_2_gene_or_igr_indices
    data$outliers_direct$Pos_1_gene_name <- data$gff$Name[pos_1_gene_or_igr_indices]
    data$outliers_direct$Pos_2_gene_name <- data$gff$Name[pos_2_gene_or_igr_indices]
    data$outliers_direct$Pos_1_region <- data$gff$gene_regions[pos_1_gene_or_igr_indices]
    data$outliers_direct$Pos_2_region <- data$gff$gene_regions[pos_2_gene_or_igr_indices]

    # Precompute main plot.
    circular_data <- .create_circular_data(data)
    top_level_dependencies <- .create_top_level_links(data)
    edges <- .circular_plot_vega_spec(circular_data, top_level_dependencies)

    # Add gene data.
    gene_data <- .create_gene_data(data)
    position_data <- .create_position_data(data)
    position_links <- .cpp_create_bidirectional_position_links(data$outliers_direct, position_data)
    position_links$weight <- .rescale_weights(position_links$MI, 0.5, 1)
    gene_data <- .add_link_info_to_gene_data(data, gene_data, position_links)
    edges$data <- append(edges$data, .circular_plot_vega_gene_data(gene_data))
    edges$data <- append(edges$data, .circular_plot_vega_pos_data_and_links(position_data, position_links))
    edges$marks <- append(edges$marks, .circular_plot_vega_gene_marks())
    edges$marks <- append(edges$marks, .circular_plot_vega_pos_marks())

    data$edges <- edges
}

.rescale_weights <- function(weights, a, b) {
    min_w <- min(weights)
    max_w <- max(weights)

    # Avoid division by zero when all weights are equal.
    if (min_w == max_w) {
        return(rep((a + b) / 2, length(weights)))
    }

    return((weights - min_w) * (b - a) / (max_w - min_w) + a)
}

.create_top_level_links <- function(data) {
    dependencies <- transform(data$outliers_direct,
                              source = Pos_1_region,
                              target = Pos_2_region,
                              weight = MI
    )[c("source", "target", "weight")]
    dependencies$count <- stats::ave(dependencies$target,
                                     dependencies$source,
                                     dependencies$target,
                                     FUN = length)
    dependencies$count <- 1 + log(log(dependencies$count + 2))
    dependencies <- dependencies[!duplicated(dependencies[c("source", "target")]), ]
    dependencies[, 3] <- .rescale_weights(dependencies$weight, 0.75, 1)
    return(dependencies)
}

.create_gene_data <- function(data) {
    gene_data <- do.call(rbind, lapply(seq_len(.circular_plot_regions()), function(region) {
        region_genes <- which(data$gff$gene_regions == region)
        data.frame(
            id = region_genes,
            name = data$gff$Name[region_genes],
            region = region,
            angle_step = seq(0, 1, length.out = length(region_genes)),
            step_size = 1 / length(region_genes),
            start = data$gff$start[region_genes],
            end = data$gff$end[region_genes],
            stringsAsFactors = FALSE
        )
    }))
    return(gene_data)
}

.create_position_data <- function(data) {
    position_data <- do.call(rbind, lapply(seq_len(.circular_plot_regions()), function(region) {
        create_position_data_for_endpoint <- function(position_column) {
            outlier_indices <- which(data$outliers_direct[[paste0(position_column, "_region")]] == region)
            if (length(outlier_indices) == 0) {
                return(NULL)
            }
            data.frame(
                name = data$outliers_direct[[position_column]][outlier_indices],
                parent = data$outliers_direct[[paste0(position_column, "_gene")]][outlier_indices],
                idx = outlier_indices,
                region = region,
                weight = data$outliers_direct$MI[outlier_indices],
                stringsAsFactors = FALSE
            )
        }
        rbind(create_position_data_for_endpoint("Pos_1"), create_position_data_for_endpoint("Pos_2"))
    }))

    position_data <- position_data[order(-position_data$weight), ]
    position_data <- position_data[!duplicated(position_data$name), ]
    position_data$weight <- .rescale_weights(position_data$weight, 0.5, 1)
    position_data <- position_data[order(position_data$region), ]

    gene_start  <- data$gff$start[position_data$parent]
    gene_end    <- data$gff$end[position_data$parent]
    gene_length <- gene_end - gene_start
    position_data$pos_in_gene <- pmin(0.9, pmax(0.1, (position_data$name - gene_start) / gene_length))

    return(position_data)
}

.add_link_info_to_gene_data <- function(data, gene_data, position_links) {
    sorted_gene_links <- .cpp_sort_gene_links_for_tooltips(position_links)
    n_genes <- nrow(gene_data)
    genes_linked_to <- vector("list", n_genes)
    n_genes_linked_to <- integer(n_genes)
    n_outliers <- integer(n_genes)
    tooltip_lengths <- integer(n_genes)

    for (link_index in seq_len(nrow(sorted_gene_links))) {
        source_gene_index <- sorted_gene_links$gene_1[link_index]
        target_gene_index <- sorted_gene_links$gene_2[link_index]
        mutual_information <- sorted_gene_links$MI[link_index]
        first_link_for_source_gene <- is.null(genes_linked_to[[source_gene_index]])

        if (first_link_for_source_gene) {
            genes_linked_to[[source_gene_index]] <- "Linked to the following genes:"
        }

        # Add new linked gene's info.
        if (first_link_for_source_gene ||
            (sorted_gene_links$gene_1[link_index - 1L] == source_gene_index &&
             sorted_gene_links$gene_2[link_index - 1L] != target_gene_index))
        {
            n_genes_linked_to[source_gene_index] <- n_genes_linked_to[source_gene_index] + 1L
            linked_gene_info <- sprintf("%s (%s-%s)",
                                        data$gff$Name[target_gene_index],
                                        data$gff$start[target_gene_index],
                                        data$gff$end[target_gene_index])
            genes_linked_to[[source_gene_index]] <- append(genes_linked_to[[source_gene_index]], linked_gene_info)
        }

        genes_linked_to[[source_gene_index]] <- append(genes_linked_to[[source_gene_index]],
                                                        mutual_information)
        n_outliers[source_gene_index] <- n_outliers[source_gene_index] + 1L
    }

    for (gene_index in seq_len(nrow(gene_data))) {
        tooltip_lengths[gene_index] <- length(genes_linked_to[[gene_index]])
    }

    gene_data$genes_linked_to <- genes_linked_to
    gene_data$n_genes_linked_to <- n_genes_linked_to
    gene_data$n_outliers <- n_outliers
    gene_data$length <- tooltip_lengths

    return(gene_data)
}
