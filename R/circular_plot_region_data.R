# Calculate region boundaries. Returns a string vector with "start-end" strings.
.get_region_boundaries <- function(region_end_positions) {
    paste0(c(1, utils::head(region_end_positions, -1) + 1), "-", region_end_positions)
}

.get_region_end_rows <- function(data, n_regions) {
    n_features <- nrow(data$gff)
    as.integer(c(ceiling(seq_len(n_regions - 1L) * (n_features / n_regions)), n_features))
}

.compute_feature_regions <- function(data, n_regions) {
    region_end_rows <- .get_region_end_rows(data, n_regions)
    feature_regions <- integer(max(region_end_rows))

    for (region in seq_len(n_regions)) {
        start_row <- if (region > 1L) region_end_rows[region - 1L] + 1L else 1L
        end_row <- region_end_rows[region]
        feature_regions[start_row:end_row] <- region
    }

    return(feature_regions)
}

.create_circular_data <- function(data) {
    n_groups <- .circular_plot_groups()
    n_regions_per_group <- .circular_plot_regions_per_group()
    n_regions <- n_groups * n_regions_per_group
    sz <- n_groups + n_groups * n_regions_per_group + 1L

    group_names <- .get_region_boundaries(data$gff$end[.get_region_end_rows(data, n_groups)])

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
    group_label_rows <- seq.int(n_regions_per_group %/% 2L, sz - n_groups - 1L, n_regions_per_group)
    circular_data$name[group_label_rows] <- group_names

    return(circular_data)
}

.create_top_level_links <- function(data) {
    dependencies <- data.frame(
        source = data$outliers_direct$Pos_1_region,
        target = data$outliers_direct$Pos_2_region,
        weight = data$outliers_direct$MI
    )
    dependencies$count <- stats::ave(dependencies$target,
                                     dependencies$source,
                                     dependencies$target,
                                     FUN = length)
    dependencies$count <- 1 + log(log(dependencies$count + 2))
    dependencies <- dependencies[!duplicated(dependencies[c("source", "target")]), ]
    dependencies[, 3] <- .rescale_weights(dependencies$weight, 0.75, 1)
    return(dependencies)
}
