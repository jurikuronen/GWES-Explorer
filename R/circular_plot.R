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
                                    "selected_feature_1",
                                    data$outliers_direct$Pos_1_feature_row[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_1",
                                    data$outliers_direct$Pos_1[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_region_2",
                                    data$outliers_direct$Pos_2_region[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_feature_2",
                                    data$outliers_direct$Pos_2_feature_row[selected_row])
    vegawidget::vw_shiny_set_signal("circular_plot",
                                    "selected_position_2",
                                    data$outliers_direct$Pos_2[selected_row])
}

# Calculate region boundaries. Returns a string vector with "start-end" strings.
.get_region_boundaries <- function(region_end_positions) {
    paste0(c(1, head(region_end_positions, -1) + 1), "-", region_end_positions)
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

# Precomputes necessary data for rendering the circular plot.
.precompute_circular_plot_data <- function(data) {
    # Assign each GFF row to one of the circular plot's regions.
    data$gff$feature_regions <- .compute_feature_regions(data, .circular_plot_regions())

    # Find the feature containing each outlier position.
    outlier_feature_rows <- .cpp_find_outlier_feature_rows(data$gff$start,
                                                           data$gff$end,
                                                           data$outliers_direct$Pos_1,
                                                           data$outliers_direct$Pos_2)

    position_1_feature_rows <- outlier_feature_rows$position_1_feature_row
    position_2_feature_rows <- outlier_feature_rows$position_2_feature_row

    data$outliers_direct$Pos_1_feature_row <- position_1_feature_rows
    data$outliers_direct$Pos_2_feature_row <- position_2_feature_rows
    data$outliers_direct$Pos_1_feature <- data$gff$Name[position_1_feature_rows]
    data$outliers_direct$Pos_2_feature <- data$gff$Name[position_2_feature_rows]
    data$outliers_direct$Pos_1_region <- data$gff$feature_regions[position_1_feature_rows]
    data$outliers_direct$Pos_2_region <- data$gff$feature_regions[position_2_feature_rows]

    # Build the outer region slices and links.
    circular_data <- .create_circular_data(data)
    top_level_dependencies <- .create_top_level_links(data)
    edges <- .circular_plot_vega_spec(circular_data, top_level_dependencies)

    # Add the feature and position data used by the two inner views.
    feature_data <- .create_feature_data(data)
    position_data <- .create_position_data(data)
    position_links <- .cpp_create_bidirectional_position_links(data$outliers_direct, position_data)
    position_links$weight <- .rescale_weights(position_links$MI, 0.5, 1)
    feature_data <- .add_link_info_to_feature_data(data, feature_data, position_links)
    edges$data <- append(edges$data, .circular_plot_vega_feature_data(feature_data))
    edges$data <- append(edges$data,
                         .circular_plot_vega_position_data_and_links(position_data, position_links))
    edges$marks <- append(edges$marks, .circular_plot_vega_feature_marks())
    edges$marks <- append(edges$marks, .circular_plot_vega_position_marks())

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

.create_feature_data <- function(data) {
    feature_data <- do.call(rbind, lapply(seq_len(.circular_plot_regions()), function(region) {
        region_features <- which(data$gff$feature_regions == region)
        data.frame(
            feature_row = region_features,
            feature = data$gff$Name[region_features],
            region = region,
            angle_step = seq(0, 1, length.out = length(region_features)),
            step_size = 1 / length(region_features),
            start = data$gff$start[region_features],
            end = data$gff$end[region_features],
            stringsAsFactors = FALSE
        )
    }))
    return(feature_data)
}

.create_position_data <- function(data) {
    position_data <- do.call(rbind, lapply(seq_len(.circular_plot_regions()), function(region) {
        create_position_data_for_endpoint <- function(position_column) {
            outlier_rows <- which(data$outliers_direct[[paste0(position_column, "_region")]] == region)
            if (length(outlier_rows) == 0) {
                return(NULL)
            }
            data.frame(
                position = data$outliers_direct[[position_column]][outlier_rows],
                feature_row = data$outliers_direct[[paste0(position_column, "_feature_row")]][outlier_rows],
                region = region,
                weight = data$outliers_direct$MI[outlier_rows],
                stringsAsFactors = FALSE
            )
        }
        rbind(create_position_data_for_endpoint("Pos_1"), create_position_data_for_endpoint("Pos_2"))
    }))

    position_data <- position_data[order(-position_data$weight), ]
    position_data <- position_data[!duplicated(position_data$position), ]
    position_data$weight <- .rescale_weights(position_data$weight, 0.5, 1)
    position_data <- position_data[order(position_data$region), ]

    feature_start <- data$gff$start[position_data$feature_row]
    feature_end <- data$gff$end[position_data$feature_row]
    feature_length <- feature_end - feature_start
    position_data$position_in_feature <- pmin(0.9,
                                              pmax(0.1,
                                                   (position_data$position - feature_start) / feature_length))

    return(position_data)
}

.add_link_info_to_feature_data <- function(data, feature_data, position_links) {
    sorted_feature_links <- .cpp_sort_feature_links_for_tooltips(position_links)
    n_features <- nrow(feature_data)
    features_linked_to <- vector("list", n_features)
    n_features_linked_to <- integer(n_features)
    n_outliers <- integer(n_features)
    n_self_links <- integer(n_features)
    tooltip_lengths <- integer(n_features)

    for (link_row in seq_len(nrow(sorted_feature_links))) {
        source_feature_row <- sorted_feature_links$feature_row_1[link_row]
        target_feature_row <- sorted_feature_links$feature_row_2[link_row]

        # Self-links are not listed as links to another feature.
        if (source_feature_row == target_feature_row) {
            n_self_links[source_feature_row] <- n_self_links[source_feature_row] + 1L
            next
        }

        mutual_information <- sorted_feature_links$MI[link_row]
        first_link_for_source_feature <- is.null(features_linked_to[[source_feature_row]])

        if (first_link_for_source_feature) {
            features_linked_to[[source_feature_row]] <- "Linked to:"
        }

        # Add the linked feature's name and location before its first MI value.
        if (first_link_for_source_feature ||
            (sorted_feature_links$feature_row_1[link_row - 1L] == source_feature_row &&
             sorted_feature_links$feature_row_2[link_row - 1L] != target_feature_row))
        {
            n_features_linked_to[source_feature_row] <- n_features_linked_to[source_feature_row] + 1L
            linked_feature_info <- sprintf("%s (%s-%s)",
                                           data$gff$Name[target_feature_row],
                                           data$gff$start[target_feature_row],
                                           data$gff$end[target_feature_row])
            features_linked_to[[source_feature_row]] <- append(features_linked_to[[source_feature_row]],
                                                               linked_feature_info)
        }

        features_linked_to[[source_feature_row]] <- append(features_linked_to[[source_feature_row]],
                                                           mutual_information)
        n_outliers[source_feature_row] <- n_outliers[source_feature_row] + 1L
    }

    # Self-links have been counted twice.
    n_self_links <- n_self_links %/% 2L

    n_outliers <- n_outliers + n_self_links

    for (feature_row in seq_len(nrow(feature_data))) {
        tooltip_lengths[feature_row] <- length(features_linked_to[[feature_row]])
    }

    feature_data$features_linked_to <- features_linked_to
    feature_data$n_features_linked_to <- n_features_linked_to
    feature_data$n_outliers <- n_outliers
    feature_data$n_self_links <- n_self_links
    feature_data$length <- tooltip_lengths

    return(feature_data)
}
