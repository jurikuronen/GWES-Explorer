# Creates genomic coordinate-range labels for region groups.
.create_region_group_coordinate_labels <- function(group_end_positions) {
    group_start_positions <- c(1, utils::head(group_end_positions, -1) + 1)
    paste0(group_start_positions, "-", group_end_positions)
}

# Calculates the last 1-based feature row in each region.
.calculate_region_end_feature_rows <- function(feature_count, region_count) {
    as.integer(c(
        ceiling(seq_len(region_count - 1L) * (feature_count / region_count)),
        feature_count
    ))
}

# Calculates the region ID for each feature row.
.calculate_feature_region_ids <- function(feature_count, region_count) {
    region_end_feature_rows <- .calculate_region_end_feature_rows(feature_count, region_count)
    feature_region_ids <- integer(feature_count)

    for (region_id in seq_len(region_count)) {
        start_row <- if (region_id > 1L) region_end_feature_rows[region_id - 1L] + 1L else 1L
        end_row <- region_end_feature_rows[region_id]
        feature_region_ids[start_row:end_row] <- region_id
    }

    return(feature_region_ids)
}

# Creates circular plot nodes for hierarchical edge bundling:
#
# - Each region becomes a node.
# - Each group of regions becomes a hidden node inward towards the circle center.
# - The circle center becomes the hidden root node.
#
# Region group nodes and the root guide the bundled links.
#
# Based on Vega's edge-bundling example (https://vega.github.io/vega/examples/edge-bundling/), which implements the
# method by Holten (2006), https://doi.org/10.1109/TVCG.2006.147.
.create_region_edge_bundling_hierarchy <- function(feature_end_positions) {
    region_group_count <- .settings$circular_plot_region_group_count
    regions_per_group_count <- .settings$circular_plot_regions_per_group_count
    region_count <- .settings$circular_plot_region_count
    node_count <- region_count + region_group_count + 1L
    # Put region rows first so their row numbers match their region IDs. Then region groups and finally the root.
    root_id <- node_count
    group_ids <- seq.int(region_count + 1L, root_id - 1L)

    group_labels <- .create_region_group_coordinate_labels(
        feature_end_positions[.calculate_region_end_feature_rows(length(feature_end_positions), region_group_count)]
    )

    # Initialize the hierarchy data frame.
    region_hierarchy <- data.frame(
        id = seq_len(node_count),
        group_coordinate_label = rep(NA_character_, node_count),
        parent_id = rep(NA_integer_, node_count),
        is_region = rep(NA, node_count),
        stringsAsFactors = FALSE
    )

    # Region groups are children of the root.
    region_hierarchy$parent_id[group_ids] <- root_id

    # Set the parent of the regions in each region group.
    region_hierarchy$parent_id[seq_len(region_count)] <-
        (seq_len(region_count) - 1L) %/% regions_per_group_count + region_count + 1L

    # Mark region nodes for drawing.
    region_hierarchy$is_region[seq_len(region_count)] <- TRUE

    # Pick a region near the middle of each group as an anchor for the group's coordinate label.
    group_label_rows <- seq.int(regions_per_group_count %/% 2L,
                                region_count,
                                regions_per_group_count)
    region_hierarchy$group_coordinate_label[group_label_rows] <- group_labels

    return(region_hierarchy)
}

# Creates region links from the direct outliers.
# - Region link width scales by the number of outliers within a link.
# - Region link color and opacity scale by the highest MI outlier within a link.
.create_region_links <- function(direct_outliers) {
    region_links <- data.frame(
        source = direct_outliers$Pos_1_region,
        target = direct_outliers$Pos_2_region,
        weight = direct_outliers$MI
    )

    # Count the number of outliers within each region link.
    outlier_counts <- stats::ave(region_links$target,
                                 region_links$source,
                                 region_links$target,
                                 FUN = length)

    # Scale line width by the number of outliers.
    region_links$stroke_width <- 1 + log(log(outlier_counts + 2))

    # Remove duplicates; outliers are sorted so the highest MI per link is kept.
    region_links <- region_links[!duplicated(region_links[c("source", "target")]), ]

    # Re-scale MI to use with color and opacity signals.
    region_links$weight <- .rescale_weights(region_links$weight, 0.75, 1)

    return(region_links)
}
