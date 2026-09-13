# Creates feature data that contains position information for the feature views.
.create_feature_data <- function(gff) {
    feature_data <- do.call(rbind, lapply(seq_len(.settings$circular_plot_region_count), function(region_id) {
        feature_rows <- which(gff$feature_region_ids == region_id)
        feature_count <- length(feature_rows)
        data.frame(
            feature_row = feature_rows,
            feature = gff$Name[feature_rows],
            region = region_id,
            position_fraction = seq(0, 1, length.out = feature_count),
            position_step_size = 1 / max(1, feature_count - 1),
            start = gff$start[feature_rows],
            end = gff$end[feature_rows],
            stringsAsFactors = FALSE
        )
    }))
    return(feature_data)
}

# Adds information to feature_data about linked features and outliers for the tooltips.
.add_link_info_to_feature_data <- function(feature_data, position_links) {
    sorted_feature_links <- .cpp_sort_feature_links_for_tooltips(position_links)
    feature_count <- nrow(feature_data)
    features_linked_to <- vector("list", feature_count)
    linked_feature_count <- integer(feature_count)
    outlier_count <- integer(feature_count)
    self_link_count <- integer(feature_count)

    for (link_row in seq_len(nrow(sorted_feature_links))) {
        source_feature_row <- sorted_feature_links$feature_row_1[link_row]
        target_feature_row <- sorted_feature_links$feature_row_2[link_row]

        # Self-links are not listed as links to another feature.
        if (source_feature_row == target_feature_row) {
            self_link_count[source_feature_row] <- self_link_count[source_feature_row] + 1L
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
            linked_feature_count[source_feature_row] <- linked_feature_count[source_feature_row] + 1L
            linked_feature_info <- sprintf("%s (%s-%s)",
                                           feature_data$feature[target_feature_row],
                                           feature_data$start[target_feature_row],
                                           feature_data$end[target_feature_row])
            features_linked_to[[source_feature_row]] <- c(features_linked_to[[source_feature_row]],
                                                          linked_feature_info)
        }

        features_linked_to[[source_feature_row]] <- c(features_linked_to[[source_feature_row]],
                                                      mutual_information)
        outlier_count[source_feature_row] <- outlier_count[source_feature_row] + 1L
    }

    # Self-links have been counted twice.
    self_link_count <- self_link_count %/% 2L

    outlier_count <- outlier_count + self_link_count

    feature_data$features_linked_to <- features_linked_to
    feature_data$linked_feature_count <- linked_feature_count
    feature_data$outlier_count <- outlier_count
    feature_data$self_link_count <- self_link_count
    feature_data$features_linked_to_line_count <- lengths(features_linked_to)

    return(feature_data)
}
