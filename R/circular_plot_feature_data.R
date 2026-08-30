.create_feature_data <- function(data) {
    feature_data <- do.call(rbind, lapply(seq_len(.settings$circular_plot_region_count), function(region) {
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
