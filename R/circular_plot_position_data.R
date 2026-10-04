# Creates outlier position data for the feature views.
.create_position_data <- function(direct_outliers, gff) {

    # Creates outlier position data for one region.
    create_position_data_for_region <- function(region_id) {

        # Creates position data for this region from Pos_1 or Pos_2.
        create_position_data_for_endpoint <- function(position_column) {
            outlier_rows <- which(direct_outliers[[paste0(position_column, "_region")]] == region_id)
            if (length(outlier_rows) == 0) {
                return(NULL)
            }
            data.frame(
                position = direct_outliers[[position_column]][outlier_rows],
                feature_row = direct_outliers[[paste0(position_column, "_feature_row")]][outlier_rows],
                region = region_id,
                weight = direct_outliers$MI[outlier_rows],
                stringsAsFactors = FALSE
            )
        }

        rbind(create_position_data_for_endpoint("Pos_1"), create_position_data_for_endpoint("Pos_2"))
    }

    # Combine outlier positions from all regions.
    position_data_by_region <- lapply(seq_len(.settings$circular_plot_region_count),
                                     create_position_data_for_region)
    position_data <- do.call(rbind, position_data_by_region)

    # First sort by descending MI to remove duplicate positions (keep the highest MI).
    position_data <- position_data[order(position_data$weight, decreasing = TRUE), ]
    position_data <- position_data[!duplicated(position_data$position), ]

    # Rescale MI values to 0.5-1. To be used for clearer position marker sizes.
    position_data$weight <- .rescale_weights(position_data$weight, 0.5, 1)

    # Finally sort back by region. This preserves the descending MI sort above within each region.
    position_data <- position_data[order(position_data$region), ]

    feature_start <- gff$start[position_data$feature_row]
    feature_end <- gff$end[position_data$feature_row]
    feature_span <- feature_end - feature_start
    position_in_feature <- (position_data$position - feature_start) / feature_span

    # Move positions near either end inward for display.
    position_data$position_in_feature <- pmin(0.9, pmax(0.1, position_in_feature))

    return(position_data)
}
