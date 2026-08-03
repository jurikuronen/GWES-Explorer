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
