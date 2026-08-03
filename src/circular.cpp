#include <algorithm>
#include <cmath>
#include <cstddef>
#include <map>
#include <numeric>
#include <utility>
#include <vector>

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

/*
 * Creates two directed circular-plot link rows for each direct outlier pair.
 *
 * Both directions are needed because either endpoint can be shown in the first selected region.
 *
 * Feature rows and region IDs are 1-based for R, while position-data indices are 0-based for Vega.
*/
// [[Rcpp::export(.cpp_create_bidirectional_position_links)]]
Rcpp::DataFrame create_bidirectional_position_links(const Rcpp::DataFrame& outliers_direct,
                                                    const Rcpp::DataFrame& position_data)
{
    const Rcpp::IntegerVector positions = position_data["position"];
    const Rcpp::IntegerVector outlier_positions_1 = outliers_direct["Pos_1"];
    const Rcpp::IntegerVector outlier_positions_2 = outliers_direct["Pos_2"];
    const Rcpp::IntegerVector region_ids_1 = outliers_direct["Pos_1_region"];
    const Rcpp::IntegerVector region_ids_2 = outliers_direct["Pos_2_region"];
    const Rcpp::IntegerVector feature_rows_1 = outliers_direct["Pos_1_feature_row"];
    const Rcpp::IntegerVector feature_rows_2 = outliers_direct["Pos_2_feature_row"];
    const Rcpp::NumericVector mutual_information = outliers_direct["MI"];

    const auto n_outliers = outlier_positions_1.size();

    if (outlier_positions_2.size() != n_outliers ||
        region_ids_1.size() != n_outliers ||
        region_ids_2.size() != n_outliers ||
        feature_rows_1.size() != n_outliers ||
        feature_rows_2.size() != n_outliers ||
        mutual_information.size() != n_outliers)
    {
        Rcpp::stop("Circular plot outlier columns must have equal lengths.");
    }

    if (n_outliers == 0) {
        Rcpp::stop("Circular plot data must contain at least one direct outlier link.");
    }

    // Map each genomic position to its 0-based index in the Vega position data.
    std::map<int, int> position_data_index_by_position;

    for (R_xlen_t i = 0; i < positions.size(); ++i) {
        const auto position = positions[i];

        if (position == NA_INTEGER || position < 1) {
            Rcpp::stop("Position data values must be positive integers.");
        }

        const auto inserted = position_data_index_by_position.emplace(position, static_cast<int>(i));

        if (!inserted.second) {
            Rcpp::stop("Circular plot position data must contain each position only once.");
        }
    }

    const auto n_directed_links = n_outliers * 2;
    Rcpp::IntegerVector output_region_ids_1(n_directed_links);
    Rcpp::IntegerVector output_region_ids_2(n_directed_links);
    Rcpp::IntegerVector output_feature_rows_1(n_directed_links);
    Rcpp::IntegerVector output_feature_rows_2(n_directed_links);
    Rcpp::IntegerVector output_position_data_indices_1(n_directed_links);
    Rcpp::IntegerVector output_position_data_indices_2(n_directed_links);
    Rcpp::NumericVector output_mutual_information(n_directed_links);

    const auto get_position_data_index = [&position_data_index_by_position](int position) {
        const auto position_it = position_data_index_by_position.find(position);

        if (position_it == position_data_index_by_position.end()) {
            Rcpp::stop("Every outlier position must be present in the circular plot position data.");
        }

        // Vega uses 0-based indices when reading rows from its position data.
        return position_it->second;
    };

    for (R_xlen_t i = 0; i < n_outliers; ++i) {
        const auto outlier_position_1 = outlier_positions_1[i];
        const auto outlier_position_2 = outlier_positions_2[i];
        const auto region_id_1 = region_ids_1[i];
        const auto region_id_2 = region_ids_2[i];
        const auto feature_row_1 = feature_rows_1[i];
        const auto feature_row_2 = feature_rows_2[i];
        const auto mutual_information_value = mutual_information[i];

        if (outlier_position_1 == NA_INTEGER || outlier_position_1 < 1 ||
            outlier_position_2 == NA_INTEGER || outlier_position_2 < 1)
        {
            Rcpp::stop("Outlier positions must be positive integers.");
        }

        if (region_id_1 == NA_INTEGER || region_id_1 < 1 ||
            region_id_2 == NA_INTEGER || region_id_2 < 1)
        {
            Rcpp::stop("Region IDs must be positive integers.");
        }

        if (feature_row_1 == NA_INTEGER || feature_row_1 < 1 ||
            feature_row_2 == NA_INTEGER || feature_row_2 < 1)
        {
            Rcpp::stop("Feature rows must be positive integers.");
        }

        if (!std::isfinite(mutual_information_value)) {
            Rcpp::stop("MI values must be finite.");
        }

        const auto position_data_index_1 = get_position_data_index(outlier_position_1);
        const auto position_data_index_2 = get_position_data_index(outlier_position_2);
        const auto direct_link_index = i * 2;
        const auto reverse_link_index = direct_link_index + 1;

        // Add both directions so the link works whichever endpoint region is selected first.
        output_region_ids_1[direct_link_index] = region_id_1;
        output_region_ids_2[direct_link_index] = region_id_2;
        output_feature_rows_1[direct_link_index] = feature_row_1;
        output_feature_rows_2[direct_link_index] = feature_row_2;
        output_position_data_indices_1[direct_link_index] = position_data_index_1;
        output_position_data_indices_2[direct_link_index] = position_data_index_2;
        output_mutual_information[direct_link_index] = mutual_information_value;

        output_region_ids_1[reverse_link_index] = region_id_2;
        output_region_ids_2[reverse_link_index] = region_id_1;
        output_feature_rows_1[reverse_link_index] = feature_row_2;
        output_feature_rows_2[reverse_link_index] = feature_row_1;
        output_position_data_indices_1[reverse_link_index] = position_data_index_2;
        output_position_data_indices_2[reverse_link_index] = position_data_index_1;
        output_mutual_information[reverse_link_index] = mutual_information_value;
    }

    return Rcpp::DataFrame::create(
            Rcpp::Named("region_1") = output_region_ids_1,
            Rcpp::Named("region_2") = output_region_ids_2,
            Rcpp::Named("feature_row_1") = output_feature_rows_1,
            Rcpp::Named("feature_row_2") = output_feature_rows_2,
            Rcpp::Named("position_data_index_1") = output_position_data_indices_1,
            Rcpp::Named("position_data_index_2") = output_position_data_indices_2,
            Rcpp::Named("MI") = output_mutual_information);
}

/*
 * Sorts the bidirectional position links for building feature tooltips.
 *
 * - Source features are sorted by row number.
 * - Target-feature groups are sorted by highest MI in the group.
 * - Equal target-feature groups are sorted by target row number.
 * - Links within each target group are sorted by MI.
*/
// [[Rcpp::export(.cpp_sort_feature_links_for_tooltips)]]
Rcpp::DataFrame sort_feature_links_for_tooltips(const Rcpp::DataFrame& position_links) {
    const Rcpp::IntegerVector source_feature_rows = position_links["feature_row_1"];
    const Rcpp::IntegerVector target_feature_rows = position_links["feature_row_2"];
    const Rcpp::NumericVector mutual_information = position_links["MI"];

    if (target_feature_rows.size() != source_feature_rows.size() ||
        mutual_information.size() != source_feature_rows.size())
    {
        Rcpp::stop("Circular plot feature-link columns must have equal lengths.");
    }

    if (source_feature_rows.size() == 0) {
        Rcpp::stop("Circular plot position links must contain at least one row.");
    }

    // A target feature's highest MI determines where its whole group appears in the tooltip.
    std::map<std::pair<int, int>, double> highest_mutual_information_by_feature_pair;

    // Validate the links and find the highest MI for each source-target feature pair.
    for (R_xlen_t i = 0; i < source_feature_rows.size(); ++i) {
        const auto source_feature_row = source_feature_rows[i];
        const auto target_feature_row = target_feature_rows[i];
        const auto mutual_information_value = mutual_information[i];

        if (source_feature_row == NA_INTEGER || source_feature_row < 1 ||
            target_feature_row == NA_INTEGER || target_feature_row < 1)
        {
            Rcpp::stop("Feature rows must be positive integers.");
        }

        if (!std::isfinite(mutual_information_value)) {
            Rcpp::stop("MI values must be finite.");
        }

        const auto feature_pair = std::make_pair(source_feature_row, target_feature_row);
        const auto inserted = highest_mutual_information_by_feature_pair.emplace(feature_pair,
                                                                                  mutual_information_value);

        if (!inserted.second) {
            inserted.first->second = std::max(inserted.first->second, mutual_information_value);
        }
    }

    // Prepare sorted link indices for the output.
    std::vector<std::size_t> link_indices(source_feature_rows.size());
    std::iota(link_indices.begin(), link_indices.end(), std::size_t{0});

    // Compare two input rows by the order detailed in the function comment.
    const auto sort_link_indices_func = [&source_feature_rows,
                                         &target_feature_rows,
                                         &mutual_information,
                                         &highest_mutual_information_by_feature_pair](std::size_t left_index,
                                                                                      std::size_t right_index)
    {
        const auto left_source_feature_row = source_feature_rows[left_index];
        const auto right_source_feature_row = source_feature_rows[right_index];

        if (left_source_feature_row != right_source_feature_row) {
            return left_source_feature_row < right_source_feature_row;
        }

        const auto left_target_feature_row = target_feature_rows[left_index];
        const auto right_target_feature_row = target_feature_rows[right_index];
        const auto left_feature_pair = std::make_pair(left_source_feature_row, left_target_feature_row);
        const auto right_feature_pair = std::make_pair(right_source_feature_row, right_target_feature_row);
        const auto left_highest_mi = highest_mutual_information_by_feature_pair.at(left_feature_pair);
        const auto right_highest_mi = highest_mutual_information_by_feature_pair.at(right_feature_pair);

        if (left_highest_mi != right_highest_mi) {
            return left_highest_mi > right_highest_mi;
        }

        if (left_target_feature_row != right_target_feature_row) {
            return left_target_feature_row < right_target_feature_row;
        }

        return mutual_information[left_index] > mutual_information[right_index];
    };

    std::sort(link_indices.begin(), link_indices.end(), sort_link_indices_func);

    Rcpp::IntegerVector output_source_feature_rows(source_feature_rows.size());
    Rcpp::IntegerVector output_target_feature_rows(target_feature_rows.size());
    Rcpp::NumericVector output_mutual_information(mutual_information.size());

    for (std::size_t output_index = 0; output_index < link_indices.size(); ++output_index) {
        const auto input_index = link_indices[output_index];
        output_source_feature_rows[output_index] = source_feature_rows[input_index];
        output_target_feature_rows[output_index] = target_feature_rows[input_index];
        output_mutual_information[output_index] = mutual_information[input_index];
    }

    return Rcpp::DataFrame::create(
            Rcpp::Named("feature_row_1") = output_source_feature_rows,
            Rcpp::Named("feature_row_2") = output_target_feature_rows,
            Rcpp::Named("MI") = output_mutual_information);
}
