#include <algorithm>
#include <cstddef>
#include <iterator>
#include <string>
#include <vector>

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

namespace {

// Returns a sorted vector of all outlier positions in the data.
std::vector<std::size_t> get_sorted_outlier_positions(const Rcpp::List& outliers_direct) {
    auto sorted_outlier_positions = Rcpp::as<std::vector<std::size_t>>(outliers_direct["Pos_1"]);
    const auto pos_2 = Rcpp::as<std::vector<std::size_t>>(outliers_direct["Pos_2"]);

    sorted_outlier_positions.insert(sorted_outlier_positions.end(), pos_2.begin(), pos_2.end());

    std::sort(sorted_outlier_positions.begin(), sorted_outlier_positions.end());

    return sorted_outlier_positions;
}

// Finds the iterator of the last position that is less than or equal to value.
template <typename Container, typename Value>
typename Container::const_iterator upper_bound_prev(const Container& data, const Value& value) {
    const auto upper_bound_it = std::upper_bound(data.begin(), data.end(), value);

    return (upper_bound_it != data.begin()) ? std::prev(upper_bound_it) : data.end();
}

} // namespace

// Extracts the Name attribute values from each GFF3 attributes string.
// [[Rcpp::export(.cpp_get_gff_name_from_attributes)]]
Rcpp::CharacterVector get_gff_name_from_attributes(const Rcpp::CharacterVector& attributes) {
    const std::string name_key = "Name=";

    Rcpp::CharacterVector names(attributes.size());

    for (R_xlen_t i = 0; i < attributes.size(); ++i) {
        if (Rcpp::CharacterVector::is_na(attributes[i])) {
            Rcpp::stop("GFF3 attributes must not contain missing values.");
        }

        const auto attribute = Rcpp::as<std::string>(attributes[i]);
        auto name_key_start = attribute.find(name_key);

        // Ensure that "Name" is the full attribute key, i.e. not part of another key.
        while (name_key_start != std::string::npos && name_key_start != 0 && attribute[name_key_start - 1] != ';') {
            name_key_start = attribute.find(name_key, name_key_start + name_key.size());
        }

        // Name attribute not found in this GFF3 attributes string.
        if (name_key_start == std::string::npos) {
            continue;
        }

        // Extract the value.
        const auto name_value_start = name_key_start + name_key.size();
        const auto name_value_end = attribute.find(';', name_value_start);

        if (name_value_end == std::string::npos) {
            names[i] = attribute.substr(name_value_start);
        } else {
            names[i] = attribute.substr(name_value_start, name_value_end - name_value_start);
        }
    }

    return names;
}

/*
 * Finds and returns all intergenic regions (IGRs) with at least one outlier position in them.
 *
 * Expects that the GFF3 data has already been sorted by the caller.
*/
// [[Rcpp::export(.cpp_find_igrs_with_outliers)]]
Rcpp::DataFrame find_igrs_with_outliers(const Rcpp::NumericVector& gene_start_positions,
                                        const Rcpp::NumericVector& gene_end_positions,
                                        const Rcpp::List& outliers_direct,
                                        const Rcpp::NumericVector& region_ranges)
{
    if (gene_start_positions.size() == 0) {
        Rcpp::stop("GFF3 data must contain at least one gene.");
    }

    if (gene_start_positions.size() != gene_end_positions.size()) {
        Rcpp::stop("GFF3 start and end columns must have equal lengths.");
    }

    if (region_ranges.size() != 2 || region_ranges[0] < 1 || region_ranges[0] > region_ranges[1]) {
        Rcpp::stop("The GFF3 region must contain a valid start and end.");
    }

    const auto region_start = static_cast<std::size_t>(region_ranges[0]);
    const auto region_end = static_cast<std::size_t>(region_ranges[1]);

    // Join all outliers together into a sorted vector to enable binary search.
    const auto sorted_outlier_positions = get_sorted_outlier_positions(outliers_direct);

    std::vector<std::size_t> igr_starts{};
    std::vector<std::size_t> igr_ends{};
    std::vector<std::string> igr_names{};

    // Add an IGR between two genes if it contains at least one outlier.
    const auto add_igr_if_contains_outlier = [&sorted_outlier_positions,
                                              &igr_starts,
                                              &igr_ends,
                                              &igr_names](std::size_t igr_start, std::size_t igr_end)
    {
        if (igr_start > igr_end) {
            return;
        }

        const auto outlier_it = upper_bound_prev(sorted_outlier_positions, igr_end);

        if (outlier_it != sorted_outlier_positions.end() && *outlier_it >= igr_start) {
            const auto igr_midpoint = igr_start + (igr_end - igr_start) / 2;

            igr_starts.push_back(igr_start);
            igr_ends.push_back(igr_end);
            igr_names.push_back("IGR_" + std::to_string(igr_midpoint / 1000) + "k");
        }
    };

    // The regions on both sides of the origin form one IGR. Add both if either side contains an outlier.
    const auto add_igr_origin_if_contains_outlier = [&sorted_outlier_positions,
                                                     &igr_starts,
                                                     &igr_ends,
                                                     &igr_names,
                                                     region_start,
                                                     region_end](std::size_t first_gene_start_position,
                                                                 std::size_t furthest_seen_gene_end_position)
    {
        const auto outlier_before_first_gene_it = upper_bound_prev(sorted_outlier_positions,
                                                                   first_gene_start_position - 1);
        const auto outlier_after_last_gene_it = upper_bound_prev(sorted_outlier_positions, region_end);

        const auto contains_outlier_before_first_gene
            = outlier_before_first_gene_it != sorted_outlier_positions.end() &&
              *outlier_before_first_gene_it >= region_start;

        const auto contains_outlier_after_last_gene = outlier_after_last_gene_it != sorted_outlier_positions.end() &&
                                                      *outlier_after_last_gene_it > furthest_seen_gene_end_position;

        if (!contains_outlier_before_first_gene && !contains_outlier_after_last_gene) {
            return;
        }

        if (furthest_seen_gene_end_position < region_end) {
            igr_starts.push_back(furthest_seen_gene_end_position + 1);
            igr_ends.push_back(region_end);
            igr_names.push_back("IGR_origin");
        }

        if (region_start < first_gene_start_position) {
            igr_starts.push_back(region_start);
            igr_ends.push_back(first_gene_start_position - 1);
            igr_names.push_back("IGR_origin");
        }
    };

    std::size_t furthest_seen_gene_end_position = 0;

    // Check the regions between genes.
    for (std::size_t i = 0; i < gene_start_positions.size(); ++i) {
        const auto gene_start_position = static_cast<std::size_t>(gene_start_positions[i]);
        const auto gene_end_position = static_cast<std::size_t>(gene_end_positions[i]);

        if (gene_start_position > gene_end_position) {
            Rcpp::stop("A GFF3 gene start must not be after its end.");
        }

        if (gene_start_position < region_start || gene_end_position > region_end) {
            Rcpp::stop("GFF3 genes must be within the GFF3 region.");
        }

        if (i > 0) {
            if (gene_start_position < static_cast<std::size_t>(gene_start_positions[i - 1])) {
                Rcpp::stop("GFF3 genes must be sorted by start position.");
            }

            if (gene_start_position > furthest_seen_gene_end_position + 1) {
                add_igr_if_contains_outlier(furthest_seen_gene_end_position + 1, gene_start_position - 1);
            }
        }

        furthest_seen_gene_end_position = std::max(furthest_seen_gene_end_position, gene_end_position);
    }

    add_igr_origin_if_contains_outlier(static_cast<std::size_t>(gene_start_positions[0]),
                                       furthest_seen_gene_end_position);

    return Rcpp::DataFrame::create(
            Rcpp::Named("start") = Rcpp::wrap(igr_starts),
            Rcpp::Named("end") = Rcpp::wrap(igr_ends),
            Rcpp::Named("Name") = Rcpp::wrap(igr_names),
            Rcpp::Named("stringsAsFactors") = false);
}

/*
 * Finds the gene or calculated IGR containing each outlier position and returns its 1-based R row index.
 *
 * When genes overlap, the gene with the greatest start position is selected.
 *
 * Expects that the data is already sorted and validated by the caller.
*/
// [[Rcpp::export(.cpp_find_outlier_gene_or_igr_indices)]]
Rcpp::DataFrame find_outlier_gene_or_igr_indices(const Rcpp::NumericVector& gene_or_igr_start_positions,
                                                 const Rcpp::NumericVector& gene_or_igr_end_positions,
                                                 const Rcpp::NumericVector& outlier_positions_1,
                                                 const Rcpp::NumericVector& outlier_positions_2)
{
    if (gene_or_igr_start_positions.size() == 0) {
        Rcpp::stop("Gene and IGR data must contain at least one row.");
    }

    if (gene_or_igr_start_positions.size() != gene_or_igr_end_positions.size()) {
        Rcpp::stop("Gene and IGR start and end columns must have equal lengths.");
    }

    const auto find_gene_or_igr_index = [&gene_or_igr_start_positions,
                                         &gene_or_igr_end_positions](std::size_t outlier_position)
    {
        // Points to the gene or IGR with the greatest start position not exceeding the outlier position.
        const auto gene_or_igr_start_it = upper_bound_prev(gene_or_igr_start_positions, outlier_position);

        if (gene_or_igr_start_it == gene_or_igr_start_positions.end()) {
            Rcpp::stop("Outlier position is not within a GFF3 gene or intergenic region.");
        }

        auto gene_or_igr_index = static_cast<std::size_t>(
                std::distance(gene_or_igr_start_positions.begin(), gene_or_igr_start_it));

        // When genes overlap, the latest gene to start before the outlier may not contain the outlier. Move backwards
        // until finding the latest-starting gene that contains the outlier.
        while (gene_or_igr_index > 0 &&
               static_cast<std::size_t>(gene_or_igr_end_positions[gene_or_igr_index]) < outlier_position)
        {
            --gene_or_igr_index;
        }

        if (static_cast<std::size_t>(gene_or_igr_end_positions[gene_or_igr_index]) < outlier_position) {
            Rcpp::stop("Outlier position is not within a GFF3 gene or intergenic region.");
        }

        // R uses 1-based row indices.
        return gene_or_igr_index + 1;
    };

    std::vector<std::size_t> outlier_gene_or_igr_indices_1(outlier_positions_1.size());
    std::vector<std::size_t> outlier_gene_or_igr_indices_2(outlier_positions_2.size());

    std::transform(outlier_positions_1.begin(),
                   outlier_positions_1.end(),
                   outlier_gene_or_igr_indices_1.begin(),
                   find_gene_or_igr_index);
    std::transform(outlier_positions_2.begin(),
                   outlier_positions_2.end(),
                   outlier_gene_or_igr_indices_2.begin(),
                   find_gene_or_igr_index);

    return Rcpp::DataFrame::create(
            Rcpp::Named("pos1_gene_or_igr_index") = Rcpp::wrap(outlier_gene_or_igr_indices_1),
            Rcpp::Named("pos2_gene_or_igr_index") = Rcpp::wrap(outlier_gene_or_igr_indices_2));
}
