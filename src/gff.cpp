#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

namespace {
    std::vector<int64_t> get_sorted_outlier_positions(Rcpp::List outliers_direct) {
        auto all_outlier_positions = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1"]);
        const auto pos_2 = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2"]);
        all_outlier_positions.insert(all_outlier_positions.end(), pos_2.begin(), pos_2.end());
        std::sort(all_outlier_positions.begin(), all_outlier_positions.end());
        return all_outlier_positions;
    }

    int64_t upper_bound_prev_idx(const std::vector<int64_t>& vec, int64_t key) {
        return std::distance(vec.begin(), std::upper_bound(vec.begin(), vec.end(), key)) - 1;
    }
}

// [[Rcpp::export(.cpp_get_gff_name_from_attributes)]]
Rcpp::CharacterVector get_gff_name_from_attributes(Rcpp::CharacterVector attributes) {
    auto names = Rcpp::as<std::vector<std::string>>(attributes);

    // Replace strings in copied vector directly, retaining only values to key "Name=".
    for (auto& str : names) {
        const auto idx = str.find("Name=");
        if (idx == std::string::npos) {
            str = "";
        } else {
            // Skip past "Name=".
            const auto name_value_start = idx + 5;
            auto name_value_end = str.find(";", name_value_start);
            if (name_value_end == std::string::npos) {
                name_value_end = str.size();
            }
            str = str.substr(name_value_start, name_value_end - name_value_start);
        }
    }

    return Rcpp::wrap(names);
}

// [[Rcpp::export(.cpp_add_igrs_to_gff)]]
Rcpp::DataFrame add_igrs_to_gff(Rcpp::List gff, Rcpp::List outliers_direct, Rcpp::NumericVector region_ranges) {
    const auto outlier_positions = get_sorted_outlier_positions(outliers_direct);
    // Add more data to the copied vectors directly.
    auto gene_start_positions = Rcpp::as<std::vector<int64_t>>(gff["start"]);
    auto gene_end_positions = Rcpp::as<std::vector<int64_t>>(gff["end"]);
    auto gene_names = Rcpp::as<std::vector<std::string>>(gff["Name"]);
    std::vector<std::pair<int64_t, int64_t>> igr_ranges;

    const auto n = static_cast<int64_t>(gene_start_positions.size());
    for (auto i = 0; i <= n; ++i) {
        const auto igr_start = (i == 0) ? region_ranges[0]: gene_end_positions[i - 1] + 1;
        const auto igr_end = (i == n) ? region_ranges[1] : gene_start_positions[i] - 1;
        // Check if there is an outlier in IGR range [igr_start, igr_end].
        const auto idx = upper_bound_prev_idx(outlier_positions, igr_end);
        if (outlier_positions[idx] >= igr_start && outlier_positions[idx] <= igr_end) {
            igr_ranges.emplace_back(igr_start, igr_end);
        }
    }
    // Add IGRs to the gene data, will be sorted outside this function.
    for (const auto& igr_range : igr_ranges) {
        int64_t igr_start, igr_end;
        std::tie(igr_start, igr_end) = igr_range;
        const auto rough_position = (igr_range.second + igr_range.first) / 2 / 1000;

        gene_start_positions.push_back(igr_start);
        gene_end_positions.push_back(igr_end);
        gene_names.push_back("IGR_" + std::to_string(rough_position) + "k");
    }

    return Rcpp::DataFrame::create(
            Rcpp::Named("start") = Rcpp::wrap(gene_start_positions),
            Rcpp::Named("end") = Rcpp::wrap(gene_end_positions),
            Rcpp::Named("Name") = Rcpp::wrap(gene_names),
            Rcpp::Named("stringsAsFactors") = false);
}

// [[Rcpp::export(.cpp_compute_outlier_genes)]]
Rcpp::DataFrame compute_outlier_genes(Rcpp::List gff, Rcpp::List outliers_direct) {
    const auto gene_start_positions = Rcpp::as<std::vector<int64_t>>(gff["start"]);
    const auto pos_1 = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1"]);
    const auto pos_2 = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2"]);
    std::vector<int64_t> pos_1_gene(pos_1.size());
    std::vector<int64_t> pos_2_gene(pos_2.size());

    const auto to_gene_index_1based = [&gene_start_positions](int64_t pos) {
        // Convert to 1-based indexing.
        return upper_bound_prev_idx(gene_start_positions, pos) + 1;
    };

    std::transform(pos_1.begin(), pos_1.end(), pos_1_gene.begin(), to_gene_index_1based);
    std::transform(pos_2.begin(), pos_2.end(), pos_2_gene.begin(), to_gene_index_1based);

    return Rcpp::DataFrame::create(
            Rcpp::Named("pos1_gene") = Rcpp::wrap(pos_1_gene),
            Rcpp::Named("pos2_gene") = Rcpp::wrap(pos_2_gene));
}
