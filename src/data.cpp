#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export(.cpp_extract_gff_name_from_attributes)]]
Rcpp::CharacterVector extract_gff_name_from_attributes(Rcpp::CharacterVector attributes) {
    auto res = Rcpp::as<std::vector<std::string>>(attributes);
    for (auto& str : res) {
        auto idx = str.find("Name");
        if (idx == std::string::npos) str = "";
        else {
            idx += 5; // Skip past "Name="
            auto length = str.find(";", idx) - idx;
            str = str.substr(idx, length);
        }
    }
    return Rcpp::wrap(res);
}

std::vector<int64_t> get_sorted_outlier_positions(Rcpp::List outliers_direct) {
    auto res = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1"]);
    auto other_pos = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2"]);
    res.insert(res.end(), other_pos.begin(), other_pos.end());
    std::sort(res.begin(), res.end());
    return res;
}

// Find element equal to key or first element less than key.
int64_t binary_search(const std::vector<int64_t>& vec, int64_t key) {
    int64_t idx = 0, n = vec.size();
    for (int64_t step = n / 2; step >= 1; step /= 2) {
        while (idx + step < n && vec[idx + step] <= key) idx += step;
    }
    return idx;
}

// Check if there is an outlier in range [start, end] corresponding to an IGR.
bool outlier_in_igr_range(const std::vector<int64_t>& outlier_positions, int64_t start, int64_t end) {
    if (start > end) return false;
    auto idx = binary_search(outlier_positions, end);
    return outlier_positions[idx] >= start && outlier_positions[idx] <= end;
}

// [[Rcpp::export(.cpp_add_igrs_to_gff)]]
Rcpp::DataFrame add_igrs_to_gff(Rcpp::List gff, Rcpp::List outliers_direct, Rcpp::NumericVector ranges) {
    auto gff_start = Rcpp::as<std::vector<int64_t>>(gff["start"]);
    auto gff_end = Rcpp::as<std::vector<int64_t>>(gff["end"]);
    auto gff_names = Rcpp::as<std::vector<std::string>>(gff["Name"]);
    auto outlier_positions = get_sorted_outlier_positions(outliers_direct);
    std::vector<std::pair<int64_t, int64_t>> igr_ranges;
    int64_t region_start = ranges[0], region_end = ranges[1], n = gff_start.size();
    int64_t start = region_start, end = gff_start[0] - 1;
    if (outlier_in_igr_range(outlier_positions, start, end)) igr_ranges.emplace_back(start, end);
    for (auto i = 0; i < n - 1; ++i) {
        start = gff_end[i] + 1, end = gff_start[i + 1] - 1;
        if (outlier_in_igr_range(outlier_positions, start, end)) igr_ranges.emplace_back(start, end);
    }
    start = gff_end[n - 1] + 1, end = region_end;
    if (outlier_in_igr_range(outlier_positions, start, end)) igr_ranges.emplace_back(start, end);
    for (auto i = 0; i < igr_ranges.size(); ++i) {
        gff_start.push_back(igr_ranges[i].first);
        gff_end.push_back(igr_ranges[i].second);
        gff_names.push_back("IGR");
    }
    return Rcpp::DataFrame::create(
            Rcpp::Named("start") = Rcpp::wrap(gff_start),
            Rcpp::Named("end") = Rcpp::wrap(gff_end),
            Rcpp::Named("Name") = Rcpp::wrap(gff_names),
            Rcpp::Named("stringsAsFactors") = false);
}

// [[Rcpp::export(.cpp_compute_outlier_genes)]]
Rcpp::DataFrame compute_outlier_genes(Rcpp::List gff, Rcpp::List outliers_direct) {
    auto gff_start = Rcpp::as<std::vector<int64_t>>(gff["start"]);
    auto gff_end = Rcpp::as<std::vector<int64_t>>(gff["end"]);
    auto pos1 = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1"]);
    auto pos2 = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2"]);
    int64_t n = pos1.size();
    std::vector<int64_t> pos1_gene(n);
    std::vector<int64_t> pos2_gene(n);
    for (auto i = 0; i < n; ++i) {
        // binary_search(...) returns first gene starting position less than given position
        pos1_gene[i] = binary_search(gff_start, pos1[i]) + 1; // Convert to 1-based index
        pos2_gene[i] = binary_search(gff_start, pos2[i]) + 1;
    }
    return Rcpp::DataFrame::create(
            Rcpp::Named("pos1_gene") = Rcpp::wrap(pos1_gene),
            Rcpp::Named("pos2_gene") = Rcpp::wrap(pos2_gene));
}
