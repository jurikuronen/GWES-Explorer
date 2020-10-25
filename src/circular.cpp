#include <Rcpp.h>

#include <cstdint>
#include <vector>

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export(.cpp_create_pos_links)]]
Rcpp::DataFrame create_pos_links(Rcpp::List outliers_direct, Rcpp::List pos_data) {
    std::map<int64_t, int64_t> pos_data_idx_mapper;
    auto pos_data_name = Rcpp::as<std::vector<int64_t>>(pos_data["name"]);
    for (auto idx = 0; idx < pos_data_name.size(); ++idx) pos_data_idx_mapper[pos_data_name[idx]] = idx;
    auto Pos_1 = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1"]);
    auto Pos_2 = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2"]);
    auto Pos_1_region = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1_region"]);
    auto Pos_2_region = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2_region"]);
    auto Pos_1_gene = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1_gene"]);
    auto Pos_2_gene = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2_gene"]);
    auto MI = Rcpp::as<std::vector<double>>(outliers_direct["MI"]);
    std::size_t n = Pos_1.size(), n2 = 2 * n;
    std::vector<int64_t> pos_data_idx_1(n2), pos_data_idx_2(n2), region_1(n2), region_2(n2), gene_1(n2), gene_2(n2);
    std::vector<double> weight(n2);
    for (std::size_t i = 0; i < n; ++i) {
        region_1[i * 2] = region_2[i * 2 + 1] = Pos_1_region[i];
        region_2[i * 2] = region_1[i * 2 + 1] = Pos_2_region[i];
        gene_1[i * 2] = gene_2[i * 2 + 1] = Pos_1_gene[i];
        gene_2[i * 2] = gene_1[i * 2 + 1] = Pos_2_gene[i];
        pos_data_idx_1[i * 2] = pos_data_idx_2[i * 2 + 1] = pos_data_idx_mapper[Pos_1[i]];
        pos_data_idx_2[i * 2] = pos_data_idx_1[i * 2 + 1] = pos_data_idx_mapper[Pos_2[i]];
        weight[i * 2] = weight[i * 2 + 1] = MI[i];
    }
    return Rcpp::DataFrame::create(
            Rcpp::Named("region_1") = Rcpp::wrap(region_1),
            Rcpp::Named("region_2") = Rcpp::wrap(region_2),
            Rcpp::Named("gene_1") = Rcpp::wrap(gene_1),
            Rcpp::Named("gene_2") = Rcpp::wrap(gene_2),
            Rcpp::Named("pos_data_idx_1") = Rcpp::wrap(pos_data_idx_1),
            Rcpp::Named("pos_data_idx_2") = Rcpp::wrap(pos_data_idx_2),
            Rcpp::Named("MI") = Rcpp::wrap(weight));
}

// [[Rcpp::export(.cpp_sorted_pos_links)]]
Rcpp::DataFrame sorted_pos_links(Rcpp::List pos_links) {
    auto gene_1 = Rcpp::as<std::vector<int64_t>>(pos_links["gene_1"]);
    auto gene_2 = Rcpp::as<std::vector<int64_t>>(pos_links["gene_2"]);
    auto MI = Rcpp::as<std::vector<double>>(pos_links["MI"]);
    std::size_t n = gene_1.size();
    std::map<int64_t, std::vector<std::pair<double, int64_t>>> gene_data;
    // Gather all links related to gene_1.
    for (std::size_t i = 0; i < n; ++i) gene_data[gene_1[i]].emplace_back(MI[i], gene_2[i]);
    for (auto& gene_data_point : gene_data) {
        auto& gene_links = gene_data_point.second;
        // Sort links by MI first
        std::sort(gene_links.rbegin(), gene_links.rend());
        std::map<int64_t, bool> used;
        for (auto& gene_link : gene_links) used[gene_link.second] = false;
        std::vector<std::pair<double, int64_t>> sorted_gene_links;
        // Put gene_2 duplicates below the max MI gene_2.
        for (std::size_t i = 0; i < gene_links.size(); ++i) {
            auto gene2 = gene_links[i].second;
            if (used[gene2]) continue;
            for (std::size_t j = i; j < gene_links.size(); ++j) {
                if (gene_links[j].second == gene2) sorted_gene_links.emplace_back(gene_links[j].first, gene2);
            }
            used[gene2] = true;
        }
        gene_links = sorted_gene_links;
    }
    std::vector<int64_t> gene_1_out(n);
    std::vector<int64_t> gene_2_out(n);
    std::vector<double> MI_out(n);
    auto counter = 0;
    for (auto& gene_data_point : gene_data) {
        auto gene = gene_data_point.first;
        for (auto& gene_links : gene_data_point.second) {
            auto mi = gene_links.first;
            auto gene2 = gene_links.second;
            gene_1_out[counter] = gene;
            gene_2_out[counter] = gene2;
            MI_out[counter] = mi;
            ++counter;
        }
    }
    return Rcpp::DataFrame::create(
            Rcpp::Named("gene_1") = Rcpp::wrap(gene_1_out),
            Rcpp::Named("gene_2") = Rcpp::wrap(gene_2_out),
            Rcpp::Named("MI") = Rcpp::wrap(MI_out));
}
