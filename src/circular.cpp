#include <Rcpp.h>

#include <cstdint>
#include <vector>

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export(.cpp_create_pos_links)]]
Rcpp::DataFrame create_pos_links(Rcpp::List outliers_direct, Rcpp::List pos_data) {
    std::map<int64_t, int64_t> pos_data_idx_mapper;
    auto pos_data_name = Rcpp::as<std::vector<int64_t>>(pos_data["name"]);
    for (int64_t idx = 0; idx < pos_data_name.size(); ++idx) pos_data_idx_mapper[pos_data_name[idx]] = idx;
    auto Pos_1 = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1"]);
    auto Pos_2 = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2"]);
    auto Pos_1_region = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1_region"]);
    auto Pos_2_region = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2_region"]);
    auto Pos_1_gene = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_1_gene"]);
    auto Pos_2_gene = Rcpp::as<std::vector<int64_t>>(outliers_direct["Pos_2_gene"]);
    auto MI = Rcpp::as<std::vector<double>>(outliers_direct["MI"]);
    int64_t n = Pos_1.size(), n2 = 2 * n;
    std::vector<int64_t> pos_data_idx_1(n2), pos_data_idx_2(n2), region_1(n2), region_2(n2), gene_1(n2), gene_2(n2);
    std::vector<double> weight(n2);
    for (int64_t i = 0; i < n; ++i) {
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
            Rcpp::Named("weight") = Rcpp::wrap(weight));
}

