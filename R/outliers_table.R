# Column names in outliers table.
.default_outlier_columns <- function() { c("Pos_1", "Pos_2", "MI", "MI_wogaps", "Distance") }
.extended_outlier_columns <- function() { c("Pos_1", "Pos_2", "MI", "MI_wogaps", "Pos_1_gene_name", "Pos_2_gene_name", "Distance") }

# Generate default table when there is no input.
.generate_default_outliers_table <- function() {
    DT::renderDT(tibble::tibble(Pos_1 = integer(), Pos_2 = integer(), Distance = integer(), MI = numeric(), MI_wogaps = numeric()),
    server = FALSE, options = list(pageLength = 25, scrollX = TRUE))
}

# Outliers table generation logic.
.generate_outliers_table <- function(input, outlier_columns) {
    if (.outliers_is_not_null()) {
        return(DT::renderDT(.data$outliers_direct[, outlier_columns], server = FALSE,
            options = list(pageLength = 25, scrollX = TRUE), selection = input$select_row_type))
    } else {
        return(.generate_default_outliers_table())
    }
}
