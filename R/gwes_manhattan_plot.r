.render_gwes_manhattan_plot <- function(input, mh_gwes_ranges) {
    if (.outliers_is_not_null()) {
        Distance = MI = Direct = fontsize = NULL  # R CMD check hack.
        min_mi <- min(.data$outliers$MI)
        max_mi <- max(.data$outliers$MI)
        max_distance <- max(.data$outliers$Distance)
        return(shiny::renderPlot({
            ggplot(data = .data$outliers, mapping = aes(x = Distance, y = MI, group = Direct, fontsize(12))) +
                geom_point(aes(color = Direct), size = 0.5) +
                geom_point(data = .data$outliers_direct[input$outliers_table_rows_selected, ], size = 5, color = "red", shape = 1) +
                scale_colour_manual(values=c("grey", "black")) +
                geom_line(aes(y = min_mi), color = "black") +
                scale_x_continuous(limits = c(0, max_distance), expand = c(0, 0)) +
                scale_y_continuous(limits = c(min_mi, max_mi + 0.05), expand = c(0, 0)) +
                theme(text = element_text(size = 14)) +
                coord_cartesian(xlim = mh_gwes_ranges$x, ylim = mh_gwes_ranges$y, expand = FALSE)
        }))
    } else return(NULL)
}

.render_gwes_manhattan_plot_table <- function(input, outlier_columns) { 
    return(shiny::renderTable({shiny::nearPoints(.data$outliers_direct, input$manhattan_plot_click, addDist = TRUE)[, outlier_columns]}))
}