.render_gwes_manhattan_plot <- function(input, mh_gwes_ranges) {
    shiny::renderPlot({ .gwes_manhattan_plot(input, mh_gwes_ranges) })
}

.gwes_manhattan_plot <- function(input, mh_gwes_ranges) {
    if (.outliers_is_not_null()) {
        Distance = MI = Direct = fontsize = NULL  # R CMD check hack.
        min_mi <- min(.data$outliers$MI)
        max_mi <- max(.data$outliers$MI)
        max_distance <- max(.data$outliers$Distance)
        return(ggplot(
                data = dplyr::arrange(.data$outliers, Direct), # Sort data such that direct outliers are plotted in the last layer.
                mapping = aes(x = Distance, y = MI, group = Direct)
            ) + geom_point(aes(color = Direct, size = Direct)) +
                geom_point(
                    data = .data$outliers_direct[input$outliers_table_rows_selected, ],
                    size = input$gwes_selection_size,
                    color = input$gwes_selection_color,
                    shape = 1
                ) +
                scale_size_manual(
                    values = c(input$gwes_size_indirect, input$gwes_size_direct)
                ) +
                scale_colour_manual(
                    values = c(input$gwes_color_indirect, input$gwes_color_direct)
                ) +
                geom_line(aes(y = min_mi), color = "black") +
                scale_x_continuous(limits = c(0, max_distance), expand = c(0, 0)) +
                scale_y_continuous(limits = c(min_mi, max_mi + 0.05), expand = c(0, 0)) +
                theme(
                    axis.text = element_text(size = input$gwes_axis_size),
                    axis.title = element_text(size = input$gwes_title_size),
                    legend.text = element_text(size = input$gwes_legend_size),
                    legend.title = element_text(size = input$gwes_legend_size)
                ) +
                coord_cartesian(
                    xlim = mh_gwes_ranges$x,
                    ylim = mh_gwes_ranges$y,
                    expand = FALSE
                )
        )
    } else return(NULL)
}

.render_gwes_manhattan_plot_table <- function(input, outlier_columns) {
    return(shiny::renderTable({shiny::nearPoints(.data$outliers_direct, input$manhattan_plot_click, addDist = TRUE)[, outlier_columns]}))
}
