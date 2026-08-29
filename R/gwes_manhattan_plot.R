# Creates a Shiny renderer for the GWES Manhattan plot.
# The plot is reactive and redraws when its settings, selected outliers or zoom change.
.render_gwes_manhattan_plot <- function(data, input, mh_gwes_ranges) {
    shiny::renderPlot({
        .gwes_manhattan_plot(data, input, mh_gwes_ranges)
    })
}

# Creates the GWES Manhattan plot.
.gwes_manhattan_plot <- function(data, input, mh_gwes_ranges) {
    if (is.null(data$outliers) || is.null(data$outliers_direct)) {
        return(NULL)
    }

    # R CMD check hack.
    Distance = MI = Direct = NULL
    min_mi <- min(data$outliers$MI)
    max_mi <- max(data$outliers$MI)
    max_distance <- max(data$outliers$Distance)

    return(
        ggplot(
            # Sort by Direct so direct outliers are plotted on top.
            data = dplyr::arrange(data$outliers, Direct),
            mapping = aes(x = Distance, y = MI)
        ) +
            geom_point(aes(color = Direct, size = Direct)) +
            # Highlight the selected direct outliers.
            geom_point(
                data = data$outliers_direct[input$outliers_table_rows_selected, ],
                size = input$gwes_highlight_size,
                color = input$gwes_highlight_color,
                # "circle open".
                shape = 1
            ) +
            # Draw a black horizontal line at the bottom edge of the plot (at min MI).
            geom_hline(yintercept = min_mi, color = "black") +
            scale_size_manual(values = c(input$gwes_indirect_point_size, input$gwes_direct_point_size)) +
            scale_colour_manual(values = c(input$gwes_indirect_point_color, input$gwes_direct_point_color)) +
            # The expand arguments below disable ggplot's default padding around plot limits.
            scale_x_continuous(limits = c(0, max_distance), expand = c(0, 0)) +
            # Leave space above the max MI point.
            scale_y_continuous(limits = c(min_mi, max_mi + 0.05), expand = c(0, 0)) +
            # Use coord_cartesian to enable zooming into the plot.
            coord_cartesian(
                xlim = mh_gwes_ranges$x,
                ylim = mh_gwes_ranges$y,
                expand = FALSE
            ) +
            theme(
                axis.text = element_text(size = input$gwes_axis_text_size),
                axis.title = element_text(size = input$gwes_axis_title_size),
                legend.text = element_text(size = input$gwes_legend_text_size),
                legend.title = element_text(size = input$gwes_legend_text_size)
            )
    )
}

# Creates a Shiny renderer for outliers table where outliers near a click will be shown.
.render_gwes_manhattan_plot_table <- function(data, input, outlier_columns) {
    shiny::renderTable({
        if (is.null(data$outliers) || is.null(data$outliers_direct)) {
            return(NULL)
        }
        shiny::nearPoints(data$outliers_direct, input$manhattan_plot_click)[, outlier_columns]
    })
}
