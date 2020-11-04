# Download handler for saving plots.
.gwes_manhattan_plot_download_handler <- function(input, mh_gwes_ranges) {
    shiny::downloadHandler(
        filename = function() { paste0(.download_name("gwes_manhattan_plot"), ".", input$gwes_manhattan_plot_type) },
        content = function(file) {
            ggsave(
                filename = file,
                plot = .gwes_manhattan_plot(input, mh_gwes_ranges),
                device = input$gwes_manhattan_plot_type,
                width = input$gwes_manhattan_plot_width * (input$gwes_manhattan_plot_dpi / 96),
                height = input$gwes_manhattan_plot_height * (input$gwes_manhattan_plot_dpi / 96),
                dpi = 96,
                units = input$gwes_manhattan_plot_unit
            )
        }
    )
}

.phylogenetic_tree_plot_download_handler <- function(input) {
    shiny::downloadHandler(
        filename = function() { paste0(.download_name("phylogenetic_tree_plot"), ".", input$phylogenetic_tree_plot_type) },
        content = function(file) {
            ggsave(
                filename = file,
                plot = .tree_plot(input),
                device = input$phylogenetic_tree_plot_type,
                width = input$phylogenetic_tree_plot_width * (input$gwes_manhattan_plot_dpi / 96),
                height = input$phylogenetic_tree_plot_height * (input$gwes_manhattan_plot_dpi / 96),
                dpi = 96,
                units = input$phylogenetic_tree_plot_unit
            )
        }
    )
}

.circular_plot_download_handler <- function(input, output) {
    NULL
}

.program_name <- function() { "GWES-Explorer" }
.current_time <- function() { format(Sys.time(), "%Y%m%d_%H%M%S") }
.download_name <- function(suffix) { paste0(.program_name(), "_", .current_time(), "_", suffix) }
