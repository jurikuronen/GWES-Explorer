.circular_plot_panel <- function() {
    shiny::tabPanel("Circular plot",
        shiny::br(),
        .div_inline_block(3, "Modify figure:"),
        .div_inline_block(4, .prettySwitch("show_circular_plot_settings", "Transform")),
        .div_inline_block(4, .prettySwitch("show_circular_plot_text_size", "Text size")),
        .div_inline_block(4, .prettySwitch("show_circular_plot_hide_elements", "Visibility")),
        shiny::conditionalPanel(
            condition = "input.show_circular_plot_settings",
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_radius",
                "Radius:",
                min = 300,
                max = .get_cp_radius(),
                value = .get_cp_radius(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_radius_offset",
                "Radius offset:",
                min = 0,
                max = 200,
                value = .get_cp_radius_offset(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_radius_offset_adjustment",
                "Radius offset2:",
                min = -100,
                max = 200,
                value = .get_cp_radius_offset_adjustment(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_rotate",
                "Rotate plot:",
                min = 0,
                max = 360,
                value = .get_cp_rotate(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_gene_arc_angle",
                "Arc angle (gene):",
                min = 15,
                max = 90,
                value = .get_cp_gene_arc_angle(),
                step = 1)
            ),
        ),
        shiny::conditionalPanel(
            condition = "input.show_circular_plot_text_size",
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_text_size_region",
                "Text size (region):",
                min = 6,
                max = 25,
                value = .get_cp_text_size_region(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_text_size_gene",
                "Text size (gene):",
                min = 6,
                max = 20,
                value = .get_cp_text_size_gene(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_text_size_tooltip",
                "Text size (tooltip):",
                min = 6,
                max = 20,
                value = .get_cp_text_size_tooltip(),
                step = 1)
            ),
        ),
        shiny::conditionalPanel(
            condition = "input.show_circular_plot_hide_elements",
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_opacity_region_link_adjustment",
                "Region link opacity:",
                min = 0,
                max = 1,
                value = 1,
                step = 0.01)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_opacity_gene_link_adjustment",
                "Gene link opacity:",
                min = 0,
                max = 1,
                value = 1,
                step = 0.01)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::checkboxInput(
                "circular_plot_show_region_links",
                "Show region links:",
                value = TRUE)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::checkboxInput(
                "circular_plot_show_gene_links",
                "Show gene links:",
                value = TRUE)
            ),
        ),
        shiny::br(), shiny::br(),
        vegawidget::vegawidgetOutput("circular_plot"),
        #shiny::br(),
        #.div_inline_block(4, .prettySwitch("circular_plot_download_button", "Save plot")),
        #.save_plot_button("circular_plot", 3),
        shiny::br(), shiny::br(),
        shiny::h4("Click a region to open gene view. Shift-clicking another region opens another gene view for comparison."),
        shiny::h4("To hide gene views, click outside the gene view areas.")
    )
}
