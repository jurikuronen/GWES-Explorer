.circular_plot_panel <- function() {
    shiny::tabPanel("Circular plot",
        shiny::br(),
        .div_inline_block(3, "Modify figure:"),
        .div_inline_block(4, .prettySwitch("show_circular_plot_settings", "Transform")),
        .div_inline_block(4, .prettySwitch("show_circular_plot_text_size", "Text size")),
        .div_inline_block(4, .prettySwitch("show_circular_plot_hide_elements", "Visibility")),
        shiny::conditionalPanel(
            condition = "input.show_circular_plot_settings",
            .div_inline_block(5, shiny::sliderInput(
                "circular_plot_radius",
                "Radius:",
                min = 100,
                max = .circular_plot_radius(),
                value = .circular_plot_radius(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(5, shiny::sliderInput(
                "circular_plot_rotate",
                "Rotate:",
                min = 0,
                max = 360,
                value = .circular_plot_rotate(),
                step = 1)
            ),
            shiny::br(),
            .div_inline_block(5, shiny::sliderInput(
                "circular_plot_radius_gene_view_1",
                "Radius (gene view 1):",
                min = 0,
                max = 400,
                value = .circular_plot_radius_gene_view_1(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(5, shiny::sliderInput(
                "circular_plot_gene_arc_angle_1",
                "Arc angle (gene view 1):",
                min = 15,
                max = 135,
                value = .circular_plot_gene_arc_angle_1(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(5, shiny::sliderInput(
                "circular_plot_rotate_gene_view_1",
                "Rotate (gene view 1):",
                min = 0,
                max = 360,
                value = .circular_plot_rotate_gene_view_1(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::checkboxInput(
                "circular_plot_flip_gene_view_1",
                "Flip gene view 1:",
                value = .circular_plot_flip_gene_view_1())
            ),
            shiny::br(),
            .div_inline_block(5, shiny::sliderInput(
                "circular_plot_radius_gene_view_2",
                "Radius (gene view 2):",
                min = 0,
                max = 400,
                value = .circular_plot_radius_gene_view_2(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(5, shiny::sliderInput(
                "circular_plot_gene_arc_angle_2",
                "Arc angle (gene view 2):",
                min = 15,
                max = 135,
                value = .circular_plot_gene_arc_angle_2(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(5, shiny::sliderInput(
                "circular_plot_rotate_gene_view_2",
                "Rotate (gene view 2):",
                min = 0,
                max = 360,
                value = .circular_plot_rotate_gene_view_2(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::checkboxInput(
                "circular_plot_flip_gene_view_2",
                "Flip gene view 2:",
                value = .circular_plot_flip_gene_view_2())
            )
        ),
        shiny::conditionalPanel(
            condition = "input.show_circular_plot_text_size",
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_text_size_region",
                "Text size (region):",
                min = 6,
                max = 25,
                value = .circular_plot_text_size_region(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_text_size_gene",
                "Text size (gene):",
                min = 6,
                max = 20,
                value = .circular_plot_text_size_gene(),
                step = 1)
            ),
            .div_inline_br_block(0.5),
            .div_inline_block(4, shiny::sliderInput(
                "circular_plot_text_size_tooltip",
                "Text size (tooltip):",
                min = 6,
                max = 20,
                value = .circular_plot_text_size_tooltip(),
                step = 1)
            )
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
            )
        ),
        shiny::br(), shiny::br(),
        vegawidget::vegawidgetOutput("circular_plot"),
        shiny::br(), shiny::br(),
        shiny::h4("Click a region to open gene view. Shift-clicking another region opens another gene view for ",
                  "comparison."),
        shiny::h4("To hide gene views, click outside the gene view areas.")
    )
}
