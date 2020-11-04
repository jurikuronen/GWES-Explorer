.div_inline_block <- function(width, content) { shiny::div(style = paste0("display: inline-block; vertical-align: top; width: ", width, "cm"), content)}
.div_inline_br_block <- function(width) { .div_inline_block(width, shiny::br()) }
.prettySwitch <- function(id, name) { shinyWidgets::prettySwitch(id, name, fill = TRUE, status = "primary", value = FALSE) }

.save_plot_button <- function(prefix, width, height) {
    if (.svg_lite_is_installed()) {
        choices <- c("png", "tiff", "svg", "jpeg", "ps", "pdf")
    } else {
        choices <- c("png", "tiff", "jpeg", "ps", "pdf")
    }
    shiny::conditionalPanel(
        condition = paste0("input.", prefix, "_download_button"),
        .div_inline_block(4, shiny::downloadButton(paste0(prefix, "_download"), label = "Download plot")),
        .div_inline_block(3, shiny::radioButtons(paste0(prefix, "_unit"), label = "Unit", choices = c("in", "cm"), selected = "in", inline = TRUE)),
        .div_inline_block(2.5, shiny::numericInput(paste0(prefix, "_width"), label = "Width", value = width, min = 0, max = 1000)),
        .div_inline_block(2.5, shiny::numericInput(paste0(prefix, "_height"), label = "Height", value = height, min = 0, max = 1000)),
        .div_inline_block(2.5, shiny::numericInput(paste0(prefix, "_dpi"), label = "DPI", value = 300, min = 0, max = 5000)),
        .div_inline_block(7.5, shiny::radioButtons(paste0(prefix, "_type"), label = "File type", choices = choices, selected = "png", inline = TRUE))
    )
}
