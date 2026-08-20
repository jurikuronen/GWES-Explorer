.div_inline_block <- function(width_cm, content) {
    shiny::div(style = paste0("display: inline-block; ",
                              "vertical-align: top; ",
                              "width: ", width_cm, "cm"),
               content)
}

.div_inline_br_block <- function(width) {
    .div_inline_block(width, shiny::br())
}

.prettySwitch <- function(id, name) {
    shinyWidgets::prettySwitch(id,
                               name,
                               fill = TRUE,
                               status = "primary",
                               value = FALSE)
}

# Creates a toggle switch for showing and hiding plot settings.
.plot_settings_switch <- function(input_id, label) {
    .div_inline_block(
        # Use a fixed width for a consistent layout.
        width_cm = 4,
        shinyWidgets::prettySwitch(
            inputId = input_id,
            label = label,
            fill = TRUE,
            status = "primary",
            # Hide the settings by default.
            value = FALSE
        )
    )
}

# Creates the controls for downloading a plot.
.plot_download_controls <- function(prefix, width, height) {
    # SVG downloads require the svglite package.
    if (requireNamespace("svglite", quietly = TRUE)) {
        filetype_choices <- c("png", "tiff", "svg", "jpeg", "ps", "pdf")
    } else {
        filetype_choices <- c("png", "tiff", "jpeg", "ps", "pdf")
    }

    shiny::tagList(
        .plot_settings_switch(
            input_id = paste0(prefix, "_download_button"),
            label = "Save plot"
        ),
        # Show the download options when the user selects "Save plot".
        shiny::conditionalPanel(
            condition = paste0("input.", prefix, "_download_button"),
            .div_inline_block(
                width_cm = 4,
                shiny::downloadButton(
                    outputId = paste0(prefix, "_download"),
                    label = "Download plot"
                )
            ),
            .div_inline_block(
                width_cm = 3,
                shiny::radioButtons(
                    inputId = paste0(prefix, "_unit"),
                    label = "Unit",
                    choices = c("cm", "in"),
                    selected = "cm",
                    inline = TRUE
                )
            ),
            .div_inline_block(
                width_cm = 2.5,
                shiny::numericInput(
                    inputId = paste0(prefix, "_width"),
                    label = "Width",
                    value = width,
                    min = 1,
                    max = 50,
                    step = 0.1
                )
            ),
            .div_inline_block(
                width_cm = 2.5,
                shiny::numericInput(
                    inputId = paste0(prefix, "_height"),
                    label = "Height",
                    value = height,
                    min = 1,
                    max = 50,
                    step = 0.1
                )
            ),
            # DPI only applies to raster image formats.
            .div_inline_block(
                width_cm = 2.5,
                shiny::conditionalPanel(
                    condition = paste0(
                        "input.", prefix, "_type === 'png' || ",
                        "input.", prefix, "_type === 'tiff' || ",
                        "input.", prefix, "_type === 'jpeg'"
                    ),
                    shiny::numericInput(
                        inputId = paste0(prefix, "_dpi"),
                        label = "DPI",
                        value = 300,
                        min = 10,
                        max = 600,
                        step = 10
                    )
                )
            ),
            .div_inline_block(
                width_cm = 7.5,
                shiny::radioButtons(
                    inputId = paste0(prefix, "_type"),
                    label = "File type",
                    choices = filetype_choices,
                    selected = "png",
                    inline = TRUE
                )
            )
        )
    )
}
