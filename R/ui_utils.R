.div_inline_block <- function(width, content) { shiny::div(style = paste0("display: inline-block; vertical-align: top; width: ", width, "cm"), content)}
.div_inline_br_block <- function(width) { .div_inline_block(width, shiny::br()) }
.prettySwitch <- function(id, name) { shinyWidgets::prettySwitch(id, name, fill = TRUE, status = "primary", value = FALSE) }
