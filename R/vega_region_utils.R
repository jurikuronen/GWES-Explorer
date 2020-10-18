# .add_selected_gene_text <- function(selection) {
#     list(
#         type = "text",
#         name = paste0("selected_gene_text_", selection),
#         from = list(data = paste0("pos_data_selected_", selection)),
#         encode = list(
#             enter = list(
#                 text = list(field = "name"),
#                 baseline = "middle"
#             ),
#             update = list(
#                 x = list(
#                     list(test = paste0("selected_position_", selection, " === null"), field = "x"),
#                     list(value = 0)
#                 ),
#                 y = list(
#                     list(test = paste0("selected_position_", selection, " === null"),
#                         signal = paste0("right_side_y_adj + 15 * (datum.vertical_offset - data('pos_data_selected_", selection, "')[0].vertical_offset)")),
#                     list(value = 0)
#                 ),
#                 fontSize = list(signal = "innerTextSize"),
#                 fill = list(value = "black"),
#                 opacity = list(
#                     list(test = paste0("selected_position_", selection, " === null"), value = 1),
#                     list(value = 0)
#                 )
#             )
#         )
#     )
# }
#
# .add_selected_gene_main_text <- function(selection) {
#     list(
#         type = "text",
#         name = paste0("selected_gene_main_text_", selection),
#         encode = list(
#             enter = list(baseline = "middle"),
#             update = list(
#                 x = list(signal = paste("right_side_start + right_side_x_adj *", selection - 1)),
#                 y = list(signal = "right_side_y_adj - 20"),
#                 fontSize = list(signal = "innerTextSize"),
#                 fill = list(value = "black"),
#                 text = list(
#                     list(test = paste0("selected_position_", selection, "!= null"), value = ""),
#                     list(test = paste0("selected_gene_", selection, " != null"), signal = paste0("data('gene_data')[selected_gene_", selection, " - 1].name + ':'")),
#                     list(value = "")
#                 ),
#                 fontWeight = list(value = "bold")
#             )
#         )
#     )
# }
#
# .add_selected_position_main_text <- function(selection) {
#     list(
#         type = "text",
#         name = paste0("selected_position_main_text_", selection),
#         encode = list(
#             enter = list(baseline = "middle"),
#             update = list(
#                 x = list(signal = paste("right_side_start + right_side_x_adj *", selection - 1)),
#                 y = list(signal = "right_side_y_adj - 40"),
#                 fontSize = list(signal = "innerTextSize"),
#                 fill = list(value = "black"),
#                 text = list(
#                     list(test = paste0("selected_position_", selection, " != null"), signal = paste0("selected_position_", selection)),
#                     list(value = "")
#                 ),
#                 fontWeight = list(value = "bold")
#             )
#         )
#     )
# }
#
# .add_selected_position_main_gene_text <- function(selection) {
#     list(
#         type = "text",
#         name = paste0("selected_position_main_gene_text_", selection),
#         encode = list(
#             enter = list(baseline = "middle"),
#             update = list(
#                 x = list(signal = paste("right_side_start + right_side_x_adj *", selection - 1)),
#                 y = list(signal = "right_side_y_adj - 20"),
#                 fontSize = list(signal = "innerTextSize"),
#                 fill = list(value = "black"),
#                 text = list(
#                     list(test = paste0("selected_position_", selection, " != null"), signal = paste0("'(' + data('gene_data')[selected_gene_", selection, " - 1].name + '):'")),
#                     list(value = "")
#                 ),
#                 fontWeight = list(value = "bold")
#             )
#         )
#     )
# }
#
# .get_gene_marks <- function() {
#     list(
#         .add_selected_gene_main_text(1),
#         .add_selected_gene_main_text(2),
#         .add_selected_gene_text(1),
#         .add_selected_gene_text(2),
#         .add_selected_position_main_text(1),
#         .add_selected_position_main_text(2),
#         .add_selected_position_main_gene_text(1),
#         .add_selected_position_main_gene_text(2)
#     )
# }
