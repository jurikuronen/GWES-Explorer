.get_cp_tension <- function() { .settings$circular_plot_tension }
.get_cp_radius <- function() { .settings$circular_plot_radius }
.get_cp_groups <- function() { .settings$circular_plot_n_groups }
.get_cp_regions <- function() { .settings$circular_plot_n_regions }
.get_cp_text_size <- function() { .settings$circular_plot_text_size }
.get_cp_small_text_size <- function(){ .settings$circular_plot_small_text_size }
.get_cp_padding <- function(){ .settings$circular_plot_padding }

.get_cp_radius_offset <- function(selection = 1) {
    return(.settings$circular_plot_radius_offset + (selection == 2) * 70)
}

.get_cp_gene_arc_angle <- function(selection = 1) {
    if (selection == 1) return(.settings$circular_plot_gene_circle_arc_angle)
    # Compute inner angle that has equal arc length.
    r1 <- .get_cp_radius() - .get_cp_radius_offset(1)
    r2 <- .get_cp_radius() - .get_cp_radius_offset(2)
    return(.get_cp_gene_arc_angle(1) * r1 / r2)
}