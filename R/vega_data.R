.vega_data <- function(region_data, region_links) {
    data <- .region_tree_data(region_data)
    data <- append(data, .region_links_data(region_links))
}

.region_tree_data <- function(region_data) {
    list(
        # Full data already in tree format with hidden parent nodes for the regions.
        list(
            name = "region_data_tree",
            values = region_data,
            transform = list(
                list(type = "stratify", key = "id", parentKey = "parent"),
                list(type = "tree", method = "tidy", size = c(1, 1), as = c("alpha", "beta", "depth", "children")),
                list(type = "formula", as = "angle", expr = "(rotate + extent * datum.alpha + 270) % 360"),
                list(type = "formula", as = "leftside", expr = "inrange(datum.angle, [90, 270])"),
                list(type = "formula", as = "bottomside", expr = "inrange(datum.angle, [0, 180])"),
                list(type = "formula", as = "x", expr = "origoX + radius * datum.beta * cos(PI * datum.angle / 180)"),
                list(type = "formula", as = "y", expr = "origoY + radius * datum.beta * sin(PI * datum.angle / 180)")
            )
        ),
        # Contains only the regions (hidden parent nodes removed).
        list(
            name = "region_data",
            source = "region_data_tree",
            transform = list(list(type = "filter", expr = "datum.draw"))
        )
    )
}

.region_links_data <- function(region_links) {
    list(
        list(
            name = "region_links",
            values = region_links,
            transform = list(list(type = "formula", expr = "treePath('region_data_tree', datum.source, datum.target)", as = "treepath", initonly = TRUE))
        ),
        list(
            name = "region_links_selected",
            source = "region_links",
            transform = list(list(
                    type = "filter",
                    expr = paste("((datum.source === selected_region_1 || datum.target === selected_region_1) && selected_region_2 === null) ||",
                        "((datum.source === selected_region_2 || datum.target === selected_region_2) && selected_region_1 === null)")
                )
            )
        )
    )
}
