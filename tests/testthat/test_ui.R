test_that(".create_ui_layout displays the package version in the navbar", {
    version <- as.character(utils::packageVersion("GWESExplorer"))
    html <- htmltools::renderTags(.create_ui_layout())$html

    expect_match(html, paste0("GWES-Explorer v", version), fixed = TRUE)
})
