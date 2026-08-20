test_that(".create_ui_layout displays the package version", {
    version <- as.character(utils::packageVersion("GWESExplorer"))
    html <- htmltools::renderTags(.create_ui_layout())$html

    expect_match(html, paste("Version", version), fixed = TRUE)
})
