test_that(".vega_get_feature_tooltip displays the self-link count", {
    tooltip <- .vega_get_feature_tooltip()

    expect_match(tooltip$signal,
                 "'Self-links': datum.n_self_links",
                 fixed = TRUE)
})

