test_that(".status returns the expected field names", {
    result <- .status(.STATUS_SUCCESS, "Done.")

    expect_named(result, c("success", "status"))
})

test_that(".escape_html escapes HTML symbols", {
    expect_identical(.escape_html("<b>&"), "&lt;b&gt;&amp;")
})
