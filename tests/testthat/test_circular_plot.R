test_that(".rescale_weights scales varying weights", {
    expect_equal(.rescale_weights(c(2, 4, 6), 0.5, 1), c(0.5, 0.75, 1))
})

test_that(".rescale_weights scales equal weights", {
    expect_equal(.rescale_weights(rep(4, 3), 0.5, 1), rep(0.75, 3))
})
