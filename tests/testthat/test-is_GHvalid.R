
test_that("invalid parameter values are detected", {
  # Parameter h
  expect_equal(is_GHvalid(h = c(2, 3, 0, 0.8, 1.2, 18)), TRUE)
  expect_type(is_GHvalid(h = -3), 'character')
  expect_type(is_GHvalid(h = c(2, -3, 4, 9)), 'character')
})
