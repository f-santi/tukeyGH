
test_that("quantile function works", {
  # Minimum
  expect_identical(qgh(0, rnorm(1), rnorm(1), 0, 0), -Inf)
  expect_identical(qgh(0, rnorm(1), rnorm(1), rnorm(1), 0), -Inf)
  expect_identical(qgh(0, rnorm(1), rnorm(1), rnorm(1), rlnorm(1)), -Inf)
  
  # Maximum
  expect_identical(qgh(1, rnorm(1), rnorm(1), 0, 0), Inf)
  expect_identical(qgh(1, rnorm(1), rnorm(1), rnorm(1), 0), Inf)
  expect_identical(qgh(1, rnorm(1), rnorm(1), rnorm(1), rlnorm(1)), Inf)
  
  # Median 1
  depo <- list(a = rnorm(1), b = rnorm(1), g = 0, h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Median 2
  depo <- list(a = rnorm(1), b = rnorm(1), g = rnorm(1), h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Median 3
  depo <- list(a = rnorm(1), b = rnorm(1), g = rnorm(1), h = rlnorm(1))
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  
  # Vector median 1
  depo <- list(a = rnorm(100), b = rnorm(100), g = 0, h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Vector median 2
  depo <- list(a = rnorm(100), b = rnorm(100), g = rnorm(100), h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Vector median 3
  depo <- list(a = rnorm(100), b = rnorm(100), g = rnorm(100), h = rlnorm(100))
})
