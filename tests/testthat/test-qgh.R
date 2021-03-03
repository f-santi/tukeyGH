
test_that("quantile function works", {
  # Minimum 1
  depo <- gen_GHvalid(100)
  depo$g <- 0
  depo$h <- 0
  depo$p <- 0
  expect_identical(do.call('qgh', depo), rep(-Inf, 100))
  # Minimum 2
  depo <- gen_GHvalid(100)
  depo$h <- 0
  depo$p <- 0
  out <- with(depo, a - b / g)
  out[depo$g <= 0] <- -Inf
  expect_equal(do.call('qgh', depo), out)
  # Minimum 3
  depo <- gen_GHvalid(100)
  depo$p <- 0
  out <- rep(-Inf, nrow(depo))
  out[(depo$g > 0) & (depo$h == 0)] <- with(depo, a - b / g)
  expect_equal(do.call('qgh', depo), rep(-Inf, 100))
  
  # Maximum 1
  depo <- gen_GHvalid(100)
  depo$g <- 0
  depo$h <- 0
  depo$p <- 1
  expect_identical(do.call('qgh', depo), rep(Inf, 100))
  # Maximum 2
  expect_identical(do.call('qgh', depo), rep(Inf, 100))
  depo <- gen_GHvalid(100)
  depo$h <- 0
  depo$p <- 1
  # Maximum 3
  depo <- gen_GHvalid(100)
  depo$p <- 1
  expect_identical(do.call('qgh', depo), rep(Inf, 100))
  
  # Median 1
  depo <- list(a = rnorm(1), b = rlnorm(1), g = 0, h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Median 2
  depo <- list(a = rnorm(1), b = rlnorm(1), g = rnorm(1), h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Median 3
  depo <- list(a = rnorm(1), b = rlnorm(1), g = rnorm(1), h = rlnorm(1))
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  
  # Vector median 1
  depo <- list(a = rnorm(100), b = rlnorm(100), g = 0, h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Vector median 2
  depo <- list(a = rnorm(100), b = rlnorm(100), g = rnorm(100), h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Vector median 3
  depo <- list(a = rnorm(100), b = rlnorm(100), g = rnorm(100), h = rlnorm(100))
})
