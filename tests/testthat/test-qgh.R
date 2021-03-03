
test_that("quantile function works", {
  # Minimum 1
  depo <- gen_GHvalid(100, g = 0, h = 0)
  depo$p <- 0
  expect_identical(do.call('qgh', depo), rep(-Inf, 100))
  # Minimum 2
  depo <- gen_GHvalid(100, h = 0)
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
  depo <- gen_GHvalid(100, g = 0, h = 0)
  depo$p <- 1
  expect_identical(do.call('qgh', depo), rep(Inf, 100))
  # Maximum 2
  expect_identical(do.call('qgh', depo), rep(Inf, 100))
  depo <- gen_GHvalid(100, h = 0)
  depo$p <- 1
  # Maximum 3
  depo <- gen_GHvalid(100)
  depo$p <- 1
  expect_identical(do.call('qgh', depo), rep(Inf, 100))
  
  # Median 1
  depo <- gen_GHvalid(100, g = 0, h = 0)
  depo$p <- 0.5
  expect_equal(do.call('qgh', depo), depo$a)
  # Median 2
  depo <- gen_GHvalid(100, h = 0)
  depo$p <- 0.5
  expect_equal(do.call('qgh', depo), depo$a)
  # Median 3
  depo <- gen_GHvalid(100)
  depo$p <- 0.5
  expect_equal(do.call('qgh', depo), depo$a)
})
