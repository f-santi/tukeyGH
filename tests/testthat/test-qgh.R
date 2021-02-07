
test_that("quantile function works", {
  # Mediana 1
  depo <- list(a = rnorm(1), b = rnorm(1), g = 0, h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Mediana 2
  depo <- list(a = rnorm(1), b = rnorm(1), g = rnorm(1), h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Mediana 3
  depo <- list(a = rnorm(1), b = rnorm(1), g = rnorm(1), h = rlnorm(1))
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  
  # Mediana vettoriale 1
  depo <- list(a = rnorm(100), b = rnorm(100), g = 0, h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Mediana vettoriale 2
  depo <- list(a = rnorm(100), b = rnorm(100), g = rnorm(100), h = 0)
  with(depo, expect_equal(qgh(0.5, a, b, g, h), a))
  # Mediana vettoriale 3
  depo <- list(a = rnorm(100), b = rnorm(100), g = rnorm(100), h = rlnorm(100))
})
