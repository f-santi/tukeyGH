
test_that("quantile and distribution function works consistently", {
  # PQP 1
  depo <- gen_GHvalid(100)
  p0 <- runif(100)
  x0 <- with(depo, qgh(p = p0, a = a, b = b, g = g, h = h))
  p1 <- with(depo, pgh(x = x0, a = a, b = b, g = g, h = h, tol = 1e-6))
  expect_true(all(abs(p0 - p1) < 1e-6))
})
