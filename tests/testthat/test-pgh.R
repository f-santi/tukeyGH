
test_that("distribution function works", {
  # Range 1
  depo <- pgh(q = rnorm(100))
  expect_true(all((depo >= 0) & (depo <= 1)))
  # Range 2
  depo <- gen_GHvalid(100)
  depo$q <- rnorm(100, 0, 30)
  depo %<>% with(pgh(q = q, a = a, b = b, g = g, h = h))
  expect_true(all((depo >= 0) & (depo <= 1)))
})
