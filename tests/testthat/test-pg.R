
test_that("distribution function of g works", {
  # Range 1
  depo <- pg(q = rnorm(100))
  expect_true(all(depo >= 0))
  expect_true(all(depo <= 1))
  
  # Range 2
  depo <- gen_GHvalid(100, h = 0)
  depo$q <- with(depo, mapply(rg, a = a, b = b, g = g, MoreArgs = list(n = 1)))
  depo %<>% with(pg(q = q, a = a, b = b, g = g))
  expect_true(all(depo >= 0))
  expect_true(all(depo <= 1))
  
  # Cfr. with Gaussian
  depo <- gen_GHvalid(100, g = 0, h = 0)
  depox <- rnorm(100, depo$a, depo$b)
  expect_equal(
    with(depo, pg(depox, a, b, g, tol = 1e-7)),
    with(depo, pnorm(depox, a, b)),
    tolerance = 1e-7
  )
})
