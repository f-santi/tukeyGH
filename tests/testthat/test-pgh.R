
test_that("distribution function works", {
  # Range 1
  depo <- pgh(q = rnorm(100), tol = 1e-7)
  expect_true(all(depo >= 0))
  expect_true(all(depo <= 1))
  
  # Range 2
  depo <- gen_GHvalid(100)
  depo$q <- rnorm(100, 0, 30)
  depo %<>% with(pgh(q = q, a = a, b = b, g = g, h = h))
  expect_true(all(depo >= 0))
  expect_true(all(depo <= 1))
  
  # Cfr. with Gaussian
  depo <- gen_GHvalid(100, g = 0, h = 0)
  depox <- rnorm(100, depo$a, depo$b)
  expect_equal(
    with(depo, pgh(depox, a, b, g, h, tol = 1e-7)),
    with(depo, pnorm(depox, a, b)),
    tolerance = 1e-7
  )
})
