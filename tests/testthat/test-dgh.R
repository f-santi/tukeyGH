
test_that("dgh works", {
  # Range
  depo <- as.list(gen_GHvalid(1))
  depo %<>%
    modifyList(list(n = 250)) %>%
    do.call('rgh', .) %>%
    list(x = .) %>%
    modifyList(depo) %>%
    do.call('dgh', .)
  
  expect_true(all(depo >= 0))
  
  # Derivative
  dx <- 1e-8
  depo <- gen_GHvalid(200)
  depo$x <- with(depo,
    mapply(rgh, a = a, b = b, g = g, h = h, MoreArgs = list(n = 1))
  )
  depo$fx <- with(depo, dgh(x, a, b, g, h))
  depo$Fxl <- with(depo, pgh(x - 0.5 * dx, a, b, g, h, tol = 1e-12))
  depo$Fxr <- with(depo, pgh(x + 0.5 * dx, a, b, g, h, tol = 1e-12))
  depo$afx <- with(depo, (Fxr - Fxl) / dx)
  
  expect_equal(depo$fx, depo$afx, tolerance = 1e-3)
  
  # Cfr. with Gaussian
  depo <- gen_GHvalid(100, g = 0, h = 0)
  depox <- rnorm(100, depo$a, depo$b)
  expect_equal(
    with(depo, dgh(depox, a, b, g, h, tol = 1e-7)),
    with(depo, dnorm(depox, a, b)),
    tolerance = 1e-7
  )
  
  # Cfr. with Lognormal
  depo <- gen_GHvalid(100, h = 0)
  depo$g %<>% abs
  depo$x <- with(depo, mapply(rg, a = a, b = b, g = g, MoreArgs = list(n = 1)))
  expect_equal(
    with(depo, dgh(depo$x, a, b, g, h, tol = 1e-7)),
    with(depo, g / b * dlnorm(1 + g * (depo$x - a) / b, 0, g)),
    tolerance = 1e-7
  )
})
