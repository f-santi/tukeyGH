
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
