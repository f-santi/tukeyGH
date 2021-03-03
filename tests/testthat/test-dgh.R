
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
})
