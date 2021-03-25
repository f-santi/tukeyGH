
test_that("dg works", {
  # Range
  depo <- as.list(gen_GHvalid(1, h = 0)[, c('a', 'b', 'g')])
  depo %<>%
    modifyList(list(n = 250)) %>%
    do.call('rg', .) %>%
    list(x = .) %>%
    modifyList(depo) %>%
    do.call('dg', .)
  
  expect_true(all(depo >= 0))
  
  # Cfr. with Gaussian
  depo <- gen_GHvalid(100, g = 0, h = 0)
  depox <- rnorm(100, depo$a, depo$b)
  expect_equal(
    with(depo, dg(depox, a, b, g, tol = 1e-7)),
    with(depo, dnorm(depox, a, b)),
    tolerance = 1e-7
  )
})
