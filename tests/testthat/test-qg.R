
test_that("qg works", {
  # Minimum 1
  depo <- gen_GHvalid(100, g = 0, h = 0)[ , c('a', 'b', 'g')]
  expect_equal(do.call('qg', c(depo, list(p = 0))), do.call('infg', depo))
  # Minimum 2
  depo <- gen_GHvalid(100, h = 0)[ , c('a', 'b', 'g')]
  expect_equal(do.call('qg', c(depo, list(p = 0))), do.call('infg', depo))
  # Minimum 3
  depo <- gen_GHvalid(100)[ , c('a', 'b', 'g')]
  expect_equal(do.call('qg', c(depo, list(p = 0))), do.call('infg', depo))
  
  # Maximum 1
  depo <- gen_GHvalid(100, g = 0, h = 0)[ , c('a', 'b', 'g')]
  expect_equal(do.call('qg', c(depo, list(p = 1))), do.call('supg', depo))
  # Maximum 2
  depo <- gen_GHvalid(100, h = 0)[ , c('a', 'b', 'g')]
  expect_equal(do.call('qg', c(depo, list(p = 1))), do.call('supg', depo))
  # Maximum 3
  depo <- gen_GHvalid(100)[ , c('a', 'b', 'g')]
  expect_equal(do.call('qg', c(depo, list(p = 1))), do.call('supg', depo))
  
  # Median 1
  depo <- gen_GHvalid(100, g = 0, h = 0)[ , c('a', 'b', 'g')]
  depo$p <- 0.5
  expect_equal(do.call('qg', depo), depo$a)
  # Median 2
  depo <- gen_GHvalid(100, h = 0)[ , c('a', 'b', 'g')]
  depo$p <- 0.5
  expect_equal(do.call('qg', depo), depo$a)
  # Median 3
  depo <- gen_GHvalid(100)[ , c('a', 'b', 'g')]
  depo$p <- 0.5
  expect_equal(do.call('qg', depo), depo$a)
  
  # Cfr. with Gaussian
  depo <- gen_GHvalid(100, g = 0, h = 0)
  depox <- runif(100)
  expect_equal(
    with(depo, qg(depox, a, b, g)),
    with(depo, qnorm(depox, a, b))
  )
})
