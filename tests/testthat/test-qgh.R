
test_that("quantile function works", {
  # Minimum 1
  depo <- gen_GHvalid(100, g = 0, h = 0)
  expect_equal(do.call('qgh', c(depo, list(p = 0))), do.call('infgh', depo))
  # Minimum 2
  depo <- gen_GHvalid(100, h = 0)
  expect_equal(do.call('qgh', c(depo, list(p = 0))), do.call('infgh', depo))
  # Minimum 3
  depo <- gen_GHvalid(100)
  expect_equal(do.call('qgh', c(depo, list(p = 0))), do.call('infgh', depo))
  
  # Maximum 1
  depo <- gen_GHvalid(100, g = 0, h = 0)
  expect_equal(do.call('qgh', c(depo, list(p = 1))), do.call('supgh', depo))
  # Maximum 2
  depo <- gen_GHvalid(100, h = 0)
  expect_equal(do.call('qgh', c(depo, list(p = 1))), do.call('supgh', depo))
  # Maximum 3
  depo <- gen_GHvalid(100)
  expect_equal(do.call('qgh', c(depo, list(p = 1))), do.call('supgh', depo))
  
  # Median 1
  depo <- gen_GHvalid(100, g = 0, h = 0)
  depo$p <- 0.5
  expect_equal(do.call('qgh', depo), depo$a)
  # Median 2
  depo <- gen_GHvalid(100, h = 0)
  depo$p <- 0.5
  expect_equal(do.call('qgh', depo), depo$a)
  # Median 3
  depo <- gen_GHvalid(100)
  depo$p <- 0.5
  expect_equal(do.call('qgh', depo), depo$a)
})
