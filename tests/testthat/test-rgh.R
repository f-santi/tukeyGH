
test_that("rgh works", {
  # Length 1
  depo <- modifyList(
    as.list(gen_GHvalid(1)),
    list(n = round(1 / runif(1)^2))
  )
  expect_equal(length(do.call('rgh', depo)), depo$n)
  
  # Length 2
  depo <- modifyList(
    as.list(gen_GHvalid(1)), 
    list(n = rpois(round(2 + 1 / runif(1)^2), 3))
  )
  expect_equal(length(do.call('rgh', depo)), length(depo$n))
  
  # Warnings
  expect_warning(
    rgh(9, a = rnorm(2), b = rlnorm(1), g = rnorm(1), h = rlnorm(1))
  )
  expect_warning(
    rgh(9, a = rnorm(1), b = rlnorm(2), g = rnorm(1), h = rlnorm(1))
  )
  expect_warning(
    rgh(9, a = rnorm(1), b = rlnorm(1), g = rnorm(2), h = rlnorm(1))
  )
  expect_warning(
    rgh(9, a = rnorm(1), b = rlnorm(1), g = rnorm(1), h = rlnorm(2))
  )
})


