
test_that("loglikGH works", {
  # n = 1
  depo <- as.list(gen_GHvalid(1))
  x <- do.call('rgh', args = append(depo, list(n = 1)))
  fx_dgh <- do.call('dgh', args = append(depo, list(x = x, log = TRUE)))
  fx_llk <- loglikGH(as.numeric(depo), x)
  expect_equal(fx_dgh, fx_llk)
  
  # n = 100
  x <- do.call('rgh', args = append(depo, list(n = 100)))
  fx_dgh <- do.call('dgh', args = append(depo, list(x = x, log = TRUE)))
  fx_llk <- loglikGH(as.numeric(depo), x)
  expect_equal(sum(fx_dgh), fx_llk)
})
