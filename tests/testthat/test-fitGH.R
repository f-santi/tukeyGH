
test_that("GH fitting through Hoaglin (1985) works", {
  # Generate data
  theta <- gen_GHvalid(1)
  p <- (1:99) / 100
  z <- qnorm(p)
  x <- lapply(z, Tgh, a = theta$a, b = theta$b, g = theta$g, h = theta$h) %>%
    unlist()
  
  # Fit the distribution
  est <- unname(fitGH_hoaglin1985(x)$estimate)
  
  # Check
  expect_equal(est[1], theta$a)
})
