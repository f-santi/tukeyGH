
test_that("qg and pg work consistently", {
  # PQP 1
  depo <- gen_GHvalid(100, h = 0)
  p0 <- runif(100)
  q0 <- with(depo, qgh(p = p0, a = a, b = b, g = g))
  p1 <- with(depo, pgh(q = q0, a = a, b = b, g = g, tol = 1e-6))
  expect_true(all(abs(p0 - p1) < 1e-6))
  
  # PQP 2
  depo <- gen_GHvalid(100, h = 0)
  p0 <- runif(100)
  q0 <- with(depo, qgh(p = p0, a = a, b = b, g = g, lower.tail = TRUE))
  p1 <- with(depo,
    pgh(q = q0, a = a, b = b, g = g, lower.tail = TRUE, tol = 1e-6)
  )
  expect_true(all(abs(p0 - p1) < 1e-6))
  
  # PQP 3
  depo <- gen_GHvalid(100, h = 0)
  p0 <- log(runif(100))
  q0 <- with(depo, qgh(p = p0, a = a, b = b, g = g, log.p = TRUE))
  p1 <- with(depo, pgh(q = q0, a = a, b = b, g = g, log.p = TRUE, tol = 1e-9))
  expect_true(all(abs(p0 - p1) < 1e-5))
})
