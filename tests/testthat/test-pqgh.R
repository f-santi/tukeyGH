
test_that("qgh and pgh work consistently", {
  # PQP 1
  depo <- gen_GHvalid(100)
  p0 <- runif(100)
  q0 <- with(depo, qgh(p = p0, a = a, b = b, g = g, h = h))
  p1 <- with(depo, pgh(q = q0, a = a, b = b, g = g, h = h, tol = 1e-6))
  expect_true(all(abs(p0 - p1) < 1e-6))
  
  # PQP 2
  depo <- gen_GHvalid(100)
  p0 <- runif(100)
  q0 <- with(depo, qgh(p = p0, a = a, b = b, g = g, h = h, lower.tail = TRUE))
  p1 <- with(depo, 
    pgh(q = q0, a = a, b = b, g = g, h = h, lower.tail = TRUE, tol = 1e-6)
  )
  expect_true(all(abs(p0 - p1) < 1e-6))
  
  # PQP 3
  depo <- gen_GHvalid(100)
  p0 <- log(runif(100))
  q0 <- with(depo, qgh(p = p0, a = a, b = b, g = g, h = h, log.p = TRUE))
  p1 <- with(depo, 
    pgh(q = q0, a = a, b = b, g = g, h = h, log.p = TRUE, tol = 1e-9)
  )
  expect_true(all(abs(p0 - p1) < 1e-6))
})
