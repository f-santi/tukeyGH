
test_that("vectorisation of *gh works", {
  # qgh
  depoP <- gen_GHvalid(1)
  depoU <- runif(200)
  depoQv <- with(depoP, qgh(depoU, a, b, g, h))
  depoQs <- numeric(200)
  for (j in seq_along(depoU)) {
    depoQs[j] <- with(depoP, qgh(depoU[j], a, b, g, h))
  }
  expect_equal(depoQv, depoQs)
  
  # pgh
  depoP <- gen_GHvalid(1)
  depoX <- with(depoP, rgh(200, a, b, g, h))
  depoUv <- with(depoP, pgh(depoX, a, b, g, h))
  depoUs <- numeric(200)
  for (j in seq_along(depoX)) {
    depoUs[j] <- with(depoP, pgh(depoX[j], a, b, g, h))
  }
  expect_equal(depoUv, depoUs)
  
  # dgh
  depoP <- gen_GHvalid(1)
  depoX <- with(depoP, rgh(200, a, b, g, h))
  depofv <- with(depoP, dgh(depoX, a, b, g, h))
  depofs <- numeric(200)
  for (j in seq_along(depoX)) {
    depofs[j] <- with(depoP, dgh(depoX[j], a, b, g, h))
  }
  expect_equal(depofv, depofs)
})
