
test_that("vectorisation of *g works", {
  # qg
  depoP <- gen_GHvalid(1, h = 0)
  depoU <- runif(200)
  depoQv <- with(depoP, qg(depoU, a, b, g))
  depoQs <- numeric(200)
  for (j in seq_along(depoU)) {
    depoQs[j] <- with(depoP, qg(depoU[j], a, b, g))
  }
  expect_equal(depoQv, depoQs)
  
  # pg
  depoP <- gen_GHvalid(1, h = 0)
  depoX <- with(depoP, rg(200, a, b, g))
  depoUv <- with(depoP, pg(depoX, a, b, g))
  depoUs <- numeric(200)
  for (j in seq_along(depoX)) {
    depoUs[j] <- with(depoP, pg(depoX[j], a, b, g))
  }
  expect_equal(depoUv, depoUs)
  
  # dg
  depoP <- gen_GHvalid(1, h = 0)
  depoX <- with(depoP, rg(200, a, b, g))
  depofv <- with(depoP, dg(depoX, a, b, g))
  depofs <- numeric(200)
  for (j in seq_along(depoX)) {
    depofs[j] <- with(depoP, dg(depoX[j], a, b, g))
  }
  expect_equal(depofv, depofs)
})
