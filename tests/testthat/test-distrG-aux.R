
test_that("transformation g and its inverse are consistent", {
  depo   <- gen_GHvalid(100, h = 0)
  depo$x <- with(depo, mapply(rg, a = a, b = b, g = g, MoreArgs = list(n = 1)))
  depo$Tgx      <- with(depo, Tg(x, a, b, g))
  depo$invTgx   <- with(depo, inv_Tg(Tgx, a, b, g))
  depo$TginvTgx <- with(depo, Tg(invTgx, a, b, g))
  depo$errTit   <- with(depo, abs(x - invTgx))
  depo$erriTT   <- with(depo, abs(Tgx - TginvTgx))
  
  expect_true(all(depo$errTiT < 1e-12))
  expect_true(all(depo$erriTT < 1e-12))
})


