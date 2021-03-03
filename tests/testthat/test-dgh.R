
test_that("dgh works", {
  # Range
  depo <- as.list(gen_GHvalid(1))
  depo %<>%
    modifyList(list(n = 250)) %>%
    do.call('rgh', .) %>%
    list(x = .) %>%
    modifyList(depo) %>%
    do.call('dgh', .)
  
  expect_true(all(depo >= 0))
})
