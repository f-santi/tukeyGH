
# Instantiate a new object of S3 class "ghfit"
new_ghfit <- function() {
  list(
    distr = NULL,
    method = NULL,
    call = NULL,
    estimate = c('a' = NA, 'b' = NA, 'g' = NA, 'h' = NA),
    sd = NULL,
    vcov = NULL,
    n = 0,
    x = NULL,
    estimator = NULL,
    time = NULL
  ) %>%
    structure(class = 'ghfit') %>%
    return()
}



gh_mle_sub2 <- function(init, x) {
  optim(
    par = init,
    fn = function(theta, xdata) { loglikGH(c(0, 1, theta), xdata) },
    x = x,
    method = 'L-BFGS-B',
    lower = c(-Inf, 0),
    control = list(fnscale = -1)
  )
}


