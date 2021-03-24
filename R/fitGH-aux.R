
# Instantiate a new object of S3 class "fitGH"
new_fitGH <- function() {
  list(
    distr = NULL,
    method = NULL,
    textmethod = NULL,
    call = NULL,
    estimate = c('a' = NA, 'b' = NA, 'g' = NA, 'h' = NA),
    sd = NULL,
    vcov = NULL,
    n = 0,
    df = NA,
    x = NULL,
    estimator = NULL,
    init = c('a' = NA, 'b' = NA, 'g' = NA, 'h' = NA),
    loglik = NA,
    AIC = NA,
    BIC = NA,
    time = NULL
  ) %>%
    structure(class = 'fitGH') %>%
    return()
}



fitGH_mle_sub2 <- function(init, x) {
  # Checks on starting values
  if ((msg <- is_GHvalid(g = init[1], h = init[2])) != TRUE) { stop(msg) }
  
  # Estimation
  optim(
    par = init,
    fn = function(theta, xdata) { loglikGH(c(0, 1, theta), xdata) },
    x = x,
    method = 'L-BFGS-B',
    lower = c(-Inf, 0),
    control = list(fnscale = -1)
  )
}


