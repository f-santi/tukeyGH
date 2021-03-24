
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



# Bootstrap estimation of matrix Xi
XiBoot <- function(x, nboot, init) {
  # initialisation
  n <- length(x)
  out <- matrix(0, nboot, 2)
  
  for (i in seq_len(nboot)) {
    # bootstrap
    xboot <- x[sample(n, n, replace = TRUE)]
    # quantile estimator
    Qest <- fitGH_hoaglin1985(x)$estimate
    # standardisation
    xstd <- (xboot - Qest[1]) / Qest[2]
    # pseudo MLE
    optim(
      par = init,
      fn = function(theta, x) { loglikST(c(0, 1, theta), x) },
      x = xstd,
      control = list(fnscale = -1)
    ) -> depo
    # results
    out[i, ] <- depo$par
  }
  
  # output
  stats::cov(out)
}



# 
iinferenceGH_ST <- function(x, parmt, W, nsim) {
  # simulation
  xsim <- rgh(n = nsim, a = 0, b =  1, g = x[1], h = x[2])
  
  # optimisation
  hatPsi <- optim(
    par = c(0.1, 0.5),
    fn = function(theta, x) { loglikST(c(0, 1, theta), x) },
    x = xsim,
    method = "L-BFGS-B",
    lower = c(-Inf, 0),
    upper = c(Inf, Inf),
    control = list(fnscale = -1)
  )
  
  # estimation
  hatPsi %>%
    use_series('par') %>%
    { t(parmt - .) %*% W %*% (parmt - .) } %>%
    as.vector() %>%
    return()
}




