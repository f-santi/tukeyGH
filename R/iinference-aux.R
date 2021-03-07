
# Bootstrap estimation of matrix Xi
XiBoot <- function(x, nboot, init) {
  # initialisation
  n <- length(x)
  out <- matrix(0, nboot, 2)
  
  for (i in seq_len(nboot)) {
    # bootstrap
    xboot <- x[sample(n, n, replace = TRUE)]
    # quantile estimator
    Qest <- gh_hoaglin1985(x)$estimate
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
    par = c(0.1, 0.51),
    fn = function(theta, x) { loglikST(c(0, 1, theta), x) },
    x = xsim,
    method = "L-BFGS-B",
    lower = c(0.01, 0.01),
    upper = c(10, Inf),
    control = list(fnscale = -1)
  )
  
  # estimation
  hatPsi %>%
    use_series('par') %>%
    { t(parmt - .) %*% W %*% (parmt - .) } %>%
    as.vector() %>%
    return()
}

