
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
    k = NA,
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
  if ((msg <- is_GHvalid(g = init[1], h = exp(init[2]))) != TRUE) { stop(msg) }
  
  stats::nlm(
    f = function(theta, xdata) { -loglikGH(c(0, 1, theta[1], exp(theta[2])), xdata) },
    p = init,
    xdata = x
  )
}



# Bootstrap estimation of matrix Xi
XiBoot <- function(x, nboot, init) {
  # Initialisation
  n <- length(x)
  out <- matrix(0, nboot, 2)
  
  for (i in seq_len(nboot)) {
    # Bootstrap
    xboot <- x[sample(n, n, replace = TRUE)]
    # Quantile estimator
    Qest <- fitGH_hoaglin1985(x)$estimate
    # Pseudo MLE
    stats::nlm(
      f = function(theta, x) { -loglikST(c(0, 1, exp(theta)), x) },
      p = log(init),
      x = (xboot - Qest[1]) / Qest[2]
    ) -> depo
    # Results
    out[i, ] <- exp(depo$estimate)
  }
  
  # Output
  stats::cov(out)
}



# 
iinferenceGH_ST <- function(x, parmt, W, nsim) {
  # Simulation
  xsim <- rgh(n = nsim, a = 0, b =  1, g = x[1], h = x[2])
  
  # Optimisation
  stats::nlm(
    f = function(theta, x) { -loglikST(c(0, 1, exp(theta)), x) },
    p = log(c(0.1, 0.5)),
    x = xsim
  ) -> hatPsi
  
  # Estimation
  exp(hatPsi$estimate) %>%
    { t(parmt - .) %*% W %*% (parmt - .) } %>%
    as.vector() %>%
    return()
}




