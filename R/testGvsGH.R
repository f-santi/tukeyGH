
#' compute simulation-based p-value of llr
#' 
#' Compute simulation-based p-value of llr \insertCite{bee2021b}{tukeyGH}.
#' 
#' @param x data.
#' @param nreps number of replications.
#' 
#' @return
#' Object of class `ghtest`.
#'  
#' @references
#' \insertAllCited{}
#' 
#' @export
testGvsGH <- function(x, nreps) {  
  
  Qest <- gh_hoaglin1985(x)$estimate
  x <- (x - Qest[1]) / Qest[2]
  xmin <- min(x)
  
  # estimation under H_0
  res1 <- optimize(
    f = function(theta, x) { loglikGH(c(0, 1, theta, 0), x) },
    interval = c(0, -1 / xmin),
    x = x,
    maximum = TRUE
  )
  MLE1 <- res1$maximum
  max1 <- res1$objective
  
  # unrestricted estimation
  res2 <- optim(
    par = c(MLE1, max(c(Qest[4], 0))),
    fn = function(theta, x) { loglikGH(c(0, 1, theta), x) }, 
    x = x,
    method = 'L-BFGS-B',
    lower = c(1e-10, 0),
    upper = c(Inf, Inf),
    control = list(fnscale = -1)
  )
  
  max2 <- res2$value 
  pars2 <- res2$par 
  MLE1obs <- pars2[1]
  
  llrobs <- 2 * (max2 - max1)
  llr1obs <- pmax(2 * (max2 - max1), 0)
  
  # simulation of the null distribution
  n <- length(x)
  MLE1 <- rep(0, nreps)
  MLE2 <- rep(0, nreps)
  max1 <- rep(0, nreps)
  max2 <- rep(0, nreps)
  pars2 <- matrix(0, nreps, 2)
  conv <- rep(0, nreps)
  llr <- rep(0, nreps)
  
  i <- 1
  
  while (i <= nreps) {
    xsim <- rgh(n, 0, 1, MLE1obs, 0)
    Qest <- gh_hoaglin1985(xsim)$estimate
    xsim <- (xsim - Qest[1]) / Qest[2]
    xmin <- min(xsim)
    
    # estimating under H_0
    res1 <- optimize(
      f = function(theta, x) { loglikGH(c(0, 1, theta, 0), x) },
      interval = c(0, -1 / xmin),
      x = xsim,
      maximum = TRUE
    )
    MLE1[i] <- res1$maximum
    max1[i] <- res1$objective
    
    # unrestricted estimation
    res2 <- optim(
      par = c(MLE1[i], max(c(Qest[4], 0))),
      fn = function(theta, x) { loglikGH(c(0, 1, theta), x) }, 
      x = xsim,
      method='L-BFGS-B',
      lower = c(1e-10,0),
      upper = c(Inf,Inf),
      control = list(fnscale = -1)
    )
    max2[i] <- res2$value 
    pars2[i, ] <- res2$par 
    MLE2[i] <- pars2[1]
    
    # set the test stat. to 0 in cases when round-off error produces a negative value
    llr[i] <- pmax(2 * (max2[i] - max1[i]), 0)
    
    i <- i + 1
  }
  
  pval <- length(llr[llr > llr1obs]) / nreps
  
  # output
  list(pars2 = pars2, llr = llr, llr1obs = llr1obs, pval = pval)
}
