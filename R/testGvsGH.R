
#' compute simulation-based p-value of llr
#' 
#' Compute simulation-based p-value of llr \insertCite{bee2021b}{tukeyGH}.
#' 
#' @param x data.
#' @param nreps number of replications.
#' 
#' @return
#' Object of class `testGvsGH`.
#'  
#' @references
#' \insertAllCited{}
#' 
#' @export
testGvsGH <- function(x, nreps) {  
  
  # Initialisation
  Qest <- fitGH_hoaglin1985(x)$estimate
  x <- (x - Qest[1]) / Qest[2]
  xmin <- min(x)
  
  # Fit under Hp0 (g)
  depo <- optimize(
    f = function(theta, x) { loglikGH(c(0, 1, theta, 0), x) },
    interval = c(0, -1 / xmin),
    x = x,
    maximum = TRUE
  )
  mleG <- depo$maximum
  maxG <- depo$objective
  
  # Unrestricted fitting (g-and-h)
  depo <- optim(
    par = c(mleG, max(Qest[4], 0)),
    fn = function(theta, x) { loglikGH(c(0, 1, theta), x) }, 
    x = x,
    method = 'L-BFGS-B',
    lower = c(1e-10, 0),
    upper = c(Inf, Inf),
    control = list(fnscale = -1)
  )
  maxGH <- depo$value 
  mleGH <- depo$par 
  observed_llr <- pmax(2 * (maxGH - maxG), 0)
  
  # Simulation of the null distribution
  llr <- rep(0, nreps)
  
  for (i in seq_len(nreps)) {
    vmessage('v', 1, TRUE, 'iterazione ', i)
    # Simulation
    xsim <- rgh(n = length(x), a = 0, b = 1, g = mleGH[1], h =0)
    Qest <- fitGH_hoaglin1985(xsim)$estimate
    xsim <- (xsim - Qest[1]) / Qest[2]
    xmin <- min(xsim)
    
    # Fit under Hp0 (g)
    depo <- optimize(
      f = function(theta, x) { loglikGH(c(0, 1, theta, 0), x) },
      interval = c(0, -1 / xmin),
      x = xsim,
      maximum = TRUE
    )
    maxG <- depo$objective
    
    # Unrestricted fitting (g-and-h)
    vmessage('v', 1, FALSE, '          * ', depo$maximum, ' | ', Qest[4],
      ' || ', loglikGH(c(0, 1, depo$maximum, max(Qest[4], 0)), xsim)
    )
    depo <- optim(
      par = c(depo$maximum, max(Qest[4], 0)),
      fn = function(theta, x) { loglikGH(c(0, 1, theta), x) }, 
      x = xsim,
      method ='L-BFGS-B',
      lower = c(1e-10, 0),
      upper = c(Inf, Inf),
      control = list(fnscale = -1)
    )
    maxGH <- depo$value 
    
    # Compute the test statistic
    llr[i] <- pmax(2 * (maxGH - maxG), 0)
  }
  
  # Output
  list(
    n = length(x),
    nreps = nreps,
    statistic = observed_llr,
    llr = llr,
    p.value = mean(llr > observed_llr)
  ) %>%
    structure(class = 'testGvsGH') %>%
    return()
}
