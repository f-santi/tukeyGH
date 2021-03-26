
#' Compute simulation-based *p*-value of log-likelihood ratio test
#' 
#' Compute simulation-based *p*-value of log-likelihood ratio test
#' \insertCite{bee2021b}{tukeyGH}.
#' 
#' @inheritParams fitGH
#' @param x data.
#' @param nreps number of replications.
#' 
#' @return
#' Object of class `testGvsGH`.
#'  
#' @references
#' \insertAllCited{}
#' 
#' @examples 
#' \dontrun{
#' 
#' data(BDSF)
#' testGvsGH(BDSF, 30)
#' }
#' 
#' @export
testGvsGH <- function(x, nreps, verbose = 'v') {
  # Initialisation
  t0 <- Sys.time()
  llr <- rep(0, nreps)
  
  # Fit under Hp0 (g)
  vmessage(verbose, 1, TRUE, 'Fitting g distribution to data')
  depo <- fitG(x, verbose = FALSE)
  mleG <- stats::coef(depo)[3]
  maxG <- depo$loglik
  
  # Unrestricted fitting (g-and-h)
  vmessage(verbose, 1, TRUE, 'Fitting g-and-h distribution to data')
  depo <- fitGH(x, method = 'mle', verbose = FALSE)
  mleGH <- stats::coef(depo)[3:4]
  maxGH <- depo$loglik
  observed_llr <- pmax(2 * (maxGH - maxG), 0)
  
  # Progress bar
  vmessage(verbose, 1, TRUE, 'Running simulations')
  pb <- utils::txtProgressBar(min = 0, max = nreps, style = 3)
  
  # Simulation of the null distribution
  for (i in seq_len(nreps)) {
    # Simulation
    xsim <- rgh(n = length(x), a = 0, b = 1, g = mleGH[1], h =0)
    
    # Fit under Hp0 (g)
    depo <- fitG(xsim, verbose = FALSE)
    maxG <- depo$loglik
    
    # Unrestricted fitting (g-and-h)
    depo <- fitGH(xsim, method = 'mle', verbose = FALSE)
    maxGH <- depo$loglik
    
    # Compute the test statistic
    llr[i] <- 2 * (maxGH - maxG)
    
    # Update the progress bar
    utils::setTxtProgressBar(pb, i)
  }
  
  # Close the progress bar
  close(pb)
  
  # Output
  list(
    call = match.call(),
    n = length(x),
    nreps = nreps,
    statistic = observed_llr,
    llr = llr,
    p.value = mean(llr > observed_llr),
    time = Sys.time() - t0
  ) %>%
    structure(class = 'testGvsGH') %>%
    return()
}



#' @export
print.testGvsGH <- function(x, ...) {
  cat("\ng vs Tukey's g-and-h distribution test\n")
  cat('\nCall:\n')
  print(x$call)
  cat('\nStatistic: ', x$statistic, ', Estimated p-value: ', x$p.value,'\n\n')
  cat('\nSummary statistics of the simulated log-likelihood ratios:\n')
  print(summary(x$llr))
  
  cat('\n',
    'Fitting method: Maximum Likelihood, ',
    'Computation time: ', signif(x$time, 3), ' ', units(x$time), '\n',
    'Observations: ', x$n, ', degrees of freedom: ', 1, '\n', sep = ''
  )
  
  # Output
  invisible(x)
}



#' @export
summary.testGvsGH <- function(object, ...) {
  print(object, ...)
}

