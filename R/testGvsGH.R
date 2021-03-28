
#' Compute simulation-based *p*-value of log-likelihood ratio test
#' 
#' Compute simulation-based *p*-value of log-likelihood ratio test
#' \insertCite{bee2021b}{tukeyGH}.
#' 
#' @inheritParams fitGH
#' @param x data.
#' @param nsim number of Monte Carlo simulations
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
testGvsGH <- function(x, nsim, verbose = 'v') {
  # Initialisation
  t0 <- Sys.time()
  LLR <- rep(0, nsim)
  
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
  observed_LLR <- pmax(2 * (maxGH - maxG), 0)
  
  # Progress bar
  vmessage(verbose, 1, TRUE, 'Running simulations')
  pb <- utils::txtProgressBar(min = 0, max = nsim, style = 3)
  
  # Simulation of the null distribution
  for (i in seq_len(nsim)) {
    # Simulation
    xsim <- rgh(n = length(x), a = 0, b = 1, g = mleGH[1], h =0)
    
    # Fit under Hp0 (g)
    depo <- fitG(xsim, verbose = FALSE)
    maxG <- depo$loglik
    
    # Unrestricted fitting (g-and-h)
    depo <- fitGH(xsim, method = 'mle', verbose = FALSE)
    maxGH <- depo$loglik
    
    # Compute the test statistic
    LLR[i] <- 2 * (maxGH - maxG)
    
    # Update the progress bar
    utils::setTxtProgressBar(pb, i)
  }
  
  # Close the progress bar
  close(pb)
  
  # Output
  list(
    call = match.call(),
    n = length(x),
    nsim = nsim,
    statistic = observed_LLR,
    LLR = LLR,
    p.value = mean(LLR > observed_LLR),
    CIp.value = suppressWarnings(
      stats::prop.test(sum(LLR > observed_LLR), nsim)$conf.int
    ),
    time = Sys.time() - t0
  ) %>%
    structure(class = 'testGvsGH') %>%
    return()
}



#' @export
print.testGvsGH <- function(x, ...) {
  cat("\nSimulated LLR of g vs Tukey's g-and-h distribution test\n")
  cat('\nCall:\n')
  print(x$call)
  cat('\nStatistic: ', x$statistic, ', Estimated p-value: ', x$p.value, sep = '')
  cat('\nApproximate 95% C.I. of p-value: ')
  cat('(', paste0(signif(x$CIp.value, 4), collapse = ', '), ')', '\n', sep = '')
  cat('\nSummary statistics of the simulated log-likelihood ratios:\n')
  print(summary(x$LLR))
  
  cat('\n',
    'Fitting method: Maximum Likelihood\n',
    'Number of simulations: ', x$nsim, ', ',
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

