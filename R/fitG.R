
#' Fit g distribution
#' 
#' Fit the g distribution on a dataset through maximum likelihood
#' \insertCite{bee2019b}{tukeyGH}.
#' 
#' @inheritParams fitGH
#' 
#' @inherit fitGH return
#' 
#' @references
#' \insertAllCited{}
#' 
#' @examples 
#' data("BDSF")
#' 
#' # Fit to BDSF data
#' modG <- fitG(BDSF)
#' summary(modG)
#' 
#' @export
fitG <- function(x, verbose = 'v') {
  t0 <- Sys.time()
  
  out <- fitG_mle(x, verbose)
  
  out$call <- match.call()
  out$x <- x
  out$time <- Sys.time() - t0
  out$n <- length(x)
  out$df <- out$n - 3
  
  depo <- with(out,
    is_GHvalid(estimate[1], estimate[2], estimate[3], 0)
  )
  if (is.character(depo)) {
    out$loglik <- NA
  } else {
    out$loglik <- loglikG(out$estimate, x)
    out$AIC <- 2 * out$df - 2 * out$loglik
    out$BIC <- out$df * log(out$n) - 2 * out$loglik
  }
  
  return(out)
}



fitG_mle <- function(x, verbose) {
  # Initialisation
  vmessage(verbose, 1, TRUE, 'Maximum likelihood fitting')
  vmessage(verbose, 2, TRUE, 'Initialisation...')
  out <- new_fitGH()
  
  # Set the starting values via the quantile method
  fitGH_hoaglin1985(x) %>%
    use_series('estimate') %>%
    unname -> init
  
  # Standardisation
  xst <- (x - init[1]) / init[2]
  
  # MLE
  vmessage(verbose, 2, TRUE, 'Estimation...')
  depo <- optim(
    par = mean(c(1 / max(xst), -1 / min(xst))),
    fn = function(theta, xdata) { loglikG(c(0, 1, theta), xdata) },
    xdata = xst,
    method = 'SANN',
    control = list(fnscale = -1)
  )
  
  # Prepare the output
  vmessage(verbose, 2, TRUE, 'Preparing output...')
  out$distr <- 'g'
  out$method <- 'mle'
  out$textmethod <- 'Maxmimum likelihood'
  out$estimate[1:3] <- c(init[1:2], depo$par)
  out$estimator <- depo
  
  # Output
  vmessage(verbose, 1, TRUE, 'Done!')
  return(out)
}



#' @export
print.fitG <- function(x, ...) {
  cat('\nCall:\n')
  print(x$call)
  cat('\nPoint estimates:\n')
  print(x$estimate)
  
  # output
  invisible(x)
}



#' @export
coef.fitG <- function(object, ...) { object$estimate }



#' @export
summary.fitG <- function(object, ...) {
  cat('\nFitted', toupper(object$distr), 'distribution\n')
  cat('\nCall:\n')
  print(object$call)
  cat('\nParameters:\n\n')
  depo <- as.matrix(object$estimate)
  colnames(depo) <- 'Estimate'
  rownames(depo) %<>% paste0('  ')
  print(signif(depo, 4))
  
  cat('\n',
    'Fitting method: ', object$textmethod, ', ',
    'Computation time: ', signif(object$time, 3), ' ', units(object$time), '\n',
    'Observations: ', object$n, ', degrees of freedom: ', object$df,
    ifelse(
      test = is.na(object$loglik),
      yes = '',
      no = paste0(
        ', Log-lik: ', format(object$loglik), '\n', 'AIC: ',
        format(object$AIC), ', ', 'BIC: ', format(object$BIC)
      )
    ), '\n', sep = ''
  )
  
  # Output
  invisible(object)
}



