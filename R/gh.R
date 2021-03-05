
#' Fit the Tukey's g-and-h distribution
#' 
#' Fit the Tukey's g-and-h distribution on a dataset through various estimation
#' methods: `"quantile"` \insertCite{hoaglin1985}{tukeyGH}.
#' 
#' @param x data.
#' @param method estimation method.
#' 
#' @return
#' Object of class `ghfit`. Useful methods include:
#' * `coef()` point estimates of parameters
#' * `print()` short information about the object
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
gh <- function(x, method = c("quantile", "iinference", "mle")) {
  t0 <- Sys.time()
  
  switch(match.arg(method),
    iinference = gh_iinference(x),
    mle        = gh_mle(x),
    quantile   = gh_hoaglin1985(x)
  ) -> out
  
  out$call <- match.call()
  out$time <- Sys.time() - t0
  
  return(out)
}



gh_iinference <- function(x) {
  stop('to be implemented...')
}



gh_mle <- function(x) {
  # initialtisation
  out <- new_ghfit()
  
  # set starting values via the quantile method
  gh_hoaglin1985(x) %>%
    use_series('estimate') %>%
    pmax(-Inf, -Inf, 1e-4, 1e-4) %>%
    unname -> init
  
  # MLE
  optim(
    par = init,
    fn = loglikGH,
    x = x,
    method = 'L-BFGS-B',
    lower = c(-Inf, 0, -Inf, 0),
    control = list(fnscale = -1)
  ) -> depo
  
  # prepare the output
  out$distr <- 'g-and-h'
  out$method <- 'mle'
  out$estimate[1:4] <- depo$par
  out$optim <- depo
  
  # output
  return(out)
}



gh_hoaglin1985 <- function(x) {
  out <- new_ghfit()
  
  a <- median(x)
  
  # estimate g
  p <- c(0.005, 0.01, seq(0.025, 0.475, 0.025))
  z <- qnorm(p)
  # Add 1e-5 to prevent UHS and LHS to become zero as it creates problem in the log.
  UHS <- quantile(x, 1 - p) - a + 0.00001
  LHS <- a - quantile(x, p) + 0.00001
  g <- (-1 / z) * log(UHS / LHS)
  g <- median(g)
  
  # regression
  k <- log((UHS * g) / (exp(-g * z) - 1))
  kk <- (z^2 / 2)
  reg <- stats::lm(k ~ kk)
  
  # prepare the output
  out$distr <- 'g-and-h'
  out$method <- 'quantile'
  out$estimate['a'] <- a
  out$estimate['b'] <- exp(reg$coef[1])
  out$estimate['g'] <- g
  out$estimate['h'] <- reg$coef[2]
  
  # output
  return(out)
}



#' @export
print.ghfit <- function(x, ...) {
  cat('\nCall:\n')
  print(x$call)
  cat('\nPoint estimates:\n')
  print(x$estimate)
  
  # output
  invisible(x)
}



#' @export
coef.ghfit <- function(object, ...) { object$estimate }

