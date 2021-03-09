
#' Fit the Tukey's g-and-h distribution
#' 
#' Fit the Tukey's g-and-h distribution on a dataset through various estimation
#' methods: `"quantile"` \insertCite{hoaglin1985}{tukeyGH}.
#' 
#' @param x data.
#' @param method estimation method.
#' @param mixed whether...
#' 
#' @return
#' Object of class `ghfit`. Useful methods include:
#' * `coef()` point estimates of parameters
#' * `print()` short information about the object
#' * `summary()` summary information about the estimation process
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
gh <- function(x, method = c("quantile", "iinference", "mle"), mixed = TRUE) {
  t0 <- Sys.time()
  
  if (mixed == TRUE) {
    switch(match.arg(method),
      iinference = gh_iinference(x),
      mle        = gh_mle2(x),
      quantile   = gh_hoaglin1985(x)
    ) -> out  
  } else {
    switch(match.arg(method),
      iinference = gh_iinference(x),
      mle        = gh_mle4(x),
      quantile   = gh_hoaglin1985(x)
    ) -> out
  }
  
  out$call <- match.call()
  out$time <- Sys.time() - t0
  
  return(out)
}



gh_iinference <- function(x) {
  # initialtisation
  out <- new_ghfit()
  
  # set starting values via the quantile method
  gh_hoaglin1985(x) %>%
    use_series('estimate') %>%
    pmax(c(-Inf, -Inf, 0, 1e-50)) -> init
  
  # computing pseudo MLes
  optim(
    par = c(0.1, 0.51),
    fn = function(theta, x) { loglikST(c(init[1:2], theta), x) },
    x = x,
    control = list(fnscale = -1)
  ) -> depoH
  
  # W
  W <- XiBoot(x, 20, c(0.1, 0.5)) %>% solve()
  
  minqa::bobyqa(
    par = init[3:4],
    fn = iinferenceGH_ST,
    lower = c(0, 1e-50),
    upper = c(Inf, Inf),
    control = list(iprint = 0, maxfun = 600),
    parmt = depoH$par, W = W, nsim = 5000
  ) -> depo
  
  # prepare the output
  out$distr <- 'g-and-h'
  out$method <- 'iinference'
  out$estimate[1:4] <- c(init[1:2], depo$par)
  out$estimator <- depo
  
  # output
  return(out)
}



gh_mle2 <- function(x) {
  # Initialtisation
  out <- new_ghfit()
  
  # Set the starting values via the quantile method
  gh_hoaglin1985(x) %>%
    use_series('estimate') %>%
    unname -> init
  
  # MLE
  optim(
    par = init[3:4],
    fn = function(theta, x) { loglikGH(c(0, 1, theta), x) },
    x = x,
    method = 'L-BFGS-B',
    lower = c(-Inf, 0),
    control = list(fnscale = -1)
  ) -> depo
  
  # Prepare the output
  out$distr <- 'g-and-h'
  out$method <- 'mle'
  out$estimate[1:4] <- c(init[1:2], depo$par)
  out$estimator <- depo
  
  # output
  return(out)
}



gh_mle4 <- function(x) {
  # Initialtisation
  out <- new_ghfit()
  
  # Set the starting values via the quantile method
  gh_hoaglin1985(x) %>%
    use_series('estimate') %>%
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
  
  # Prepare the output
  out$distr <- 'g-and-h'
  out$method <- 'mle'
  out$estimate[1:4] <- depo$par
  out$estimator <- depo
  
  # output
  return(out)
}



gh_hoaglin1985 <- function(x) {
  # Initialisation
  out <- new_ghfit()
  
  # Estimate a
  a <- median(x)
  
  # Estimate g
  p <- c(0.005, 0.01, seq(0.025, 0.475, 0.025))
  z <- qnorm(p)
  UHS <- quantile(x, 1 - p) - a
  LHS <- a - quantile(x, p)
  g <- -log(UHS / LHS) / z
  g <- median(g)
  
  # Estimate b and h
  data.frame(
    y = log((UHS * g) / (exp(-g * z) - 1)),
    x = z^2 / 2
  ) -> depo
  
  stats::lm(y ~ x, data = depo) %>%
    use_series('coef') %T>%
    unname() -> bh
  
  if (bh[2] < 0) { bh <- c(mean(depo$y), 0) }
  
  # Prepare the output
  out$distr <- 'g-and-h'
  out$method <- 'quantile'
  out$estimate['a'] <- a
  out$estimate['b'] <- exp(bh[1])
  out$estimate['g'] <- g
  out$estimate['h'] <- bh[2]
  
  # Output
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



#' @export
summary.ghfit <- function(object, ...) {
  cat('\nFitted', toupper(object$distr), 'distribution\n')
  cat('\nCall:\n')
  print(object$call)
  cat('\nParameters:\n\n')
  depo <- as.matrix(object$estimate)
  colnames(depo) <- 'est.'
  rownames(depo) %<>% paste0('  ')
  print(signif(depo, 4))
  
  cat('\n',
    'Estimation method: ', object$method, '\n',
    'Estimation time: ', signif(object$time, 3), ' ', units(object$time), '\n',
    sep = ''
  )
  
  # output
  invisible(object)
}

