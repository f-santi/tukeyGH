
#' Fit the Tukey's g-and-h distribution
#' 
#' Fit the Tukey's g-and-h distribution on a dataset through various methods:
#' quantile estimator by \insertCite{hoaglin1985;textual}{tukeyGH}, indirect
#' inference \insertCite{bee2019a}{tukeyGH}, and maximum likelihood
#' \insertCite{bee2019b}{tukeyGH}.
#' 
#' @param x data as a `numeric`.
#' @param method estimation method (partial string matching is allowed).
#'   Indirect inference is adopted as default.
#' @param verbose function verbosity. Values `v`, `vv` and `vvv` are admitted,
#'   whereas other values (such as `""` or `FALSE`) will make the function
#'   silent.
#' 
#' @return
#' Object of class `fitGH`. Useful methods include:
#' * `coef()` point estimates of parameters
#' * `print()` short information about the object
#' * `summary()` summary information about the estimation process
#' 
#' @references
#' \insertAllCited{}
#' 
#' @examples 
#' data("BDSF")
#' 
#' # Fit to BDSF data through indirect inference
#' modII <- fitGH(BDSF)
#' summary(modII)
#' 
#' # Fit to BDSF data through the quantile estimator
#' modQ <- fitGH(BDSF, method = "quantile")
#' summary(modQ)
#' 
#' \dontrun{
#' 
#' # Fit to BDSF data through MLE (the computation time is much longer)
#' modMLE <- fitGH(BDSF, method = "mle")
#' summary(modMLE)
#' }
#' 
#' @export
fitGH <- function(x, method = c("iinference", "quantile", "mle"), verbose = 'v') {
  t0 <- Sys.time()
  
  switch(match.arg(method),
    iinference = fitGH_iinference(x),
    mle        = fitGH_mle(x, verbose),
    quantile   = fitGH_hoaglin1985(x)
  ) -> out  
  
  out$call <- match.call()
  out$x <- x
  out$time <- Sys.time() - t0
  out$n <- length(x)
  out$k <- 4
  out$df <- out$n - 4
  
  depo <- with(out,
    is_GHvalid(estimate[1], estimate[2], estimate[3], estimate[4])
  )
  if (is.character(depo)) {
    out$loglik <- NA
  } else {
    out$loglik <- loglikGH(out$estimate, x)
    out$AIC <- 2 * out$df - 2 * out$loglik
    out$BIC <- out$df * log(out$n) - 2 * out$loglik
  }
  
  return(out)
}



fitGH_iinference <- function(x) {
  # Initialisation
  out <- new_fitGH()
  
  # Set starting values via the quantile method
  fitGH_hoaglin1985(x) %>%
    use_series('estimate') %>%
    pmax(c(-Inf, -Inf, 0.1, 0.5)) -> init
  
  # Computing pseudo MLes
  optim(
    par = c(0.1, 0.5),
    fn = function(theta, x) { loglikST(c(init[1:2], theta), x) },
    x = x,
    control = list(fnscale = -1)
  ) -> depoH
  
  # W
  W <- XiBoot(x, 20, c(0.1, 0.5)) %>% solve()
  
  minqa::bobyqa(
    par = init[3:4],
    fn = iinferenceGH_ST,
    lower = c(-Inf, 0),
    upper = c(Inf, Inf),
    control = list(iprint = 0, maxfun = 600),
    parmt = depoH$par, W = W, nsim = 5000
  ) -> depo
  
  # Prepare the output
  out$distr <- 'g-and-h'
  out$method <- 'iinference'
  out$textmethod <- 'Indirect Inference'
  out$estimate[1:4] <- c(init[1:2], depo$par)
  out$estimator <- depo
  
  # Output
  return(out)
}



fitGH_mle <- function(x, verbose) {
  # Initialisation
  vmessage(verbose, 1, TRUE, 'Maximum likelihood fitting')
  vmessage(verbose, 2, TRUE, 'Initialisation...')
  out <- new_fitGH()
  
  # Set the starting values via the quantile method
  fitGH_hoaglin1985(x) %>%
    use_series('estimate') %>%
    unname -> init
  
  # MLE
  vmessage(verbose, 2, TRUE, 'Estimation...')
  # First try
  vmessage(verbose, 3, TRUE, '   - trying starting from: quantile')
  out$init[3:4] <- init[3:4]
  depo <- try(fitGH_mle_sub2(out$init[3:4], (x - init[1]) / init[2]), silent = TRUE)
  # Second try
  if (inherits(depo, 'try-error')) {
    vmessage(verbose, 3, TRUE, '   - trying starting from: quantile + 0.1')
    out$init[3:4] <- init[3:4] + c(0.1, 0.1)
    depo <- try(
      fitGH_mle_sub2(out$init[3:4], (x - init[1]) / init[2]),
      silent = TRUE
    )
  }
  # Third try
  if (inherits(depo, 'try-error')) {
    vmessage(verbose, 3, TRUE, '   - trying starting from: quantile - 0.1')
    out$init[3:4] <- init[3:4] - c(0.1, 0.1)
    depo <- try(
      fitGH_mle_sub2(out$init[3:4], (x - init[1]) / init[2]),
      silent = TRUE
    )
  }
  # Exit
  if (inherits(depo, 'try-error')) {
    stop('optimisation algorithm cannot be initialised')
  }
  
  # Prepare the output
  vmessage(verbose, 2, TRUE, 'Preparing output...')
  out$distr <- 'g-and-h'
  out$method <- 'mle'
  out$textmethod <- 'Maxmimum likelihood'
  out$estimate[1:4] <- c(init[1:2], depo$par)
  out$estimator <- depo
  
  # Output
  vmessage(verbose, 1, TRUE, 'Done!')
  return(out)
}



fitGH_hoaglin1985 <- function(x) {
  # Initialisation
  out <- new_fitGH()
  
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
  
  #if (bh[2] < 0) { bh <- c(mean(depo$y), 0) }
  
  # Prepare the output
  out$distr <- 'g-and-h'
  out$method <- 'quantile'
  out$textmethod <- 'Quantile (Hoaglin, 1985)'
  out$estimate['a'] <- a
  out$estimate['b'] <- exp(bh[1])
  out$estimate['g'] <- g
  out$estimate['h'] <- bh[2]
  
  # Output
  return(out)
}



#' @export
print.fitGH <- function(x, ...) {
  cat('\nCall:\n')
  print(x$call)
  cat('\nPoint estimates:\n')
  print(x$estimate[1:x$k])
  
  # output
  invisible(x)
}



#' @export
coef.fitGH <- function(object, ...) { object$estimate[1:object$k] }



#' @export
summary.fitGH <- function(object, ...) {
  cat('\nFitted', toupper(object$distr), 'distribution\n')
  cat('\nCall:\n')
  print(object$call)
  cat('\nParameters:\n\n')
  depo <- as.matrix(object$estimate)
  colnames(depo) <- 'Estimate'
  rownames(depo) %<>% paste0('  ')
  print(signif(depo[1:object$k, , drop = FALSE], 4))
  
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



