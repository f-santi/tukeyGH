
#' The Tukey's g-and-h distribution
#' 
#' Density (`dgh`), distribution function (`pgh`), quantile function (`qgh`),
#' random generation (`rgh`), and bounds of the support (`infgh` and `supgh`)
#' of the Tukey's g-and-h distribution \insertCite{tukey1977}{tukeyGH}. All
#' functions with the exception of `rgh` are vectorized with respect to all 
#' arguments on the Tukey's distribution (`x`, `q`, `p`, `a`, `b`, `g`, `h`).
#' 
#' Given a Gaussian random variable \eqn{Z\sim\mathcal{N}(0, 1)}, the following
#' transformation:
#' \deqn{
#' X=a+b\,\frac{e^{gZ}-1}{g}\,e^{\frac{hZ^2}{2}}
#' }
#' defines the Tukey's g-and-h distribution. Hence \eqn{X\sim gh(a, b, g, h)}
#' denotes a random variable distributed according to the Tukey's g-and-h
#' distribution function, where \eqn{a\in\mathbf{R}} is the location parameter,
#' \eqn{b\in\mathbf{R}^+} is the scale parameter, \eqn{g\in\mathbf{R}} is the
#' skewness parameter, and \eqn{h\in\mathbf{R}^+} is the shape parameter.
#' 
#' In principle, the shape parameter \eqn{h} may also take negative values,
#' however, in such a case, the above transformation is not monotone. All
#' functions on this page require that \eqn{h\geq0}.
#' 
#' Note that, when \eqn{g=0}, the limit for \eqn{g\to 0} of the previous
#' transformation is considered:
#' \deqn{
#' X=\lim_{g\to0}\left(a+b\,\frac{e^{gZ}-1}{g}\,e^{\frac{hZ^2}{2}}\right)=
#' a+b\,Z\,e^{\frac{hZ^2}{2}}
#' }
#' so that \eqn{X\sim gh(a, b, 0, h)}.
#' 
#' @inheritParams stats::rnorm
#' @param p vector of probabilities.
#' @param g skewness parameter(s).
#' @param h heavy-taildness parameter(s). Only non-negative values will be
#'   accepted (see *Details*).
#' @param a location parameter(s).
#' @param b scale parameter(s).
#' @param ... arguments passed to `uniroot`.
#' @param x,q vector of quantiles.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' 
#' @return
#' `dgh` gives the density, `pgh` gives the distribution function, `qgh` gives
#' the quantile function, and `rgh` generates random numbers.
#' 
#' The length of the result is determined by `n` for `rgh`, and is the maximum
#' of the lengths of the numerical arguments for the other functions.
#' 
#' The numerical arguments other than `n` are recycled to the length of the
#' result. Only the first elements of the logical arguments are used.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @name distr-gh
#' 
#' @export
dgh <- function(x, a = 0, b = 1, g = 0, h = 0.2, log = FALSE, ...) {
  # check the params
  if ((msg <- is_GHvalid(a = a, b = b, g = g, h = h)) != TRUE) { stop(msg) }
  
  fc <- ifelse(log == TRUE, `-`, `/`)
  
  # compute the new x
  x %>%
    pgh(a = a, b = b, g = g, h = h, log.p = log, ...) %>%
    qnorm(log.p = log) %>%
    { fc(
      stats::dnorm(., log = log),
      deriv_Tgh(., b = b, g = g, h = h, log = log)
    ) } %>%
    return()
}



#' @rdname distr-gh
#' @export
pgh <- function(q, a = 0, b = 1, g = 0, h = 0.2, lower.tail = TRUE,
  log.p = FALSE, ...) {
  
  # Check the params
  if ((msg <- is_GHvalid(a = a, b = b, g = g, h = h)) != TRUE) { stop(msg) }
  
  # Vectorisation
  xdf <- data.frame(x = q, a = a, b = b, g = g, h = h, p = NA, row.names = NULL)
  rm(q, a, b, g, h)
  
  # Function to be zeroed
  toroot <- function(p, a, b, g, h, x) {
    return(qgh(p, a, b, g, h, lower.tail = lower.tail[1], log.p = TRUE) - x)
  }
  
  # Initialisations
  interval <- c(-2000, 0)
  bounds <- c(-Inf, 0)
  ftrans <- ifelse(log.p == TRUE, identity, exp)
  
  # Computation
  seq_len(nrow(xdf)) %>%
    lapply(function(j) {
      suppressWarnings(uniroot(
        f = toroot, interval = interval,
        a = xdf$a[j], b = xdf$b[j], g = xdf$g[j], h = xdf$h[j], x = xdf$x[j],
        extendInt = 'upX', ...
      )) %>%
        use_series('root') %>%
        max(bounds[1]) %>%
        min(bounds[2]) %>%
        return()
    }) %>%
    unlist() %>%
    ftrans() %>%
    return()
}



#' @rdname distr-gh
#' @export
qgh <- function(p, a = 0, b = 1, g = 0, h = 0.2, lower.tail = TRUE,
  log.p = FALSE) {
  
  # check the params
  if ((msg <- is_GHvalid(a = a, b = b, g = g, h = h)) != TRUE) { stop(msg) }
  
  # vectorisation
  x <- data.frame(p = p, a = a, b = b, g = g, h = h, row.names = NULL)
  rm(p, a, b, g, h)
  
  # computation
  out <- qnorm(x$p, 0, 1, lower.tail = lower.tail[1], log.p = log.p[1])
  out <- Tgh(out, x$a, x$b, x$g, x$h)
  
  # output
  return(out)
}



#' @rdname distr-gh
#' @export
rgh <- function(n, a = 0, b = 1, g = 0, h = 0.2) {
  # check the params
  if ((msg <- is_GHvalid(a = a, b = b, g = g, h = h)) != TRUE) { stop(msg) }
  
  # set the number of replications
  if (length(n) > 1) { n <- length(n) }
  
  # check and set the parameters
  if (length(a) > 1) {
    a <- a[1]
    warning('length(a) > 1. Only the first element will be considered')
  }
  if (length(b) > 1) {
    b <- b[1]
    warning('length(b) > 1. Only the first element will be considered')
  }
  if (length(g) > 1) {
    g <- g[1]
    warning('length(g) > 1. Only the first element will be considered')
  }
  if (length(h) > 1) {
    h <- h[1]
    warning('length(h) > 1. Only the first element will be considered')
  }
  
  # normal
  depo <- stats::rnorm(n, 0, 1)
  
  # h
  out <- exp(h[1] * depo^2 / 2)
  
  # g
  if (g[1] != 0) { depo <- (exp(g[1] * depo) - 1) / g[1] }
  
  # location and scale
  out <- a[1] + b[1] * depo * out
  
  # output
  return(out)
}



#' @rdname distr-gh
#' @export
infgh <- function(a = 0, b = 1, g = 0, h = 0.2) {
  # check the params
  if ((msg <- is_GHvalid(a = a, b = b, g = g, h = h)) != TRUE) { stop(msg) }
  
  # vectorisation
  x <- data.frame(a = a, b = b, g = g, h = h, inf = -Inf, row.names = NULL)
  rm(a, b, g, h)
  
  # computation
  pos <- which((x$g > 0) & (x$h == 0))
  x$inf[pos] <- with(x, a[pos] - b[pos] / g[pos])
  
  # output
  return(x$inf)
}



#' @rdname distr-gh
#' @export
supgh <- function(a = 0, b = 1, g = 0, h = 0.2) {
  # check the params
  if ((msg <- is_GHvalid(a = a, b = b, g = g, h = h)) != TRUE) { stop(msg) }
  
  # vectorisation
  x <- data.frame(a = a, b = b, g = g, h = h, sup = Inf, row.names = NULL)
  rm(a, b, g, h)
  
  # computation
  pos <- which((x$g < 0) & (x$h == 0))
  x$sup[pos] <- with(x, a[pos] - b[pos] / g[pos])
  
  # output
  return(x$sup)
}

