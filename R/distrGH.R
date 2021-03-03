
#' The Tukey g-and-h distribution
#' 
#' Density (`dgh`), distribution function (`pgh`), quantile function (`qgh`),
#' random generation (`rgh`), and bounds of the support (`infgh` and `supgh`)
#' of the Tukey g-and-h distribution \insertCite{tukey1977;textual}{tukeyGH}.
#' All functions are vectorized with respect to all arguments, with the
#' exception of `rgh`.
#' 
#' @param p vector of probabilities.
#' @param g skewness parameter(s).
#' @param h heavy-taildness parameter(s).
#' @param a location parameter(s).
#' @param b scale parameter(s).
#' @param ... arguments passed to `uniroot`.
#' @param x vector of quantiles.
#' @inheritParams stats::rnorm
#' 
#' @references
#' \insertAllCited{}
#' 
#' @name gh
#' 
#' @export
dgh <- function(x, a = 0, b = 1, g = 0, h = 1) {
  # check the params
  if (!is_GHvalid(a = a, b = b, g = g, h = h)) { stop('Bad parameter value') }
  
  # compute the new x
  x %>%
    pgh(a = a, b = b, g = g, h = h) %>%
    qnorm() %>%
    { stats::dnorm(.) / abs(deriv_Tgh(., b = b, g = g, h = h)) } %>%
    return()
}



#' @rdname gh
#' @export
pgh <- function(x, a = 0, b = 1, g = 0, h = 1, ...) {
  # check the params
  if (!is_GHvalid(a = a, b = b, g = g, h = h)) { stop('Bad parameter value') }
  
  # vectorisation
  xdf <- data.frame(x = x, a = a, b = b, g = g, h = h, p = NA)
  rm(x, a, b, g, h)
  
  # function to be zeroed
  toroot <- function(p, a, b, g, h, x) { return(qgh(p, a, b, g, h) - x) }
  
  # computation
  seq_len(nrow(xdf)) %>%
    lapply(function(j) {
      uniroot(
        f = toroot, interval = c(0,1),
        a = xdf$a[j], b = xdf$b[j], g = xdf$g[j], h = xdf$h[j], x = xdf$x[j],
        ...
      )$root
    }) %>%
    unlist() %>%
    return()
}



#' @rdname gh
#' @export
qgh <- function(p, a = 0, b = 1, g = 0, h = 1) {
  # check the params
  if (!is_GHvalid(a = a, b = b, g = g, h = h)) { stop('Bad parameter value') }
  
  # vectorisation
  x <- data.frame(p = p, a = a, b = b, g = g, h = h)
  rm(p, a, b, g, h)
  
  # normal
  z <- qnorm(x$p, 0, 1)
  
  # h
  out <- rep(1, nrow(x))
  pos <- which(x$h != 0)
  out[pos] <- with(x, exp(h[pos] * z[pos]^2 / 2))
  
  # g
  pos <- which(x$g == 0)
  out[pos] <- z[pos] * out[pos]
  pos <- which(x$g != 0)
  out[pos] <- (exp(x$g[pos] * z[pos]) - 1) / x$g[pos] * out[pos]
  
  # location and scale
  out <- x$a + x$b * out
  
  # output
  return(out)
}



#' @rdname gh
#' @export
rgh <- function(n, a = 0, b = 1, g = 0, h = 1) {
  # check the params
  if (!is_GHvalid(a = a, b = b, g = g, h = h)) { stop('Bad parameter value') }
  
  # set the number of replications
  if (length(n) > 1) { n <- length(n) }
  
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



#' @rdname gh
#' @export
infgh <- function(a = 0, b = 1, g = 0, h = 1) {
  # check the params
  if (!is_GHvalid(a = a, b = b, g = g, h = h)) { stop('Bad parameter value') }
  
  # vectorisation
  x <- data.frame(a = a, b = b, g = g, h = h, inf = -Inf)
  rm(a, b, g, h)
  
  # computation
  pos <- which((x$g > 0) & (x$h == 0))
  x$inf[pos] <- with(x, a[pos] - b[pos] / g[pos])
  
  # output
  return(x$inf)
}



#' @rdname gh
#' @export
supgh <- function(a = 0, b = 1, g = 0, h = 1) {
  # check the params
  if (!is_GHvalid(a = a, b = b, g = g, h = h)) { stop('Bad parameter value') }
  
  # vectorisation
  x <- data.frame(a = a, b = b, g = g, h = h, sup = Inf)
  rm(a, b, g, h)
  
  # computation
  pos <- which((x$g < 0) & (x$h == 0))
  x$sup[pos] <- with(x, a[pos] - b[pos] / g[pos])
  
  # output
  return(x$sup)
}

