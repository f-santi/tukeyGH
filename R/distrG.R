
#' The g distribution
#' 
#' Density (`dg`), distribution function (`pg`), quantile function (`qg`),
#' random generation (`rg`), and bounds of the support (`infg` and `supg`) of
#' the g distribution \insertCite{tukey1977}{tukeyGH}. All functions with the
#' exception of `rg` are vectorized with respect to all  arguments on the g
#' distribution (`x`, `q`, `p`, `a`, `b`, `g`). The functions are wrappers of
#' the g-and-h family with `h = 0`.
#' 
#' @inheritParams distr-gh
#' @param x,q vector of quantiles.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' 
#' @return
#' `dg` gives the density, `pg` gives the distribution function, `qg` gives the
#' quantile function, and `rg` generates random numbers.
#' 
#' The length of the result is determined by `n` for `rg`, and is the maximum
#' of the lengths of the numerical arguments for the other functions.
#' 
#' The numerical arguments other than `n` are recycled to the length of the
#' result. Only the first elements of the logical arguments are used.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @name distr-g
#' 
#' @export
dg <- function(x, a = 0, b = 1, g = 0, log = FALSE, ...) {
  # Check the params
  if ((msg <- is_GHvalid(a = a, b = b, g = g, h = 0)) != TRUE) { stop(msg) }
  
  fc <- ifelse(log == TRUE, `-`, `/`)
  
  # Vectorisation
  X <- data.frame(x = x, a = a, b = b, g = g, row.names = NULL)
  rm(x, a, b, g)
  
  # Computation
  with(X, pg(q = x, a = a, b = b, g = g, log.p = log, ...)) %>%
    qnorm(log.p = log) %>%
    { fc(
      stats::dnorm(., log = log),
      with(X, deriv_Tg(., b = b, g = g, log = log))
    ) } %>%
    return()
}



#' @rdname distr-g
#' @export
pg <- function(q, a = 0, b = 1, g = 0, lower.tail = TRUE, log.p = FALSE, ...) {
  # Check the params
  if ((msg <- is_GHvalid(a = a, b = b, g = g, h = 0)) != TRUE) { stop(msg) }
  
  # Vectorisation
  xdf <- data.frame(x = q, a = a, b = b, g = g, p = NA, row.names = NULL)
  rm(q, a, b, g)
  
  # Computation
  with(xdf, inv_Tg(x, a, b, g)) %>%
    pnorm(0, 1, lower.tail = lower.tail[1], log.p = log.p[1]) %>%
    return()
}



#' @rdname distr-g
#' @export
qg <- function(p, a = 0, b = 1, g = 0, lower.tail = TRUE, log.p = FALSE) {
  # Check the params
  if ((msg <- is_GHvalid(a = a, b = b, g = g, h = 0)) != TRUE) { stop(msg) }
  
  # Vectorisation
  x <- data.frame(p = p, a = a, b = b, g = g, row.names = NULL)
  rm(p, a, b, g)
  
  # Computation
  out <- qnorm(x$p, 0, 1, lower.tail = lower.tail[1], log.p = log.p[1])
  out <- Tg(out, x$a, x$b, x$g)
  
  # Output
  return(out)
}



#' @rdname distr-g
#' @export
rg <- function(n, a = 0, b = 1, g = 0) {
  rgh(n = n, a = a[1], b = b[1], g = g[1], h = 0)
}



#' @rdname distr-g
#' @export
infg <- function(a = 0, b = 1, g = 0) {
  infgh(a = a, b = b, g = g, h = 0)
}



#' @rdname distr-g
#' @export
supg <- function(a = 0, b = 1, g = 0) {
  supgh(a = a, b = b, g = g, h = 0)
}

