
#' The g distribution
#' 
#' Density (`dg`), distribution function (`pg`), quantile function (`qg`),
#' random generation (`rg`), and bounds of the support (`infg` and `supg`) of
#' the g distribution \insertCite{tukey1977}{tukeyGH}. All functions with the
#' exception of `rgh` are vectorized with respect to all  arguments on the g
#' distribution (`x`, `q`, `p`, `a`, `b`, `g`). The functions are wrappers of
#' the g-and-h family with `h = 0`.
#' 
#' @inheritParams distr-gh
#' @param x,q vector of quantiles.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' 
#' @references
#' \insertAllCited{}
#' 
#' @name distr-g
#' 
#' @export
dg <- function(x, a = 0, b = 1, g = 0, log = FALSE, ...) {
  dgh(x = x, a = a, b = b, g = g, h = 0, log = log, ...)
}



#' @rdname distr-g
#' @export
pg <- function(q, a = 0, b = 1, g = 0, lower.tail = TRUE, log.p = FALSE, ...) {
  pgh(q = q, a = a, b = b, g = g, h = 0, lower.tail = lower.tail, log.p = log.p,
      ...)
}



#' @rdname distr-g
#' @export
qg <- function(p, a = 0, b = 1, g = 0, lower.tail = TRUE, log.p = FALSE) {
  qgh(p = p, a = a, b = b, g = g, h = 0, lower.tail = lower.tail, log.p = log.p)
}



#' @rdname distr-g
#' @export
rg <- function(n, a = 0, b = 1, g = 0) {
  rgh(n = n, a = a, b = b, g = g, h = 0)
}



#' @rdname distr-g
#' @export
infg <- function(a = 0, b = 1, g = 0) {
  infgh(a = a, b = b, g = g, h = 0)
}



#' @rdname distr-g
#' @export
supg <- function(a = 0, b = 1, g = 0) {
  infgh(a = a, b = b, g = g, h = 0)
}

