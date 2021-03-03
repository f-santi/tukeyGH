
#' The g distribution
#' 
#' Density (`dg`), distribution function (`pg`), quantile function (`qg`),
#' random generation (`rg`), and bounds of the support (`infg` and `supg`) of
#' the g distribution \insertCite{tukey1977}{tukeyGH}. All functions are
#' vectorized with respect to all arguments, with the exception of `rg`. The
#' functions are wrappers of the g-and-h family with `h = 0`.
#' 
#' @inheritParams gh
#' @param x,q vector of quantiles.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @name g
#' 
#' @export
dg <- function(x, a = 0, b = 1, g = 0) {
  dgh(x = x, a = a, b = b, g = g, h = 0)
}



#' @rdname g
#' @export
pg <- function(q, a = 0, b = 1, g = 0, ...) {
  pgh(q = q, a = a, b = b, g = g, h = 0)
}



#' @rdname g
#' @export
qg <- function(p, a = 0, b = 1, g = 0) {
  qgh(p = p, a = a, b = b, g = g, h = 0)
}



#' @rdname g
#' @export
rg <- function(n, a = 0, b = 1, g = 0) {
  rgh(n = n, a = a, b = b, g = g, h = 0)
}



#' @rdname g
#' @export
infg <- function(a = 0, b = 1, g = 0) {
  infgh(a = a, b = b, g = g, h = 0)
}



#' @rdname g
#' @export
supg <- function(a = 0, b = 1, g = 0) {
  infgh(a = a, b = b, g = g, h = 0)
}

