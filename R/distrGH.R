
#' The Tukey g-and-h distribution
#' 
#' Density, distribution function, quantile function and random generation for
#' the Tukey g-and-h distribution \insertCite{tukey1977;textual}{tukeyGH}
#' 
#' @param p vector of probabilities.
#' @param g skewness parameter.
#' @param h heavy-taildness parameter.
#' @param a location parameter.
#' @param b scale parameter.
#' @param ... arguments passed to `uniroot`.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @name gh
#' 
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
  out <- exp(x$h * z^2 / 2)
  
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
#' 
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

