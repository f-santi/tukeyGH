
#' The Tukey g-and-h distribution
#' 
#' Density, distribution function, quantile function and random generation for
#' the Tukey g-and-h distribution \insertCite{tukey1977;textual}{tukeyGH}
#' 
#' @param p vector of probabilities.
#' @param g skewness parameter.
#' @param h havy-taildness parameter.
#' @param a location parameter.
#' @param b scale parameter.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
qgh <- function(p, a = 0, b = 1, g = 0, h = 1) {
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


