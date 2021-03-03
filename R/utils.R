
# Check the validity of parameters
is_GHvalid <- function(a, b, g, h) {

  if (!missing(h)) {
    if (any(h < 0)) { stop('Parameter "h" must be non-negative') }
  }
  
  invisible(TRUE)
}



# Randomly generates parameters
gen_GHvalid <- function(n) {
  data.frame(
    a = stats::rnorm(n),
    b = stats::rnorm(n),
    g = stats::rnorm(n),
    h = stats::rchisq(n, 1)
  )
}



# Derivative of gh transformation
deriv_Tgh <- function(x, b, g, h) {
  # check the params
  if (!is_GHvalid(b = b, g = g, h = h)) { stop('Bad parameter value') }
  
  # vectorisation
  xdf <- data.frame(x = x, b = b, g = g, h = h, p = NA)
  rm(x, b, g, h)
  
  # computation
  out <- xdf$b * exp(xdf$h * xdf$x^2 / 2)
  pos <- which(xdf$g == 0)
  out[pos] <- (1 + xdf$h[pos] * xdf$x[pos]^2) * out[pos]
  pos <- which(xdf$g != 0)
  out[pos] <- with(xdf, ((g[pos] + h[pos] * x[pos]) * exp(g[pos] * x[pos]) -
    h[pos] * x[pos]) / g[pos]) * out[pos]
  
  # output
  return(out)
}

