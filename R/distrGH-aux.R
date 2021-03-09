
# Check the validity of parameters
is_GHvalid <- function(a, b, g, h) {

  if (!missing(b)) {
    if (any(b <= 0)) { stop('Parameter "b" must be positive') }
  }
  
  if (!missing(h)) {
    if (any(h < 0)) { stop('Parameter "h" must be non-negative') }
  }
  
  invisible(TRUE)
}



# Randomly generates parameters
gen_GHvalid <- function(n, a, b, g, h) {
  if (missing(a)) { a <- stats::rnorm(n) }
  if (missing(b)) { b <- stats::rlnorm(n) }
  if (missing(g)) { g <- stats::rnorm(n) }
  if (missing(h)) { h <- stats::rchisq(n, 1) }
  
  data.frame(a = a, b = b, g = g, h = h)
}



# gh transformation
# NOTE: all arguments must be valid and vectorised
Tgh <- function(z, a, b, g, h) {
  # h
  out <- rep(1, length(z))
  pos <- which(h != 0)
  out[pos] <- exp(h[pos] * z[pos]^2 / 2)
  
  # g
  pos <- which(g == 0)
  out[pos] <- z[pos] * out[pos]
  pos <- which(g != 0)
  out[pos] <- (exp(g[pos] * z[pos]) - 1) / g[pos] * out[pos]
  
  # location and scale
  out <- a + b * out
  
  # output
  return(out)
}



# Derivative of gh transformation
# NOTE: all arguments must be valid and vectorised
deriv_Tgh <- function(x, b, g, h) {
  # computation
  out <- b * exp(h * x^2 / 2)
  pos <- which(g == 0)
  out[pos] <- (1 + h[pos] * x[pos]^2) * out[pos]
  pos <- which(g != 0)
  out[pos] <- ((g[pos] + h[pos] * x[pos]) * exp(g[pos] * x[pos]) -
    h[pos] * x[pos]) / g[pos] * out[pos]
  
  # output
  return(out)
}



# log-likelihood function of gh
loglikGH <- function(theta, x) {
  dgh(x, a = theta[1], b = theta[2], g = theta[3], h = theta[4], log = TRUE) %>%
    sum() %>%
    return()
} 


