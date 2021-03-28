
# Check the validity of parameters
is_GHvalid <- function(a, b, g, h) {
  out <- TRUE

  if (!missing(b)) {
    if (any(b <= 0)) { out <- 'Parameter "b" must be positive' }
  }
  
  if (!missing(h)) {
    if (any(h < 0)) { out <-  'Parameter "h" must be non-negative' }
  }
  
  return(out)
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
deriv_Tgh <- function(x, b, g, h, log = FALSE) {
  posg0 <- which(g == 0)
  posgN <- which(g != 0)
  gZ <- g * x
  gZ[posg0] <- 0
  hZ <- h * x^2
  hZ[h == 0] <- 0
  
  if (log == TRUE) {
    out <- log(b) + hZ / 2
    out[posg0] %<>% add(log(1 + hZ[posg0]))
    out[posgN] %<>% add(log(
      exp(gZ[posgN]) + (exp(gZ[posgN] - 1)) / g[posgN] * hZ[posgN]
    ))
  } else {
    out <- b * exp(hZ / 2)
    out[posg0] %<>% multiply_by(1 + hZ[posg0])
    out[posgN] %<>% multiply_by(
      (exp(gZ[posgN]) + (exp(gZ[posgN] - 1)) / g[posgN] * hZ[posgN])
    )
  }
  
  # Output
  return(out)
}



# log-likelihood function of gh
loglikGH <- function(theta, x) {
  dgh(x, a = theta[1], b = theta[2], g = theta[3], h = theta[4], log = TRUE) %>%
    sum() %>%
    max(-.Machine$double.xmax, na.rm = TRUE) %>%
    return()
} 


