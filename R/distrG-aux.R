
# g transformation
# NOTE: all arguments must be valid and vectorised
Tg <- function(x, a, b, g) {
  # g
  pos <- which(g != 0)
  x[pos] <- (exp(g[pos] * x[pos]) - 1) / g[pos]
  
  # Location and scale
  x <- a + b * x
  
  # Output
  return(x)
}



# Inverse of the g transformation
# NOTE: all arguments must be valid and vectorised
inv_Tg <- function(x, a, b, g) {
  # Computation
  x <- (x - a) / b
  pos <- which(g != 0)
  x[pos] <- log(1 + g[pos] * x[pos]) / g[pos]
  
  # Output
  return(x)
}



# Derivative of g transformation
# NOTE: all arguments must be valid and vectorised
deriv_Tg <- function(x, b, g) {
  return(b * exp(g * x))
}



# log-likelihood function of g
loglikG <- function(theta, x) {
  dg(x, a = theta[1], b = theta[2], g = theta[3], log = TRUE) %>%
    sum() %>%
    return()
} 


