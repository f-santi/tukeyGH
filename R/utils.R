
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



# log-likelihood function of gh
loglikGH <- function(theta, x) {
  dgh(x, a = theta[1], b = theta[2], g = theta[3], h = theta[4], log = TRUE) %>%
    sum() %>%
    return()
} 


# Instantiate a new object of S3 class "ghfit"
new_ghfit <- function() {
  list(
    distr = NULL,
    method = NULL,
    call = NULL,
    estimate = c('a' = NA, 'b' = NA, 'g' = NA, 'h' = NA),
    sd = NULL,
    vcov = NULL,
    n = 0,
    x = NULL,
    estimator = NULL,
    time = NULL
  ) %>%
    structure(class = 'ghfit') %>%
    return()
}


