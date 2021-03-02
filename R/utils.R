
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
    a = rnorm(n),
    b = rnorm(n),
    g = rnorm(n),
    h = rchisq(n, 2)
  )
}

