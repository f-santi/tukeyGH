
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

