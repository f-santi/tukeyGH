
# Density of the skewed-t distribution - Fernandez and Steel (1998)
dst <- function (x, a = 0, b = 1, skew = 1, df = 10, log = FALSE) {
  if (log) {
    lnumerator <- log(2) - log(skew + 1 / skew)
    ldenom1 <- lbeta(0.5, df / 2) + log(b) + log(df) / 2
    ldenom2 <- (df + 1) * log(1 + (x - a)^2 / (skew^(2 * sign(x - a)) * df * b^2)) / 2
    out <- lnumerator - ldenom1 - ldenom2
  } else {
    numerator <- 2 / (skew + 1 / skew)
    denom1 <- beta(0.5, df / 2) * b * sqrt(df)
    denom2 <- (1 + (x - a)^2 / (skew^(2 * sign(x - a)) * df * b^2))^((df + 1) / 2)
    out <- numerator / (denom1 * denom2)
  }
  
  return(out)
}



# log-likelihood of the skew-t distribution of Fernandez and Steel (1998)
# theta = c(a, b, skew, df)
loglikST <- function(theta, x) {
  dst(x, a = theta[1], b = theta[2], skew = theta[3], df = theta[4], log = TRUE) %>%
    sum() %>%
    max(-.Machine$double.xmax, na.rm = TRUE) %>%
    return()
}


