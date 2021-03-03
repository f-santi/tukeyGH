
#' Fit the Tukey's g-and-h distribution
#' 
#' Fit...
#' 
#' @param x data.
#' @param method estimation method.
#' 
#' @export
gh <- function(x, method = c("quantile", "iinference", "mle")) {
  
  switch(match.arg(method),
    iinference = gh_iinference(x),
    mle        = gh_mle(x),
    quantile   = gh_quantile(x)
  )
}



gh_iinference <- function(x) {
  stop('to be implemented...')
}



gh_mle <- function(x) {
  stop('to be implemented...')
}



gh_quantile <- function(x) {
  stop('to be implemented...')
}



