
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
    quantile   = gh_hoaglin1985(x)
  ) -> out
  
  out$call <- match.call()
  
  return(out)
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



#' @rdname gh
#' @export
print.ghfit <- function(x, ...) {
  cat('\nCall:\n')
  print(x$call)
  cat('\nPoint estimates:\n')
  print(x$estimate)
  
  # output
  invisible(x)
}



#' @rdname gh
#' @export
coef.ghfit <- function(object, ...) { object$estimate }

