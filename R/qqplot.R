
#' Draw a Q-Q plot based on g-and-h distribution
#' 
#' Draw a quantile-quantile plot based on the Tukey's g-and-h distribution.
#' 
#' @param x either data as a `numeric` vector, or an object of class `fitGH`,
#'   as returned by [fitGH()].
#' @param theta parameters of the g-and-h distribution as a `numeric` vector of
#'   length four: \eqn{(a, b, g, h)}. If argument `x` is a `fitGH` object,
#'   and argument `theta` is `NULL`, `theta` will be initialised with `coef(x)`.
#' @param qqline if `TRUE` (default) a Q-Q line will be added to the graph by
#'   means of [qqline()]
#' @param grid if `TRUE` (default) a Q-Q line will be added to the graph by
#'   means of [grid()].
#' @param ... other arguments passed to [qqplot()].
#' 
#' @return
#' A named list with the following components:
#' * `teo_quantile`: theoretical quantile function (with argument `p`)
#' * `qqplot`: output of function [qqplot()]
#' 
#' @examples
#' data("EPWS2014")
#' modII <- fitGH(EPWS2014)
#' qqgh(modII)
#' 
#' @export
qqgh <- function(x, theta = NULL, qqline = TRUE, grid = TRUE, ...) {
  
  # Initialisation
  if (inherits(x, 'fitGH') & is.null(theta)) {
    theta <- stats::coef(x)
    x <- x$x
  }
  
  if (is.null(theta)) { stop('distribution parameters are not specified') }
  if (is.null(names(theta))) { names(theta) <- c('a', 'b', 'g', 'h')}
  teoqgh <- function(p) { do.call('qgh', append(list(p = p), as.list(theta))) }
  n <- length(x)
  p <- ((1:n) - 0.5) / n
  out <- list(teo_quantile = teoqgh, qqplot = NULL)

  # Q-Q plot
  list(
    main = 'g-and-h Q-Q plot',
    xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles',
    ylim = range(c(teoqgh(c(utils::head(p, 1), utils::tail(p, 1))), x))
  ) %>%
    utils::modifyList(list(...)) %>%
    utils::modifyList(list(x = teoqgh(p), y = x)) %>%
    do.call('qqplot', .) -> out$qqplot
  
  # Q-Q line
  if (qqline == TRUE) {
    qqline(y = x, distribution = teoqgh, col = 'red', lty = 2)
  }
  
  # Grid
  if (grid == TRUE) { grid() }
  
  # Output
  invisible(out)
}

