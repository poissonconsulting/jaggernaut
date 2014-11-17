round_up <- function(x, to) {
  to <- 0.1^ceiling(to)
  to*(x%/%to + as.logical(x%%to))
}

#' @title Bayesian p-value
#'
#' @description
#' Calculates Bayesian equivalent of two-sided p-value.
#' Significance cannot be less than the number of samples.
bayesian_p_value<-function (x) {
  n <- length(x)
  d <- sum(as.integer(x >= 0))
  p <- min(d, n - d) * 2
  p <- max(p, 1)
  p <- p / n
  round_up(p, log(n + 1, base = 10))
}
