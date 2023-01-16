#' @title Wilmink's Curve function
#'
#' @param a,b,c,d Parameters for Wilmink's Curve
#' @param DIM Days in milk, `[0, 305]` typically and is regarded as the $x$.
#'
#' @return `log(SCC)`
#'
f_wilmink <- function(a, b, d, k, DIM) {
  # a * DIM **(b) * exp(-exp(c) * DIM * d)
  a * DIM **(b) * exp(-exp(c) * DIM * d)

  a + b * DIM + d * exp(-exp(k) * DIM)
}
#'
#'
#'
#'
