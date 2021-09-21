#'
#'
#' @param c
#' @param a
#' @param d
#' @param DIM Days in milk, `[0, 305]` typically and is regarded as the $x$.
f_wilmink <- function(a, b, c, d, DIM) {
  a * DIM **(b) * exp(-exp(c) * DIM * d)
}
#'
#'
#'
#'
