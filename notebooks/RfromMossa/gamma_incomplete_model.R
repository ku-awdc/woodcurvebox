#'
#'
#' @param a
#' @param b
#' @param c
#' @param n day `n`
daily_lactation_f <- function(n, a, b, c) {
  a * n ** b * exp(-c * n)
}
#'
#'
#' @param a
#' @param b
#' @param c
#' @inheritParams daily_lactation_f
daily_lactation_df <- function(n, a, b, c) {
  #' alternative: `a exp(-c n) (b n^(-1 + b) - c n^b)`
  a * exp(-c * n) * n^(-1 + b) * (b - c * n)
}

peak_location <- function(a,b,c) {
  b/c
}
