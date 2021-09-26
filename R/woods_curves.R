#' Daily lactation curve via Wood's curve
#'
#' @param n Days in lactation, i.e. within `c(0, 305)`
#' @param a,b,c Wood's curve parameters
#'
#' @return
#' @export
#'
#' @examples
daily_lactation_f <- function(n, a, b, c) {
  a * n ** b * exp(-c * n)
}
daily_lactation_f <- Vectorize(daily_lactation_f,
                               vectorize.args = c("a", "b", "c"))



#' Title
#'
#' @param a,b,c
#'
#' @return
#' @export
#'
#'
#'
#' @examples
#'
#' @source From [Algebraic Model of the Lactation Curve in Cattle](https://www.nature.com/articles/216164a0.pdf)
total_lactation <- function(a, b, c, T = 305) {
  # stop("not implemented")
  # a / (c ** (b + 1)) * gamma(b + 1)
  #FIXME: none of this really works
  if (c<0) {
    return(NA)
  }
  (a * T ^ b * (c * T) ^ (-b) * (gamma(1 + b) - pracma::gammainc(1 + b, c * T))) / c
}
