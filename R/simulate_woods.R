#' Simulate data from a woods curve
#'
#' @param a desc
#' @param b desc
#' @param c desc
#' @param DIM which DIM do we want?
#'
#' @return a series of simulated data from 1 cow
#'
#' @examples
#' data <- simulate_woods()
#'
#' @export
simulate_woods <- function(a = 5, b = 0.01, c = 0.001, DIM = seq(30,300,by=30), cowID = "CKR_1"){

  # FIX start param. Check: a = 150, b = -0.1, c = -0.003

  stopifnot(length(a)==1, length(b)==1, length(c)==1, length(cowID)==1)
  stopifnot(all(DIM > 0), all(DIM < 305))

  scc <- a * DIM ** b * exp(-c * DIM) + rnorm(length(DIM), mean = 0, sd = sd)

  return(data.frame(cowID = cowID, DIM=DIM, logSCC = scc))

}
