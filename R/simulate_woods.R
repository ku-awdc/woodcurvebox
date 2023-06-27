#' Simulate data from a woods curve
#'
#' @param a desc
#' @param b desc
#' @param c desc
#' @param dim which dim do we want?
#'
#' @return a series of simulated data from 1 cow with 30 dim between observations
#'
#' @examples
#' data <- simulate_woods()
#'
#' @export
simulate_woods <- function(a = 5, b = 0.01, c = 0.001, dim = seq(30,300,by=30), cowID = "CKR_1"){
  stopifnot(length(a)==1, length(b)==1, length(c)==1, length(cowID)==1)
  stopifnot(all(dim > 0), all(dim < 305))

  scc <- a * dim ** b * exp(-c * dim)

  return(data.frame(cowID = cowID, dim=dim, logSCC = scc))
}
