#' Simulate data from a Wilmink curve
#'
#' @param herd herd identifier
#' @param animals number of animals in the herd
#' @param interval interval between observations in days
#' @param loga parameter 'a' in the Wilmink curve (log-scale)
#' @param b parameter 'b' in the Wilmink curve
#' @param k parameter 'k' in the Wilmink curve
#' @param d parameter 'd' in the Wilmink curve
#' @param resid standard deviation of the random residuals
#'
#' @return simulated data on herd level with an interval of 28 DIM between observations
#'
#' @examples
#' data <- simulate_wilmink()
#'
#' @export
simulate_wilmink <- function(herd, animals, interval, loga, b, k, d, resid) {
  obs <- ceiling(305 / interval)
  tibble(
    herd = rep(herd, animals),
    animal = 1:animals,
    dim = sample(5:(5 + interval), animals, TRUE)
  ) |>
    group_by(herd, animal) |>
    summarise(
      dim = seq(dim, dim + (interval * obs), by = interval),
      obsnr = (0:obs) + 1,
      .groups = "drop"
    ) |>
    filter(dim <= 305) |>
    mutate(mean = daily_lactation(dim, loga, b, k, d)) |>
    mutate(logSCC = rnorm(n(), mean, resid))
}

#' Calculate daily lactation for a given DIM based on the Wilmink curve
#'
#' @param dim days in milk
#' @param loga parameter 'a' in the Wilmink curve (log-scale)
#' @param b parameter 'b' in the Wilmink curve
#' @param k parameter 'k' in the Wilmink curve
#' @param d parameter 'd' in the Wilmink curve
#'
#' @return daily lactation value
#'
daily_lactation <- function(dim, loga, b, k, d) {
  a <- exp(loga)
  a + b * dim + k * exp(-d * dim)
}
