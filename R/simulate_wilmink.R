#' Simulate data from a wilmink curve
#'
#' @param a desc
#' @param b desc
#' @param k desc
#' @param d desc
#' @param dim which DIM do we want?
#'
#' @return simulated data on herd level with an interval of 28 DIM between osbervations
#'
#' @examples
#' data <- simulate_wilmink()
#'
#' @export
simulate_wilmink <- function(herd, animals, interval, a, b, k, d, resid)
{
  obs <- ceiling(305 / interval)
  tibble(herd=herd, animal = 1:animals, dim = sample(5:(5+interval), animals, TRUE)) |>
    group_by(herd, animal) |>
    summarise(dim = seq(dim, dim+(interval*obs), by=interval), obsnr=(0:obs)+1, .groups="drop") |>
    filter(dim <= 305) |>
    mutate(mean = daily_lactation_f(dim, a, b, k, d)) |>
    mutate(logSCC = rnorm(n(), mean, resid))
}
