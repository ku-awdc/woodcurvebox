
#' Title
#'
#' @param peak_location In Wood's Model, that is `b/c`, but it could be something else?
#'
#' @return Breaks corresponding to the phase that the lactation period is currently in.
#'
#' @examples
lactation_phases <- function(peak_location) {
  early = peak_location
  mid = 2 * peak_location
  late = 305 - mid
  c(early = early, mid = mid, late = late)
}
