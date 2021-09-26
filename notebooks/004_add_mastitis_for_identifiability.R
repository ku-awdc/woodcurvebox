#'
#'

source("notebooks/eda_startup.r")
# library(truncnorm)
devtools::load_all()

#'
#' The purpose here is to investigate if we can somewhat recover the true parameters when the SCC
#' count is inflated due to Mastitis incursion.
#'
#' There are many things we could control for:
#'
#' - Severity of the mastitis (10k - 40k in the mode)
#'   According to Maya, the range should more be (40k - 200k in the mode)
#' - Proportion of infected dairy cows in the herd.
#' - Number of samples from the given lactation period.
#' - Number of samples from the "three" phases of the lactation period.
#'
#


#' Title
#'
#' @param peak_location In Wood's Model, that is `b/c`, but it could be something else?
#'
#' @return Breaks corresponding to the phase that the lactation period is currently in.
#'
#' @examples
lactation_phases <- function(peak_location) {
  early <- peak_location
  mid <- 2 * peak_location
  late <- 305 - mid
  c(early = early, mid = mid, late = late)
}

#'
#' Let us introduce the "extra" SCC numbers that
#' are adjurned due to mastitis incursion
#'

# 5 = (shape-1)*scale
# 0 = qgamma(p = 0.95, shape = shape, scale = scale)

# TODO: Try with different starting points and examine the resulting curves.

find_mastititis_shapes

mastitis_shape_par <-
  find_mastititis_shapes()
shape <- mastitis_shape_par$shape
scale <- mastitis_shape_par$scale


# VALIDATION CODE
plot.new()
curve(dgamma(x, shape = shape, scale = scale), xlim = c(0, 21))

# VALIDATION: Decay after 21 day should be steep
# curve(pgamma(x, shape = shape, scale = scale, lower.tail = FALSE),
#       # xlim = c(0, 21))
#       xlim = c(0, 50))
mastitis_shape


tibble(
  severity_scale = seq.default(0, 1, length.out = 4),
  n = 100
) %>%
  mutate(
    mastitis_shape = severity_scale %>%
      map(mastitis_shape,
        n = 100,
        mastitis_shape_par = mastitis_shape_par
      )
  ) %>%
  unnest_wider(mastitis_shape) %>%
  unnest(c(x, y)) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(x, y, group = severity_scale, color = severity_scale) +
      # aes(x, mastitis_shape) +
      geom_line() +
      scale_color_viridis_c()
  }
#'
#'
#' This now looks like the proposed extra SCC that is incurred due to mastitis.
#'















# gammainc <- expint::gammainc
# gammainc <- expint:gammainc

#
# readr::read_table(
#   locale = readr::locale("da", encoding = "utf8")
# )