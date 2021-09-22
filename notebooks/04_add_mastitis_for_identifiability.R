#'
#'

source("../asf_phd/R/eda_startup.r")
# library(truncnorm)
devtools::load_all()

#'
#' The purpose here is to investigate if we can somewhat recover the true parameters when the SCC
#' count is inflated due to Mastitis incursion.
#'
#' There are many things we could control for:
#'
#' - Severity of the mastitis (10k - 40k in the mode)
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
  early = peak_location
  mid = 2 * peak_location
  late = 305 - mid
  c(early = early, mid = mid, late = late)
}

#'
#' Let us introduce the "extra" SCC numbers that
#' are adjurned due to mastitis incursion
#'

# 5 = (shape-1)*scale
# 0 = qgamma(p = 0.95, shape = shape, scale = scale)

#' Title
#'
#' @param par
#'
#' @return
#' @export
#'
#' @note The `mode` in the output should be the same as the target
#' mode provided.
#'
#' @examples
find_mastititis_shapes <- function(par = c(1,1)) {

  optim(par = par,
        function(x) {

          if (any(x < 0)) {
            Inf
          }

          shape <- x[1]
          scale <- x[2]
          # replace shape by constrain that the mode is at 5 (i.e. `target_mode`)
          better_shape <- 5 / scale + 1

          # multiplication to bring the terms into the same level

          shape * pgamma(21, shape = shape, scale = scale,
                         lower.tail = FALSE) +
            abs(shape - better_shape) * scale +
            # mean(abs(1 - pgamma(21, shape = shape, scale = scale))) +
            0
        }) %>%
    `[[`("par") ->
    mastitis_shape_par
  shape <- mastitis_shape_par[1]
  scale <- mastitis_shape_par[2]
  list(shape = shape, scale = scale,
       mode = (shape - 1)*scale)
}

#TODO: Try with different starting points and examine the resulting curves.

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

#' Title
#'
#' @param severity_scale Between `c(0, 1)`
#' @param mastitis_shape_par
#' @param n
#' @param x
#'
#' @return
#' @export
#'
#' @examples
mastitis_shape <- function(
  severity_scale,
  n = 100,
  x = NULL,
  mastitis_shape_par = mastitis_shape_par) {
  # assume `x` is within 0 and 21

  x <- if (is.null(x)) {
    seq.default(0, 21, length.out = n)
  } else {
    x
  }

  shape <- mastitis_shape_par$shape
  scale <- mastitis_shape_par$scale
  shape_value <- dgamma(x, shape = shape, scale = scale) /
    dgamma(5, shape = shape, scale = scale)

  list(
    # severity_scale = severity_scale,
    x = x,
    # mastitis_shape = shape_value,
    y = (1 - severity_scale) * 10 * shape_value +
      severity_scale * 40 * shape_value
  )
}
# NOT POSSIBLE WITH LIST AS OUTPUT
# mastitis_shape <- Vectorize(mastitis_shape,
#                             vectorize.args = "severity_scale")


tibble(
  severity_scale = seq.default(0, 1, length.out = 4),
  n = 100
) %>%
  mutate(
    mastitis_shape = severity_scale %>%
      map(mastitis_shape,
          n = 100,
          mastitis_shape_par = mastitis_shape_par)
  ) %>%
  unnest_wider(mastitis_shape) %>%
  unnest(c(x,y)) %>%
  identity() %>% {
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

gammainc <- expint::gammainc

#
# readr::read_table(
#   locale = readr::locale("da", encoding = "utf8")
# )
