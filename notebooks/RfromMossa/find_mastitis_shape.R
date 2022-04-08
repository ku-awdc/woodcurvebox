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
          if (any(x <= 0)) {
            return(Inf)
          }

          shape <- x[1]
          scale <- x[2]
          # replace shape by constrain that the mode is at 5 (i.e. `target_mode`)
          better_shape <- 5 / scale + 1
          x[1] <- better_shape

          # multiplication to bring the terms into the same level
          # atan(pgamma(21, shape = shape, scale = scale, lower.tail = FALSE)) +
          # identity(pgamma(21, shape = shape, scale = scale, lower.tail = FALSE)) +
          atan(pgamma(21, shape = shape, scale = scale, lower.tail = FALSE) /
                 pgamma(21, shape = shape, scale = scale, lower.tail = TRUE)) +
            atan(abs(better_shape - shape)) +
            sum(abs(x))
        }) -> optim_output

  optim_output %>%
    `[[`("par") ->
    mastitis_shape_par

  shape <- mastitis_shape_par[1]
  scale <- mastitis_shape_par[2]
  structure(
    list(shape = shape,
         scale = scale,
         mode = (shape - 1)*scale,
         optim_output = optim_output),
    class = "mastitis_shape_parameters"
  )
}

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
  stopifnot(
    "`x` is between `c(0, 21)` days" = 0 <= x & x <= 21,
    "`n` should be positive integer" = n > 0
  )

  x <- if (is.null(x)) {
    seq.default(0, 21, length.out = n)
  } else {
    x
  }

  shape <- mastitis_shape_par$shape
  scale <- mastitis_shape_par$scale
  # ensures that the mode at 5 is 1.
  shape_value <- dgamma(x, shape = shape, scale = scale) /
    dgamma(5, shape = shape, scale = scale)

  list(
    # severity_scale = severity_scale,
    x = x,
    # mastitis_shape = shape_value,
    y = (1 - severity_scale) * 40 * shape_value +
      severity_scale * 200 * shape_value
  )
}
# NOT POSSIBLE WITH LIST AS OUTPUT
# mastitis_shape <- Vectorize(mastitis_shape,
#                             vectorize.args = "severity_scale")

#'
#'
#'
#' @param x
#' @param n
#' @inheritDotParams ggplot2 autoplot
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.mastitis_shape_parameters <- function(x, n = 100, ...) {
  # n <- 100
  shape <- x$shape
  scale <- x$scale
  ts <- seq.default(0, 21, length.out = n)
  shape_value <- dgamma(ts, shape = shape, scale = scale) /
    dgamma(5, shape = shape, scale = scale)

  ggplot2::ggplot(tibble(ts = ts, y = shape_value)) +
    ggplot2::aes(ts, y) +
    ggplot2::geom_line() +

    ggplot2::labs(caption = glue::glue("(shape, scale) = ({shape}, {scale})")) +
    ggpubr::theme_classic2() +
    NULL
}
