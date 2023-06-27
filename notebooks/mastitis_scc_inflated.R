#' Title
#'
#' @param severity_scale Between `c(0, 1)`
#' @param mastitis_shape_par
#' @param n Number of points between `c(0, 21)` returned
#' @param x If specified, the returned points are based on this `x`.
#'
#' @return
#' @export
#'
#' @examples
mastitis_shape <- function(severity_scale,
                           n = 100,
                           x = NULL,
                           mastitis_shape_par = mastitis_shape_par) {
  # Note that this is developed in
  # "notebooks/04_add_mastitis_for_identifiability.R"
  #

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


#' Title
#'
#' @param par
#'
#' @return
#' @export
#'
#' @note The `mode` in the output should be the same as the target mode
#'  provided.
#'
#' @examples
find_mastititis_shapes <- function(par = c(1,1)) {
  stopifnot(
    "only provide one pair of parameters (shape, scale)" = length(par) == 2)

  optim(par = par,
        function(x) {

          if (any(x < 0)) {
            return (Inf)
          }

          shape <- x[1]
          scale <- x[2]
          # replace shape by constrain that the mode is at 5 (i.e. `target_mode`)
          better_shape <- 5 / scale + 1

          # multiplication to bring the terms into the same level

          # cat(paste(shape, scale, collapse = ", "), sep = "\n")
          tail_end_weight <-
            pgamma(21, shape = shape, scale = scale,
                   lower.tail = FALSE)
          # cat(glue("skewniss {value}", value = pmax(0, log(shape / better_shape))),
          #     sep = "\n")

          # tail_end_weight +
          atan(tail_end_weight) * scale +
            2*pi * atan(abs(better_shape - shape)) / scale
        }) %>%
    `[[`("par") ->
    mastitis_shape_par
  shape <- mastitis_shape_par[1]
  scale <- mastitis_shape_par[2]
  list(shape = shape, scale = scale,
       mode = (shape - 1)*scale)
}

