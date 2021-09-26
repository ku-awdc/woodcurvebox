#' ---
#' ---
#'
#'
source("notebooks/eda_startup.r")

#' Flipping the milk curve around the peak value $y = c = \texttt{peak value}$ and
#' such is by $2c - f(x)$.
#'
#'
#' - [ ] Figure out a reasonable scaling between

tribble(
  ~x,    ~y,
   50,    25.4,
  800,    23.4
) %>%
  lm(y ~ x - 1, data = .) %>%
  # lm(x ~ 1 + y, data = .) %>%
  summary()
#'
#' This output is not useful. But it comes from somewhere, so maybe it can
#' be fixed some day.
#'
#'
#
#' Title
#'
#' @param x
#' @param y
#' @param .f
#' @param y_center
#'
#' @return
#' @export
#'
#' @examples
flip_curve_y <- function(x, y, .f, y_center) {
  stopifnot("provide either numeric `y` or function `.f`" =
              xor(missing(y), missing(.y)))


  if (missing(y)) {
    2 * y_center - .f(x)
  } else {
    2 * y_center - y
  }
}
