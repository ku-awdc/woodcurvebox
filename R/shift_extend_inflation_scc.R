#' Shift & Extend a value
#'
#' @param x
#' @param shift_by Non-negative integer representing the number of entries
#' to shift to the right.
#' @param total_length Length of the resulting value.
#'
#' @return Returns numeric that is `total_length` long with values
#' from `x` padded with `fill`
#' @export
#'
#' @examples
#'
shift_extend_inflation_scc <- function(x, shift_by,
                                       total_length = 305, fill = 0) {
  #TODO: make this work with `shift_by` negative.

  stopifnot(
    "`shift_by` must be non-negative integer" =
      shift_by >= 0 &
      all.equal.numeric(
        abs(shift_by - trunc(shift_by)), 0
      )
  )
  c(
    rep.int(fill, pmin(shift_by, total_length)),
    x[seq_len(pmin(length(x), pmax(0, total_length - shift_by)))],
    rep.int(fill, times = pmax(total_length -
                                 length(x) - shift_by, 0))
  )
}
