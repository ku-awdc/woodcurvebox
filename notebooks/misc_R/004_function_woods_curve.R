#' Returns fit for Wood's Curve
#' 
#' 
#'
#' @param data Must contain `DIM`, `y` (as this could `SCC` or `logSCC`), and `BES_ID`.
#'
#' @return
#' @export
#'
#' @examples
fit_scc_curve <- function(data, y) {
  # y_quote = qoute(y)
  nlme::nlme(
    SCC ~ a * (DIM**b) * exp(-c * DIM),
    data = data,
    fixed = a + b + c ~ 1,
    random = a + b + c ~ 1,
    # groups = ~cow_id,
    groups = ~BES_ID,
    start = c(
      a = 150,
      b = -0.1,
      c = -0.003
    ),
    # na.action = na.exclude,
    control = list(returnObject = TRUE)
  )
}