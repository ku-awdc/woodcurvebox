#' Title
#'
#' @param data
#'
#' @return
#' @importFrom nls.multstart nls_multstart
#' @importFrom nlme nlme
#'
#' @examples
#' # One animal:
#' data <- simulate_woods(5, 0.01, 0.001)
#' estimate_woods(data)
#'
#' # Multiple animals:
#' library(tidyverse)
#' data <- lapply(1:5, function(x) simulate_woods(cowID = x)) %>% bind_rows()
#' estimate_woods(data)
#'
#' FIX: Update model to run one herd, with single cowIDs based on script 013 in notebooks
#'
#' @export
estimate_woods <- function(data) {

  f_woods <- function(DIM, loga, b, k) {
    loga + b * log(DIM) + (-k * DIM)
  }

  stopifnot(is.data.frame(data), all(c("cowID", "DIM", "logSCC") %in% names(data)))

  nls_multistart_woods <- nls_multstart(
    logSCC ~ f_woods(DIM, loga, b, k),
    data = data,
    start_lower = c(loga = -100, b = -100, k = -100),
    start_upper = c(loga = 100, b = 100, k = 100),
    iter = 500,
    supp_errors = "Y"
  )

  if (length(unique(data$cowID)) == 1) {
    cat("Returning single animal model\n")
    return(nls_multistart_woods)
  }

  nls_start_woods <- coef(nls_multistart_woods) %>%
    as_tibble()

  print("Before nlme function")

  nlme_woods <- nlme(
    logSCC ~ f_woods(DIM, loga, b, k),
    data = data,
    fixed = loga + b + k ~ 1,
    random = loga + b + k ~ 1,
    groups = ~ cowID,
    start = nls_start_woods$value,
    na.action = na.exclude,
    control = list(maxIter = 1200, msMaxIter = 1200)
  )

  cat("Returning multiple animal model\n")
  return(nlme_woods)
}
