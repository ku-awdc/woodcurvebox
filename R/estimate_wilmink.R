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
#' data <- simulate_wilmink(5, 0.01, 0.001)
#' estimate_wilmink(data)
#'
#' # Multiple animals:
#' library(tidyverse)
#' data <- lapply(1:5, function(x) simulate_wilmink(cowID=x)) %>% bind_rows()
#' estimate_wilmink(data)
#'
#' FIX: Update model to run one herd, with single cowIDs based on script 013 in notebooks
#'
#' @export
estimate_wilmink <- function(data) {

  f_wilmink <- function(DIM, a, b, k) {
    a + b * DIM + k * exp(-0.5 * DIM)
  }

  stopifnot(is.data.frame(data), all(c("cowID", "dim", "logSCC") %in% names(data)))

  nls_multistart_wilmink <- nls_multstart(
    logSCC ~ f_wilmink(dim, a, b, k),
    data = data,
    start_lower = c(a = -100, b = -100, k = -100),
    start_upper = c(a = 100, b = 100, k = 100),
    iter = 500,
    supp_errors = "Y"
  )

  if (length(unique(data$cowID)) == 1) {
    cat("Returning single animal model\n")
    return(nls_multistart_wilmink)
  }

  nls_start_wilmink <- coef(nls_multistart_wilmink) %>%
    as_tibble()

  nlme_wilmink <- nlme(
    logSCC ~ f_wilmink(DIM, a, b, k),
    data = data,
    fixed = a + b + k ~ 1,
    random = a + b + k ~ 1,
    groups = ~ cowID,
    start = nls_start_wilmink$value,
    na.action = na.exclude,
    control = list(maxIter = 1200, msMaxIter = 1200)
  )

  cat("Returning multiple animal model\n")
  return(nlme_wilmink)
}
