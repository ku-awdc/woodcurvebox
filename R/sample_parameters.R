#' Let us define a "parameter sampler"
#'
#' @param n
sample_parameters <- function(n = 1) {
  list(
    #' `a` must be non-negative
    a = runif(n = n, min =  0, max = 2),
    b = runif(n = n, min = 0, max = 2.5),
    # `c` should be non-negative
    c = -log(runif(n = n, max = 2.4)),
    # c = -log(runif(n = n, max = 2.4)),
    #' `d` must be non-negative
    d = runif(n = n, min = 0, max = 4.4)
  )
}
