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
#' data <- lapply(1:5, function(x) simulate_woods(cowID=x)) %>% bind_rows()
#' estimate_woods(data)
#'
#' @export
estimate_woods <- function(data, logscale = FALSE) {

  # TODO: fix: apply the nonlogscaled Wood's
  # woods logscale: scc <- a * DIM ** b * exp(-c * DIM)
  # Woods
  if (logscale) {
    warning("log function may to be modified")
    f_woods <- function(DIM, a,b,c){
        log(a) + b * log(DIM) + (- c* DIM)
      }
  } else{
    f_woods <- function(DIM, a,b,c){
      a * DIM ** b * exp(-c * DIM)
    }
  }



  stopifnot(is.data.frame(data), all(c("cowID","DIM","logSCC") %in% names(data)))

  # nls multistart (not grouped herd level):
  # repeat for all 6 diff. datasets
  nls_oh <- nls_multstart(logSCC ~ f_woods(DIM, a, b, c),
                          data = data,
                          #                                         lower=c(a=0, b=0, c=-5),
                          #                                         upper=c(a=9, b=1.5, c=5),
                          start_lower = c(a=0, b=-1, c=-1),         # Check nonlog: a = 150, b = -0.1, c = -0.003
                          start_upper = c(a=300, b=1, c=1),
                          iter = 500,
                          supp_errors = "Y")

  if(length(unique(data[["cowID"]]))==1){
    cat("Returning single animal model\n")
    #return(coef(nls_oh))
    return(nls_oh)
  }


  nlme_oh <- nlme(logSCC ~ log(a) + b * log(DIM) + (- c* DIM),
                  data=data,
                  fixed=a+b+c~1,
                  random=a+b+c~1,
                  groups=~cowID,
                  start=coef(nls_oh),
                  na.action=na.exclude,
                  control = list(maxIter = 1200, msMaxIter = 1200))

  cat("Returning multiple animal model\n")
  return(nlme_oh)

  # TODO: remove:
  nlme_oh
  ranef(nlme_oh)

  fakedata <- expand_grid(AnimalParity = unique(one_herd$AnimalParity), DIM = 6:305)
  fakedata$prediction <- predict(nlme_oh, newdata = fakedata)

  animals <- sample(unique(one_herd$AnimalParity), 2)
  ggplot() +
    geom_line(data=fakedata %>% filter(AnimalParity %in% animals), aes(x=DIM, y=prediction, col=AnimalParity)) +
    geom_point(data=one_herd %>% filter(AnimalParity %in% animals), aes(x=DIM, y=logSCC, col=AnimalParity)) +
    theme(legend.pos = "none")



}
