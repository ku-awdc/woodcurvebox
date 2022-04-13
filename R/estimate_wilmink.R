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
#' estimate_wilmink(data)
#'
#' # Multiple animals:
#' library(tidyverse)
#' data <- lapply(1:5, function(x) simulate_woods(cowID=x)) %>% bind_rows()
#' estimate_wilmink(data)
#'
#' @export
estimate_wilmink <- function(data){

  f_wilmink <- function(DIM, a,b,k,d){
    a + b * DIM + exp(-(exp(k)) * DIM)*d
  }

  stopifnot(is.data.frame(data), all(c("cowID","DIM","logSCC") %in% names(data)))

  # nls multistart (not grouped herd level):
  # repeat for all 6 diff. datasets
  nls_oh <- nls_multstart(logSCC ~ f_wilmink(DIM, a, b, k, d),
                                         data = data,
                                         #                                         lower=c(a=0, b=0, k=-5, d=0),
                                         #                                         upper=c(a=9, b=1.5, k=0, d=5),
                                         start_lower = c(a=0, b=0, k=-5, d=0),
                                         start_upper = c(a=8, b=1, k=-0.01, d=4),
                                         iter = 500,
                                         supp_errors = "Y")

  if(length(unique(data[["cowID"]]))==1){
    cat("Returning single animal model\n")
    #return(coef(nls_oh))
    return(nls_oh)
  }


  nlme_oh <- nlme(logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
                  data=data,
                  fixed=a+b+k+d~1,
                  random=a+b+k+d~1,
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