#'
#'
#'
source("notebooks/eda_startup.r")
devtools::load_all()
library(saemix)

#TODO: Use a generated dataset to play around with this method
#
cattle_data <-
  entire_herd_scc_curves %>%
  sample_n(size = 50, replace = FALSE) %>%
  identity()
#'
#' `saemix` package fails to converge at all if we use
#'
#'
#' Obviously this needs to be subset a little bit...
#'
cattle_data <- cattle_data %>%
  unnest((c(x,y))) %>%
  group_by(cow_id) %>%
  sample_n(size = 10, replace = FALSE) %>%
  ungroup()
#'
#'
#' Let's add some noise to this
#'
cattle_data <- cattle_data %>%
  mutate(y_orig = y) %>%
  mutate(y = y + rnorm(n(), mean = 0, sd = 5)) %>%
  # mutate(y = y + rnorm(n(), mean = 0, sd = 1)) %>%
  identity()
#'
#' There's a book that we will follow here and hopefully it would eventually
#' match our use-case.
#'
cattle_data <-
  cattle_data %>%
  mutate(
    y = as.numeric(y),
    x = as.numeric(x),
    cow_id = as.character(cow_id))



saemix_cattle_data <-
  saemixData(
    name.data = cattle_data,
    header = TRUE,
    sep = " ",
    na  = NA,
    name.group = "cow_id",
    name.predictors = c("x"),
    name.response = "y",
    name.X = "x",
    # name.covariates = c(),
    # name.genetic.covariates = c(),
    # name.mdv = "",
    # name.cens = "",
    # name.occ = "",
    # name.ytype = "",
    units = list(x = "days", y = "#", covariates = c()),
    verbose = TRUE
  )
#'
#'
woods_curve_model <- function(psi, id, xidep) {
  dim <- xidep[,1]

  a <- psi[id, 1]
  b <- psi[id, 2]
  c <- psi[id, 3]

  y_pred <- a * (dim ** b) * exp(-c*dim)
  return(y_pred)
}
#'
#'
#' And now defining the model object
#'


saemix.model <- saemixModel(
  model = woods_curve_model,
  description = "somatic_cell_counts",
  # type = "structural",
  # psi0 = matrix(
  #   c(1, 7, 1, 0, 0, 0),
  #   ncol = 3,
  #   byrow = TRUE,
  #   dimnames = list(NULL, c("ka", "V", "k"))
  # ),
  # psi0 = c(
  #   a = 150,
  #   b = -0.1,
  #   c = -0.003
  # ),
  psi0 = matrix(rep(c(
    a = 150,
    b = -0.1,
    c = -0.003,0,0,0
  ),1), ncol =3,byrow = TRUE) %>%
    set_colnames(c("a","b", "c")),
  #' this assumes all are log-normal
  # transform.par = c(1, 0, 0)
  transform.par = c(0, 0, 0)
  # omega.init = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), ncol =
  #                       3, byrow = TRUE),
  # covariance.model = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), ncol =
  #                             3,
  #                           byrow = TRUE)
)
#
# saemix::initialiseMainAlgo(saemix_cattle_data,
#                            saemix.model,
#                            saemix.options = options)
#'
# data(package = "saemix")
#'
#'
#'
K1 = 200
K2 = 100
#' Run SAEM

options <- list(
  seed = 39546,
  map = TRUE,
  fim = TRUE,
  # ll.is = FALSE,
  nbiter.mcmc = c(2, 2, 2),
  nbiter.saemix = c(K1, K2),
  nbiter.sa = 0,
  displayProgress = TRUE,
  save.graphs = FALSE,
  nbiter.burn = 0,
  warnings = TRUE
)

fit <- saemix(model = saemix.model,
              data = saemix_cattle_data,
              control = options)
#'
fit %>%
  saemix::coef.SaemixObject()
#'
fit %>%
  class()
#'
methods(class = "SaemixObject")
#'
coef(fit)
psi(fit)
fitted(fit)
eta(fit)
initialize(saemix.model)
#'
saemix::coef.SaemixObject(fit)
#'
cattle_data

methods(class = "saemix")
# saemix::saemix
