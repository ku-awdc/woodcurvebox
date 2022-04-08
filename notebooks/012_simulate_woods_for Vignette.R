

set.seed(20220404)

# DIM = seq.default(5, 305)
#samples_per_cow <- 10
#max_start_date <- 60
#herd_size = 50


# scc <- 150 * DIM ** -0.1 * exp(-0.003 * DIM)


simulate_woods <- function(a = 5, b = 0.01, c = 0.001, sd= 1, DIM = seq(30,300,by=30), cowID = "CKR_1"){

  # FIX start param. Check: a = 150, b = -0.1, c = -0.003

  stopifnot(length(a)==1, length(b)==1, length(c)==1, length(cowID)==1)
  stopifnot(all(DIM > 0), all(DIM < 305))

  scc <- a * DIM ** b * exp(-c * DIM) + rnorm(length(DIM), mean = 0, sd = sd)

  return(data.frame(cowID = cowID, DIM=DIM, logSCC = scc))
}


gen_cow_ids <- str_c("CKR", 1:10)
gen_cow_ids %>%
  map_df(function(cowID) simulate_woods(cowID = cowID)) %>%
  identity() -> simulated_woods_df
# simulate_woods(cowID = gen_cow_ids) # doesn't work because `simulate_woods`
# does not vectorize over `cowID`

simulated_woods_df %>%
  dplyr::mutate(herdID = dplyr::if_else(str_detect(cowID, "[1-8]{1,1}$"), "A", "B")) %>%
  dplyr::group_by(herdID) %>%
  # mutate(epsilon_herd = rnorm(n = 1, mean = 0, sd = sd_herd)) %>%
  dplyr::mutate(epsilon_herd = rnorm(n = 1, mean = 0, sd = 2)) %>%


  dplyr::ungroup()





###################

library(nlme)
nlme::nlme(
  scc ~ a * (DIM**b) * exp(-c * DIM),
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

