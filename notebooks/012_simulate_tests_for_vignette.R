

set.seed(20220404)

### Simulate single animals with Wood: --------------------

#dim = seq.default(5, 305)
#samples_per_cow <- 10
#max_start_date <- 60
#herd_size = 50


# scc <- 150 * DIM ** -0.1 * exp(-0.003 * DIM)


### Simulate herd with wilmink: --------------------



# including 3 herds in the data each with different a,b,k,d parameters.
herd1 <- simulate_wilmink("herd1", 10, 28, 3.64, 0.00317, -2.25, 2.12, 0.15)
herd2 <- simulate_wilmink("herd2", 10, 28, 3.61, 0.00323, -2.25, 2.13, 0.15)
herd3 <- simulate_wilmink("herd3", 10, 28, 3.68, 0.00315, -2.28, 2.15, 0.15)

herds <- bind_rows(herd1, herd2, herd3)

# visualize data
ggplot(herds, aes(x=dim, y=logSCC, col=herd)) +
  geom_point()


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

