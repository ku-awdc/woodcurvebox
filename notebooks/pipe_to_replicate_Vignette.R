# SCC curves
# Wood's style function vs Wilmink style function

# Code example with simulated data for the paper:
# " Using register data to identify individual dairy cows with abnormal patterns in routinely recorded somatic cell counts"

# Corresponding author:
# Maj Beldring Henningsen
# majbh@sund.ku.dk

# June 2023, Copenhagen, Denmark

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(nls.multstart)
library(nlme)
library(pbapply)


# Simulating Dummy data --------------------------------------------------------

# Four parameter Wilmink function modified for SCC:
daily_lactation_f <- function(dim, loga, b, k, d) {
  loga + b * dim + exp(-(exp(k)) * dim) * d
}

# Simulation function 28 days between each observation
simulate_f <- function(herd, animals, interval, loga, b, k, d, resid)
{
  obs <- ceiling(305 / interval)
  tibble(herd = rep(herd, animals), animal = 1:animals, dim = sample(5:(5 + interval), animals, TRUE)) |>
    group_by(herd, animal) |>
    summarise(dim = seq(dim, dim + (interval * obs), by = interval), obsnr = (0:obs) + 1, .groups = "drop") |>
    filter(dim <= 305) |>
    mutate(mean = daily_lactation_f(dim, loga, b, k, d)) |>
    mutate(logSCC = rnorm(n(), mean, resid))
}

# including 3 herds in the data each with different a, b, k, d parameters.
herd1 <- simulate_f("herd1", 10, 28, 3.64, 0.00317, -2.25, 2.12, 0.15)
herd2 <- simulate_f("herd2", 10, 28, 3.61, 0.00323, -2.25, 2.13, 0.15)
herd3 <- simulate_f("herd3", 10, 28, 3.68, 0.00315, -2.28, 2.15, 0.15)

herds <- bind_rows(herd1, herd2, herd3)

# visualize data
ggplot(herds, aes(x = dim, y = logSCC, col = herd)) +
  geom_point()


# Define functions  -----------------------------------------

f_woods <- function(dim, loga, b, k) {
  loga + b * log(dim) + (-k * dim)
}

f_wilmink <- function(dim, a, b, k) {
  a + b * dim + k * exp(-0.5 * dim)
}


# nls multistart wilmink ------------------

nls_woods <- nls.multstart::nls_multstart(logSCC ~ f_woods(dim, loga, b, k),
                                              data = herds,
                                              # lower = c(loga = -Inf, b = -Inf, c = -5),
                                              # upper = c(a = 9, b = 1.5, c = 5),
                                              start_lower = c(loga = -100, b = -100, k = -100),
                                              start_upper = c(loga = 100, b = 100, k = 100),
                                              iter = 500,
                                              supp_errors = "Y")
    
nls_wilmink <- nls.multstart::nls_multstart(logSCC ~ f_wilmink(dim, a, b, k),
                                                data = herds,
                                                # lower = c(loga = -Inf, b = -Inf, c = -5),
                                                # upper = c(a = 9, b = 1.5, c = 5),
                                                start_lower = c(a = -100, b = -100, k = -100),
                                                start_upper = c(a = 100, b = 100, k = 100),
                                                iter = 500,
                                                supp_errors = "Y")
    
herds$Prediction_nls_wood <- predict(nls_woods)
herds$Residuals_nls_wood <- residuals(nls_woods)
herds$Prediction_nls_wil <- predict(nls_wilmink)
herds$Residuals_nls_wil <- residuals(nls_wilmink)
    
nls_start_woods <- coef(nls_woods)
nls_start_wilmink <- coef(nls_wilmink)
nls_start_woods <- as_tibble(coef(nls_woods))
nls_start_wilmink <- as_tibble(coef(nls_wilmink))
    

# NLME fit using NLS coef. as start ---------------------------------

nlme_woods <- nlme(logSCC ~ f_woods(dim, loga, b, k),
                       data = herds,
                       fixed = loga + b + k ~ 1,
                       random = loga + b + k ~ 1,
                       groups = ~animal,
                       start = nls_start_woods$value,
                       na.action = na.exclude,
                       control = list(maxIter = 1200, msMaxIter = 1200))
    
nlme_wilmink <- nlme(logSCC ~ f_wilmink(dim, a, b, k),
                         data = herds,
                         fixed = a + b + k ~ 1,
                         random = a + b + k ~ 1,
                         groups = ~animal,
                         start = nls_start_wilmink$value,
                         na.action = na.exclude,
                         control = list(maxIter = 1200, msMaxIter = 1200))
    
herds$Prediction_woods <- predict(nlme_woods)
herds$Residuals_woods <- residuals(nlme_woods)
herds$Prediction_wilmink <- predict(nlme_wilmink)
herds$Residuals_wilmink <- residuals(nlme_wilmink)
  

# create output data ----------------------------------------------------------
herds |>
  pivot_longer(Prediction_nls_wood:Residuals_wilmink, names_to = "Parameter", values_to = "Value") |>
  filter(Parameter %in% c("Residuals_woods", "Residuals_wilmink")) |>
  ungroup() |>
  group_by(herd, animal, Parameter) |>
  summarise(MSQ = mean(Value^2)) |>
  ungroup() ->
  animal_data



# Visualize MSR, Wood's vs Wilmink --------------------------------------------

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73","#CC79A7", "#F0E442", "#0072B2", "#D55E00")

ggplot(animal_data, aes(x=herd, y=MSQ, fill=Parameter)) +
  geom_boxplot() +
  labs(y= "MSR") +
  theme(axis.text = element_text(size = 22),
        axis.title = element_text(size = 22),
        text = element_text(size = 22)) +
  scale_y_continuous(trans="log10") +
  scale_fill_manual(values=cbbPalette, name = "Function Style", labels = c("Wilmink", "Wood's"))

