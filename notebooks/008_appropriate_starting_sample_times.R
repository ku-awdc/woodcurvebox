#'
#'
#'
#' Starting sample dates has to be random, but the sampling times should be
#' equidistant.
#'
#'
#' In a 305 days period, getting 10 points translates to one measurement
#' a month
library(lubridate)

max_days_in_milk <- 305

#'
#'
#' Number of days between sampling
ns <- c(
  daily = days(),
  2*days(),
  5*days(),
  weekly = weeks(),
  monthly = lubridate::dmonths()
  # 1.5 * dmonths()
) / days()
ns <- ns %>% floor()
#'
#' But then the starting time should be stochastic

#
#' Starting sampling time
#'
#' @param n Distance between sampling times
#' @inheritParams base::sample.int size
#' @return
#' @export
#'
#' @examples
starting_time <- function(n, size = 1) {
  sample.int(size = size, n = round(1.5 * n))
}



#'  Let us look at the entire space
# tibble(ns) %>%
tibble(ns = 30) %>%
  #' all possible starting times
  mutate(starting_time = ns %>%
           map(~seq.default(from = 1, to = round(1.1 * .x)))) %>%
  unnest(starting_time) %>%
  arrange(n = ns, starting_time) %>%

  mutate(sampling_times =
           starting_time %>%
           map2(ns, ~seq.default(from = .x,
                                 to = max_days_in_milk, by = .y)),
         number_of_samples = sampling_times %>% lengths()) %>%

  View() %>%
  identity()
#'
#' There is *no way* to reasonable make this a 6-point scheme.
#'
