#' Simulate SCC data for dairy cows
#'
#' @param n_cows Number of cows to simulate
#' @param a_mean Mean value for parameter 'a'
#' @param a_sd Standard deviation for parameter 'a'
#' @param b_mean Mean value for parameter 'b'
#' @param b_sd Standard deviation for parameter 'b'
#' @param c_mean Mean value for parameter 'c'
#' @param c_sd Standard deviation for parameter 'c'
#' @return A data frame with simulated SCC data for cows
#' @examples
#' simulate_data(10, 5, 0.1, 2, 0.2, 3, 0.3)
#' @export
simulate_data <- function(n_cows, a_mean, a_sd, b_mean, b_sd, c_mean, c_sd) {

  #library(tidyverse)
  #library(woodcurvebox)

  gen_cow_ids <- str_c("CKR", 1:n_cows)

  gen_cow_ids %>%
    map_df(function(cowID)
      simulate_woods(a = rnorm(1, a_mean, a_sd),
                     b = rnorm(1, b_mean, b_sd),
                     c = rnorm(1, c_mean, c_sd), cowID = cowID)) %>%
    identity()
}

# Example usage
# simulated_woods_df <- simulate_data(10, 5, 0.1, 2, 0.2, 3, 0.3)
