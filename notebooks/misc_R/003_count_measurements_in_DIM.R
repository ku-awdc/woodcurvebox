

#'  Med Mossa, 2021-08-04
#'
#'
#' FIX: include in data set: PCR_TEST
#'

load("K:/paperII/yktr_summarise.RData")


library(tidyverse)
df_model$PARITY %>% n_distinct()
df_model$DIM %>% n_distinct()

df_model %>% 
  group_by(PARITY, PCR_TEST, DIM, BES_ID) %>% 
  summarise(n_measurements = n(),
            ) ->
  
  count_measurements_df
#'
#'
#'
count_measurements_df %>% 
  
  identity() %>% {
    ggplot(.) + 
      aes(DIM, n_measurements) + 
      
      geom_col(aes(color = interaction(PARITY), fill = interaction(PARITY))) +
      facet_wrap(~factor(PCR_TEST), ncol = 1, scales = "free_y") + 
      labs(caption = "Count measurements") +
      NULL
  }
#'
#' Let's say that we want to count the number of measurements prior to the 
#' smallest attainable `logSCC` count. 