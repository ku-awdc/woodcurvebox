library(tidyverse)
# library(tidylog)
library(magrittr)
library(broom)
library(ggfortify)
library(glue)
# library(skimr)
# library(lubridate)

#FIXME: Don't export this. Don't put this as part of a package. 

# For multisession, and *apply functions, use
# library(future)
# library(future.apply)



message('Setting plot theme to classic no. 2.')
theme_set(ggpubr::theme_pubr())

if (interactive()) {
  message('Plotting happens outside of RStudio.')
  # old_device <- options(device = 'windows')
}

message('StringsAsFactors is set to `FALSE`.')
options(stringsAsFactors = FALSE)

# message("Set dark-mode theme for ggplot2") 
# theme_set(ggdark::dark_mode(force_geom_invert = TRUE))
# message("You can reset it through `theme_set(ggdark::dark_mode(force_geom_invert = TRUE))`")

table_to_slack <- function(tbl = .Last.value) {
  
  tbl %>%
    pander::pandoc.table.return(style = 'rmarkdown', 
                                split.tables = Inf, split.cells = Inf) %>%
    stringr::str_trim(.)          %T>% 
    clipr::write_clip(.) %>% 
    invisible(.)
  message('Copied to clipboard.')
}

last_to_screen <- function(tbl = .Last.value) {
  screen_display <<- tbl
  invisible(tbl)
}

# NOT IMPLEMENTED
# to_screen <- function(left = NULL, right = NULL) {
#   
#   
# }

message("Defined `c.factor`, that combines factors in an intuitive manner.")
c.factor <-
  function(..., recursive = TRUE) {
    unlist(list(...), recursive = recursive)
  }
