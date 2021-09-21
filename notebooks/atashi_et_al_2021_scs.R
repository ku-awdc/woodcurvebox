#'
#'
source("../asf_phd/R/eda_startup.r")
#'
#' Source: [Link](https://www.mdpi.com/2076-2615/11/6/1637#supplementary)
#'
# FIRST APPROACH
# r"(C:\Users\tpb398\Downloads\SCS)" %>%
#   dir(full.names = TRUE) %>%
#   enframe() %>%
#   mutate(name = value %>%
#            map_chr(. %>%
#                      fs::path_file() %>%
#                      fs::path_ext_remove())) %>%
#   mutate(df = value %>%
#            map(read_delim)) %>%
#   select(-value) %>%
#   base::split(factor(.$name)) %>%
#   map(. %>% extract2("df")) %>%
#   flatten() ->
#   datasets
#'
datasets <- list()
read_delim(
  "C:/Users/tpb398/Downloads/SCS/data.txt",
  delim = "\t",
  col_types = cols(
    reg = col_character(),
    tdate = col_character(),
    parity = col_double(),
    fatper = col_double(),
    proper = col_double(),
    milk = col_double(),
    scs = col_double(),
    pro = col_double(),
    fat = col_double(),
    x01 = col_double(),
    x11 = col_double(),
    x21 = col_double(),
    x31 = col_double(),
    x02 = col_double(),
    x12 = col_double(),
    x22 = col_double(),
    x32 = col_double(),
    x03 = col_double(),
    x13 = col_double(),
    x23 = col_double(),
    x33 = col_double(),
    AS = col_double()
  )
) -> datasets$data
# datasets$data %>%
#   summarise(across(everything(), list(range = range,
#                                       anyNA = anyNA))) %>%
#   pivot_longer(everything(),
#                names_sep = "_",
#                names_to = c("variable", ".value"))
#'
#' Reverse formula.
#'
datasets$data %>%
  # mutate(scc = (2**(scs - 3)) * 100e3) %>%
  mutate(scc = (exp(scs - 3)) * 100e3) %>%
  summarise(
    scs_range = range(scs),
    scc_range = range(scc))
#' Something is clearly wrong. Maybe the formula is incorrect.
#'
#'
datasets$data %>%
  naniar::miss_var_table()
#'
#' Nothing is missing.
#'
#'
datasets$data %>%
  head(25) %>% View()

datasets$data %>%
  count(reg) %>%
  arrange(desc(n))
#'
#'
datasets$data %>%
  filter(reg == "275908088") %>%
  identity() %>% {
    ggplot(.) +
      aes()
  }
  View()
