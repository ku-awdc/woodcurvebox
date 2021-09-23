#'
#'
#'
#' Simulate a herd consisting of 50 cows, over a "first" lactation.
#' Then see if one can retrieve that information back
#'
source("../asf_phd/R/eda_startup.r")
library(truncnorm)
devtools::load_all()

daily_lactation_f <- function(n, a, b, c) {
  a * n ** b * exp(-c * n)
}
#'
#'
#' Generate individual cow parameters from the first lactation:
milk_curves %>%
  filter(lactation == "first") %>%
  mutate(herd = factor("only_herd"),
         herd_size = 50) %>%
  identity() %>% {
    bind_cols(
      rename(., herd_a = a, herd_b = b, herd_c = c),
      tibble(
        a = truncnorm::rtruncnorm(.$herd_size,
                                  a = 0,
                                  mean = .$a,
                                  sd = 2),
        b = truncnorm::rtruncnorm(
          .$herd_size,
          a = 0,
          mean = .$b,
          sd = 0.1
        ),
        c = truncnorm::rtruncnorm(
          .$herd_size,
          a = 0, mean = .$c,
          sd = 0.001
          # sd = 0.005
        )
      ) %>%
        rename_with(~str_c("cow_", .x)) %>%
        rowid_to_column("cow_id") %>%
        mutate(cow_id = as.character(cow_id) %>%
        # mutate(cow_id = str_c("cow_", cow_id) %>%
                 fct_inorder())
    )
  } %>%
  #' flipping
  mutate(across(ends_with("_a"),
                ~ if_else(.x < 0, .x, -.x))) %>%
  last_to_screen() ->
  herd_df
#'
herd_df %>%
  pivot_longer(cols = c(cow_a, cow_b, cow_c)) %>%

  identity() %>% {
    ggplot(.) +
      aes(value, group = interaction(name)) +
      geom_density() +
      geom_rug(
        data = . %>%
          group_by(name, lactation, herd) %>%
          slice(1) %>%
          pivot_longer(cols = c(herd_a, herd_b, herd_c),
                       names_to = "herd_name",
                       values_to = "herd_value"),
        aes(herd_value)) +
      facet_wrap(~name, ncol = 1, scales = "free") +
      NULL
  }
#'
#'
herd_df %>%
  glimpse()
#'
#'
herd_full_df <- herd_df %>%
  expand_grid(
    DIM = seq.default(5, 305)
  ) %>%
  mutate(daily_lactation_f  =
           pmap_dbl(select(., n = DIM, a = cow_a, b = cow_b, c = cow_c),
                    daily_lactation_f))
#'
herd_level_curve_df <- herd_df %>%
  slice(1) %>%
  select(-cow_a, -cow_b, -cow_c, -cow_id) %>%
  expand_grid(
    DIM = seq.default(5, 305)
  ) %>%
  mutate(daily_lactation_f  =
           pmap_dbl(select(., n = DIM, a = herd_a, b = herd_b, c = herd_c),
                    daily_lactation_f))
#'
#'
herd_full_df %>%

  identity() %>% {
    ggplot(.) +
      aes(DIM, daily_lactation_f) +
      geom_line(aes(color = cow_id,
                    group = interaction(cow_id)),
                alpha = .5,
                show.legend = FALSE) +

      geom_line(aes(color = "herd-level"),
                data = herd_level_curve_df,
                linetype = "dashed",
                size = 2,
                show.legend = FALSE) +
      scale_color_viridis_d() +
      NULL
  }
#' The dashed line is the herd-level curve.
#'
#' Now, let us select different starting days
#'
samples_per_cow <- 10
max_start_date <- 60
#'
#' Here one can test if there are a proportion of cows that are logged
#' after their inherent peak.
#'
tibble(
  cow_id = herd_full_df$cow_id %>% unique() %>% sort(),
  start_date =
    sample.int(replace = TRUE,
               size = length(cow_id),
               n = 60),
  #' SCHEDULED
  dim_sample = map(start_date, . %>%
                     seq.default(from = .x, to = 305,
                                 length.out = samples_per_cow) %>%
                     round() %>%
                     trunc() %>%
                     unique()
  # RANDOM
  # dim_sample = map(start_date, ~
  #     sample(seq.default(.x, 305, by = 1),
  #            size = samples_per_cow,
  #            replace = FALSE)
  )) %>%
  unnest(dim_sample) %>%
  # print(n = Inf)
  identity() ->
  sampled_per_cow_df
#'
#'
#'
herd_full_df %>%
  semi_join(
  # anti_join(
    sampled_per_cow_df,
    by = c("cow_id", c("DIM" = "dim_sample"))
  ) ->
  sampled_herd_full_df
#'
#'
#'
sampled_herd_full_df %>%
  select(-herd_a, -herd_b, -herd_c, -herd_size) %>%

  # DEBUG
  # count(cow_id) %>%
  # print(n=Inf)


  identity() %>% {
    ggplot(.) +
      aes(DIM, daily_lactation_f) +
      geom_line(aes(color = cow_id,
                    group = interaction(cow_id)),
                alpha = .5,
                show.legend = FALSE) +
      geom_point(aes(color = cow_id,
                     group = interaction(cow_id)),
                 alpha = .5,
                 show.legend = FALSE) +
      geom_line(aes(color = "herd-level"),
                data = herd_level_curve_df,
                linetype = "dashed",
                size = 2,
                show.legend = FALSE) +
      scale_color_viridis_d() +
      NULL
  }
#'
sampled_herd_full_df
#'
sampled_herd_full_df %>%
  # mutate(daily_lactation_f = daily_lactation_f +
  #          min(daily_lactation_f)) %>%\
  mutate(daily_lactation_f = daily_lactation_f + rnorm(n(), sd = 2)) ->
  sampled_herd_full_df
#' Plot after residual noise has been added
#'
#'
sampled_herd_full_df %>%
  select(-herd_a, -herd_b, -herd_c, -herd_size) %>%

  # DEBUG
  # count(cow_id) %>%
  # print(n=Inf)


  identity() %>% {
    ggplot(.) +
      aes(DIM, daily_lactation_f) +
      geom_line(aes(color = cow_id,
                    group = interaction(cow_id)),
                alpha = .5,
                show.legend = FALSE) +
      geom_point(aes(color = cow_id,
                     group = interaction(cow_id)),
                 alpha = .5,
                 show.legend = FALSE) +
      geom_line(aes(color = "herd-level"),
                data = herd_level_curve_df,
                linetype = "dashed",
                size = 2,
                show.legend = FALSE) +
      scale_color_viridis_d() +
      NULL
  }
#'
#'
#'
#'

sampled_herd_full_df %>%
  #' ensure that the rows are sorted by `cow_id`
  nlme::nlme(
    # logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
    daily_lactation_f  ~ a * (DIM ** b) * exp(-c * DIM),
    data = .,
    fixed = a + b + c ~ 1,
    random = a + b + c ~ 1,
    groups =  ~ cow_id,
    start = c(
      a = -25,
      b = 0.02,
      c = 0.006
    ),
    na.action = na.exclude,
    control = list(maxIter = 1200, msMaxIter = 1200)
  ) -> cow_level_model_output
#'
bind_rows(
  estimate = cow_level_model_output %>%
    nlme:::coef.lme(omitGroupingFactor = FALSE) %>%
    as_tibble() %>%
    #' now that this needs to be checked..
    rownames_to_column("cow_id"),
  true =
    herd_df %>%
    select(cow_id, a = cow_a, b = cow_b, c = cow_c),
  .id = "source"
) %>%

  #' FIXME: The estimate contains a negative sign from the
  #' curve

  pivot_longer(c(a,b,c), names_to = "parameter",
               values_to = "value") %>%
  pivot_wider(names_from = source) ->
  parameter_diff_df

parameter_diff_df %>%
  identity() %>%  {
    ggplot(.) +

      aes(estimate, true,
          color = parameter,
          group = interaction(parameter)) +
      geom_point() + geom_line() +

      geom_abline(slope = 1, intercept = 0, aes(color = "identity"))+

      facet_wrap(~parameter, ncol = 1, scales = "free")
  }


parameter_diff_df %>%
  group_by(parameter) %>%

  summarise(error = estimate - true,
            abs_error = abs(error)) %>%
  pivot_longer(-parameter) %>%
  mutate(name = fct_inorder(name)) %>%
  identity() %>% {
    ggplot(.) +
      aes(value, group = interaction(name, parameter)) +
      geom_density(aes(fill = parameter), alpha = 0.5) +

      facet_wrap(parameter~name, ncol = 2, scales = "free") +
      NULL
  }
#'
#'
#'
#'
#'
#'   pivot_wider(values_from = c(a,b,c), names_from = source) %>%
#'
#'   #' I don't know what this is...
#'   # pivot_longer(c(a,b,c), names_to = "parameter",
#'   #              values_to = "value") %>%
#'   # identity() %>% {
#'   #   ggplot(.) +
#'   #     aes(group = interaction(source, cow_id, parameter),
#'   #         source, value) +
#'   #     geom_boxplot(aes(fill = interaction(source, parameter)))
#'   # }
#'
#'   identity()
#'
