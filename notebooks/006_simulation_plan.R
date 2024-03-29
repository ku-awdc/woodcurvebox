#' ---
#'
#' ---
#'
source("notebooks/eda_startup.r")
devtools::load_all()
#'
#'
#' Additional things to consider
#'
#' - [ ] Add more than one infection pr. lactation period.
#' As it is reasonable to incur the infection more than once (
#' previous infection is a risk factor, environmental effects, etc.)
#' How much worse does the recoverability of the Wood's curve parameters get,
#' with positive probability of two infections, or three?
#'

#' Note we only examine cows in the *first* lactation
set.seed(20212409)
# herd_size <- 95
herd_size <- 300
# herd_size <- 200
# proportion_of_sick_cattle <- 0.50
proportion_of_sick_cattle <- 0.25
# herd_level_std_error <- 1
herd_level_std_error <- 5
total_infected <- herd_size * proportion_of_sick_cattle

#' These values are set via inspection of a curve of treatment.
lactation_phases <- list(
  early = c(left = 0, right = 30),
  mid = c(left = 31, right = 250),
  late = c(left = 251, right = 305)
)
lactation_phases %>%
  enframe("phase", "interval") %>%
  unnest_wider(interval) ->
lactation_phases_df
number_of_infected_in_phases <-
  c(
    0.6 * total_infected,
    0.05 * total_infected,
    0.35 * total_infected
  ) %>%
  round()
# number_of_infected_in_phases <-
#   c(0.33 * total_infected,
#     0.33 * total_infected,
#     0.33 * total_infected) %>%
#   floor()
number_of_infected_in_phases[1] <- number_of_infected_in_phases[1] +
  pmax(0, total_infected - sum(number_of_infected_in_phases))
#'
# DEBUG: There must be a better way to do this...
# number_of_infected_in_phases %>% sum
# total_infected

#' There are less severe cases than otherwise. This should be revised.
#'
overall_severity_of_infection <- runif(
  n = total_infected,
  min = 0, max = 1
)

#' This is an attempt at constructing the infected curves with complete
#' control over the proportions in each phase.
infection_process_df <-
  tibble(overall_severity_of_infection,
    infection_phase =
      rep(c("early", "mid", "late"),
        times = number_of_infected_in_phases
      ) %>%
        # fct_inorder()
        factor(levels = c("early", "mid", "late"))
  ) %>%
  #' deliberately not choosing a specific relation that is kept in
  #' `conf_id` (it could be severity, infection time, etc.)
  rowid_to_column("infection_id")
#'
infection_process_df
#'
#'
find_mastititis_shapes <- function(par = c(1,1)) {
  stopifnot(
    "only provide one pair of parameters (shape, scale)" = length(par) == 2)

  optim(par = par,
        function(x) {

          if (any(x < 0)) {
            return (Inf)
          }

          shape <- x[1]
          scale <- x[2]
          # replace shape by constrain that the mode is at 5 (i.e. `target_mode`)
          better_shape <- 5 / scale + 1

          # multiplication to bring the terms into the same level

          # cat(paste(shape, scale, collapse = ", "), sep = "\n")
          tail_end_weight <-
            pgamma(21, shape = shape, scale = scale,
                   lower.tail = FALSE)
          # cat(glue("skewniss {value}", value = pmax(0, log(shape / better_shape))),
          #     sep = "\n")

          # tail_end_weight +
          atan(tail_end_weight) * scale +
            2*pi * atan(abs(better_shape - shape)) / scale
        }) %>%
    `[[`("par") ->
    mastitis_shape_par
  shape <- mastitis_shape_par[1]
  scale <- mastitis_shape_par[2]
  list(shape = shape, scale = scale,
       mode = (shape - 1)*scale)
}


mastitis_scc_inflation_parameters <-
  find_mastititis_shapes(c(1,1))


mastitis_shape <- function(
    severity_scale,
    n = 100,
    x = NULL,
    mastitis_shape_par = mastitis_shape_par) {
  # assume `x` is within 0 and 21
  stopifnot(
    "`x` is between `c(0, 21)` days" = 0 <= x & x <= 21,
    "`n` should be positive integer" = n > 0
  )

  x <- if (is.null(x)) {
    seq.default(0, 21, length.out = n)
  } else {
    x
  }

  shape <- mastitis_shape_par$shape
  scale <- mastitis_shape_par$scale
  # ensures that the mode at 5 is 1.
  shape_value <- dgamma(x, shape = shape, scale = scale) /
    dgamma(5, shape = shape, scale = scale)

  list(
    # severity_scale = severity_scale,
    x = x,
    # mastitis_shape = shape_value,
    y = (1 - severity_scale) * 40 * shape_value +
      severity_scale * 200 * shape_value
  )
}

infection_process_df <-
  infection_process_df %>%
  mutate(
    scc_mass_inflation = overall_severity_of_infection %>%
      # note: this assumes that the time ticks are daily.
      map(~ mastitis_shape(.x,
        n = 22,
        mastitis_shape_par = mastitis_scc_inflation_parameters
      ))
  )
#'
#' Starting times for the infection should be determined next.
#'
#' The phases could be determined for a particular herd and its
#' peak-location, but we will refrain from this.
#' (See `lactation_phases` function)
#' Sketch: The idea was to get the phases from the parameters for
#' the whole herd (parameters of the first lactation), then
#'
#'
infection_process_df <- infection_process_df %>%
  left_join(lactation_phases_df,
    by = c("infection_phase" = "phase")
  ) %>%
  # TODO: these times should be weighted differently depending on the
  # `infection_phase`
  mutate(
    infection_time = map2_dbl(
      left, right,
      ~ sample(.x:.y,
        size = 1,
        replace = TRUE
      )
    ),
    infection_phase = fct_inorder(infection_phase)
  )
#' Next we need a way to apply the shifting using
#' `shift_extend_inflation_scc`

shift_extend_inflation_scc <- function(x, shift_by,
                                      total_length = 305, fill = 0) {
  #TODO: make this work with `shift_by` negative.

  stopifnot(
    "`shift_by` must be non-negative integer" =
      shift_by >= 0 &
      all.equal.numeric(
        abs(shift_by - trunc(shift_by)), 0
      )
  )
  c(
    rep.int(fill, pmin(shift_by, total_length)),
    x[seq_len(pmin(length(x), pmax(0, total_length - shift_by)))],
    rep.int(fill, times = pmax(total_length -
                                 length(x) - shift_by, 0))
  )
}

infection_process_df <-
  infection_process_df %>%
  unnest_wider(scc_mass_inflation) %>%
  # glimpse() %>%

  mutate(y = map2(
    y, infection_time,
    ~ shift_extend_inflation_scc(
      x = .x,
      # shift_by = .y - 1,
      shift_by = .y,
      total_length = 305
    )
  )) %>%
  #' previous `x` is no longer valid
  select(-x) %>%
  mutate(x = seq_along(y[[1]]) %>% list())
#'
#'
infection_process_df %>%
  unnest(c(x, y)) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(x, y,
        group = infection_id,
        color = overall_severity_of_infection
      ) +
      geom_line() +

      # shows the infection time but that's unnecessary
      # geom_vline(data = . %>% distinct(infection_id, infection_time),
      #            aes(xintercept = infection_time)) +

      scale_color_viridis_c()
  }
#'
#'
#'

#'
#' ## Derive SCC-curves
#'
#' But first, we have to adjust the values of the Wood's curve so that they
#' reflect the values of `SCC` or `log(SCC)`.
#'
#'

tibble(lactation = "first",
       a = 13.546, b = 19.575 / 1e2, c = 3.388 / 1e3) %>%
  add_row(lactation = "second",
          a = 18.820, b = 23.576 / 1e2, c = 5.441 / 1e3) %>%
  add_row(lactation = "third and more",
          a = 15.956, b = 30.004 / 1e2, c = 6.067 / 1e3) %>%
  mutate(lactation = fct_inorder(lactation)) ->
  milk_curves

daily_lactation_f <- function(n, a, b, c) {
  a * n ** b * exp(-c * n)
}
daily_lactation_f <- Vectorize(daily_lactation_f,
                               vectorize.args = c("a", "b", "c"))

milk_curves %>%
  #' only consider 1st lactation
  slice(1) %>%
  mutate(x = list(seq.default(5, 305))) %>%
  #' flip according to y = f(b/c) (i.e. peak-level)
  mutate(peak_location = b / c) %>%
  mutate(peak_value = a * (peak_location**b) * exp(-c * peak_location)) %>%
  mutate(y = pmap(
    select(., n = x, a, b, c),
    daily_lactation_f
  )) %>%
  unnest(c(x, y)) %>%
  mutate(
    yy = -y,
    yy = yy + 2 * peak_value,
    yy = yy * 5
  ) ->
somatic_cell_count_avg_herd_df
somatic_cell_count_avg_herd_df %>%
  # pivot_longer(c(y, yy)) %>%
  pivot_longer(c(yy)) %>%
  last_to_screen() %>%
  identity() %>%
  {
    ggplot(.) +
      aes(x, value, group = name) +
      geom_line() +
      facet_wrap(~name, scales = "free_y", ncol = 1) +
      geom_hline(aes(yintercept = 200, color = "threshold for sick cow"),
        data = . %>% filter(name == "yy")
      ) +
      geom_hline(aes(yintercept = 100, color = "threshold for healthy cow"),
        data = . %>% filter(name == "yy")
      ) +
      NULL
  } %>%
  # plotly::ggplotly() %>%
  identity()
#'
#'
#' While this is adequate for now, we would need to know a few configurations
#' describing a good cow, from an average cow, etc. as these things should
#' not influence this simulation, but they would indirectly influence the
#' sampling of the parameters used in this simulation.
#'
#'
#' Next, we got to find the values of this curve
somatic_cell_count_avg_herd_df %>%
  mutate(yy = as.numeric(yy)) %>%
  # mutate(yy = yy + rnorm(n(), mean = 0)) %>%
  nls(
    # formula = yy ~ a * x,
    formula = yy ~ a * x^b * exp(-c * x),
    start = list(a = 150, b = 0.6, c = 0.003),
    data = .,
    lower = list(a = 0, b = -Inf, c = -Inf),
    upper = list(a = Inf, b = Inf, c = Inf),
    algorithm = "port"
  ) %>%
  coef() %>%
  as.list() ->
scc_avg_herd_parameters
#'
#'
#'
peak_location <- function(a,b,c) {
  b/c
}
scc_avg_herd_parameters %>%
  exec(peak_location, !!!.)
#'
#'
# VALIDATION: the two curves should be mostly overlapping
tibble(
  x = seq.default(5, 305),
  y = exec(daily_lactation_f, n = x, !!!scc_avg_herd_parameters) %>%
    as.numeric()
) ->
fitted_scc_avg_herd_values
#'
#'
fitted_scc_avg_herd_values %>%
  exec(pracma::trapz, !!!.)
#'
#'
fitted_scc_avg_herd_values %>%
  bind_cols(yy = somatic_cell_count_avg_herd_df$yy %>%
    as.numeric()) %>%
  pivot_longer(c(y, yy)) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(x, value, group = name, color = name) +
      geom_line()
  }
#'
#' Alright, so the estimation is not exact..
#'
herd_with_average_cow_parameters <-
  scc_avg_herd_parameters %>%
  as_tibble()
#'
#' This result says that our intuition wrt. the signs of the parameters
#' may not hold true.
#'
#'
sample_scc_curves <- function(parameters_df, herd_size) {
  #' Generate individual cow parameters from the first lactation:
  parameters_df %>%
    mutate(herd_size = herd_size) %>%
    identity() %>% {
      bind_cols(
        rename(., herd_a = a, herd_b = b, herd_c = c),
        tibble(
          a = truncnorm::rtruncnorm(.$herd_size,
                                    # a = 0,
                                    # b = 0,
                                    mean = .$a,
                                    sd = 25),
          b = truncnorm::rtruncnorm(
            .$herd_size,
            # a = 0,
            # a = 0.01,
            a = -1, # otherwise the curve is not integrable.
            b = 0,
            mean = .$b,
            sd = 0.0001
            # sd = 0.1
            # sd = 0.04
            # sd = 0.2
          ),
          c = truncnorm::rtruncnorm(
            .$herd_size,
            # a = .$c,
            # a = -1,
            b = 0,
            mean = .$c,
            sd = 0.00001
            # sd = 0.0001
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
    return()
}

herd_with_average_cow_parameters %>%
  sample_scc_curves(herd_size = herd_size) ->
entire_herd_scc_parameters

entire_herd_scc_parameters %>%
  mutate(
    # x = list(n = seq.default(5, 305)),
    x = list(n = seq.default(1, 305)),
    y = daily_lactation_f(
      n = x[[1]],
      a = cow_a, b = cow_b, c = cow_c
    ) %>%
      asplit(MARGIN = 2)
  ) ->
entire_herd_scc_curves
#'
#'

#' Note that this might be useful to save, as it may be helpful to explore
#' other parameter estimation methods, e.g. see "009_epxlore_saemix_alternative.R"
#'
usethis::use_data(entire_herd_scc_curves, overwrite = TRUE)
#'
#'
entire_herd_scc_curves %>%
  unnest(c(x, y)) %>%
  mutate(flag = cow_b < 0) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(x, y, group = cow_id, color = cow_id) +
      geom_line() +
      guides(color = "none") +
      geom_line(
        data = tibble(
          x = seq_len(305),
          y = exec(daily_lactation_f,
            n = x,
            !!!scc_avg_herd_parameters
          ) %>%
            as.numeric(),
          cow_id = NA
        ),
        mapping = aes(x, y),
        linetype = "dashed",
        size = 2,
        alpha = 0.5,
        color = "black"
      ) +
      geom_hline(aes(yintercept = 200, color = "threshold for sick cow")) +
      geom_hline(aes(yintercept = 100, color = "threshold for healthy cow")) +
      scale_color_viridis_d() +
      facet_wrap(~flag, scales = "free_y", ncol = 1)
  }
#'
#'
#' How many cows are above and below the threshold
#'
#'
#'
entire_herd_scc_curves %>%
  unnest(y) %>%
  group_by(cow_id) %>%
  summarise(
    flag_good = sum(y < 100),
    flag_bad = sum(y > 200),
    flag_middle = sum(between(y, 100, 200)),
    score = c("bad", "middle", "good")[
      which.max(c(flag_bad, flag_middle, flag_good))
    ]
  ) ->
scoring_cows_overall
#'
#'
#'
scoring_cows_overall %>%
  identity() %>%
  {
    ggplot(.) +
      aes(cow_id, score, color = score, fill = score) +
      geom_col()
  }
#'
#'
scoring_cows_overall %>%
  count(score)
#'
#'
#' There are clearly outlier cows, that we need to investigate before going
#' further with this.
#'
entire_herd_scc_curves %>%
  unnest(c(x, y)) %>%
  group_by(cow_id) %>%
  summarise(
    a = unique(cow_a),
    b = unique(cow_b),
    c = unique(cow_c),
    y_int = pracma::trapz(x = x, y = y)
  ) %>%
  mutate(cow_id = fct_reorder(cow_id, y_int)) %>%
  # print()
  identity() %>%
  {
    ggplot(.) +
      aes(cow_id, y_int, fill = y_int) +
      geom_col() +
      NULL
  }
#'
#' ## Add infections to the herd curves
#'
entire_herd_scc_curves

entire_herd_scc_curves %>%
  rename(y_without_scc = y) %>%
  left_join(
    mutate(infection_process_df,
      assigned_cow_id = sample(
        .$cow_id,
        size = n(),
        replace = FALSE
      )
    ) %>%
      rename(y_extra_scc = y),
    by = c("cow_id" = "assigned_cow_id")
  ) %>%
  identity() ->
herd_with_infection_df
#'
#'
herd_with_infection_df <-
  herd_with_infection_df %>%
  mutate(
    y = map2(
      y_without_scc,
      y_extra_scc,
      ~ .x + (.y %||% 0)
    )
  )
#'
#'
#' Let's look at a few curves
#'
herd_with_infection_df %>%
  filter(!is.na(infection_time)) %>%
  rename(x = x.x) %>%
  sample_n(1) %>%
  unnest(c(x, y_extra_scc, y_without_scc, y)) %>%
  # pivot_longer(c(y_extra_scc, y_without_scc, y))
  # pivot_longer(c(y_extra_scc, y_without_scc)) %>%
  pivot_longer(c(y_extra_scc, y_without_scc, y)) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(x, value, group = name, color = name) +
      geom_line() +
      expand_limits(y = 90)
  }
#'
#'
herd_with_infection_df <-
  herd_with_infection_df %>%
  group_by(cow_id) %>%
  mutate(
    starting_sampling_day =
      sample(1:60, replace = TRUE, size = 1)
  ) %>%
  ungroup()

herd_with_infection_df %>%
  select(
    cow_id,
    starting_sampling_day,
    DIM = x.x,
    y = y
  ) %>%
  unnest(c(DIM, y)) %>%
  #' this is might be needless
  mutate(y = as.numeric(y)) %>%
  filter(DIM > 5) %>%
  group_by(cow_id) %>%
  slice(
    # browser(),
    seq.default(
      from = unique(starting_sampling_day),
      to = max(DIM), by = 30
    )
  ) %>%
  # group_modify(~.x %>% slice(
  #
  #   seq.default(from = unique(starting_sampling_day),
  #               to = max(DIM), by = 30)
  # ))
  ungroup() %>%
  # mutate(y = y + rnorm(n(), mean = 0, sd = 1)) ->
  mutate(y = y + rnorm(n(), mean = 0, sd = herd_level_std_error)) ->
fitting_df

fitting_df %>%
  identity() %>%
  {
    ggplot(.) +
      aes(DIM, y, group = cow_id, color = cow_id) +
      geom_line() +
      guides(color = "none") +
      scale_color_viridis_d()
  }
fitting_df %>%
  # pivot_longer(c(DIM, y)) %>%
  #' ensure that the rows are sorted by `cow_id`
  nlme::nlme(
    # logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
    # daily_lactation_f  ~ a * (DIM ** b) * exp(-c * DIM),
    y ~ a * (DIM**b) * exp(-c * DIM),
    data = .,
    fixed = a + b + c ~ 1,
    random = a + b + c ~ 1,
    groups = ~cow_id,
    start = c(
      a = 150,
      b = -0.1,
      c = -0.003
    ),
    # na.action = na.exclude,
    # control = list(maxIter = 1200, msMaxIter = 1200)
    # control = list(maxIter = 1200)
    control = list(returnObject = TRUE)
  ) ->
cow_level_model_output
#'
cow_level_model_output
# nlme::predict.nlme()
#'
#'
#'
herd_with_infection_df %>%
  left_join(
    nlme:::coef.lme(object = cow_level_model_output) %>%
      rename_with(~ paste0(.x, "_est")) %>%
      rownames_to_column("cow_id"),
    by = "cow_id"
  ) %>%
  mutate(
    # x = list(n = seq.default(5, 305)),
    # x = list(n = seq.default(1, 305)),
    y_est = daily_lactation_f(
      n = x.x[[1]],
      a = a_est, b = b_est, c = c_est
    ) %>%
      asplit(MARGIN = 2)
  ) %>%
  mutate(
    is_infected = !is.na(infection_time)
  ) ->
full_fitted_herd_df
#'
#'
#'
full_fitted_herd_df %>%
  mutate(
    error_a = abs(cow_a - a_est),
    error_b = abs(cow_b - b_est),
    error_c = abs(cow_c - c_est)
  ) %>%
  pivot_longer(starts_with("error"),
    names_sep = "_",
    names_to = c(NA, "error_label")
  ) %>%
  glimpse() %>%
  identity() %>%
  mutate(infection_phase = fct_inorder(infection_phase)) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(starting_sampling_day, value, color = is_infected) +
      geom_line() +
      geom_point() +
      # facet_wrap(~error_label, ncol = 1, scales = "free")
      # facet_wrap(infection_phase~error_label, ncol = 3, scales = "free")
      facet_wrap(infection_phase ~ error_label, ncol = 3, scales = "free_y")
  }
#'
#'
#'
full_fitted_herd_df <- full_fitted_herd_df %>%
  mutate(
    error_a = abs(cow_a - a_est),
    error_b = abs(cow_b - b_est),
    error_c = abs(cow_c - c_est)
  )

full_fitted_herd_df %>%
  pivot_longer(starts_with("error"),
    names_sep = "_",
    names_to = c(NA, "error_label"),
    values_to = "error_value"
  ) %>%
  mutate(error_label = fct_inorder(error_label)) %>%
  group_by(is_infected, error_label) %>%
  slice_max(error_value, n = 50) %>%
  glimpse() %>%
  mutate(DIM = x.x) %>%
  unnest(c(DIM, y, y_est)) %>%
  pivot_longer(c(y, y_est)) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(DIM, value,
        group = interaction(cow_id, name),
        color = name
      ) +
      geom_line() +
      labs(
        x = "Days in Milk [day]",
        y = "SCC [k]"
      ) +
      facet_wrap(is_infected ~ error_label,
        ncol = 3, scales = "free_y",
        labeller = label_both
      )
  }
#'
full_fitted_herd_df %>%
  glimpse() %>%
  group_by(cow_id) %>%
  mutate(
    mse = mean((y_without_scc[[1]] - y_est[[1]])**2),
    mae = mean(abs(y_without_scc[[1]] - y_est[[1]]))
  ) %>%
  ungroup() ->
  full_fitted_herd_error_df
#'
full_fitted_herd_error_df %>%

  filter(infection_phase %in% c("early", "late", NA)) %>%
  glimpse() %>%

  pivot_longer(cols = c(mae, mse), names_to = "error_type", values_to = "error_value") %>%

  # select((where(~!is.list(.x) | length(.x) == 1))) %>% View()

  print(n = 25, width = Inf) %>% {
    ggplot(.) +
      # aes(mae, groups = is_infected, color = is_infected) +
      aes(error_value, groups = interaction(error_type, is_infected, infection_phase),
          color = interaction(is_infected, infection_phase)) +
      stat_ecdf() +
      # facet_wrap(~is_infected) +

      # scale_color_manual(values = c("TRUE" = "red", "FALSE" = "green")) +

      facet_wrap(~error_type, scales = "free_x") +
      NULL
  }
#'
#'
#'
full_fitted_herd_error_df %>%
  pivot_longer(c(mse, mae),
    names_to = "accum_error_label",
    values_to = "accum_error"
  ) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(starting_sampling_day, accum_error,
        color = is_infected
      ) +
      geom_line() +
      facet_wrap(infection_phase ~ accum_error_label,
        ncol = 2,
        scales = "free_y"
      )
  }
#' Maya: If you catch an early infection, then it is really influencial (
#' in a bad way) on the estimated parameters. If we don't catch it, then everything
#' is fine.
#'
#'
full_fitted_herd_df %>%
  glimpse()
#'
#'
#' Single plot:
filtered_data <- full_fitted_herd_df %>%
  pivot_longer(starts_with("error"),
               names_sep = "_",
               names_to = c(NA, "error_label"),
               values_to = "error_value"
  ) %>%
  mutate(error_label = fct_inorder(error_label)) %>%
  group_by(is_infected, error_label) %>%
  slice_max(error_value, n = 50) %>%
  glimpse() %>%
  mutate(DIM = x.x) %>%
  unnest(c(DIM, y, y_est)) %>%
  pivot_longer(c(y, y_est)) %>%
  identity()

ggplot(filtered_data, aes(DIM, value, group = interaction(cow_id, name), color = name)) +
  geom_line() +
  labs(x = "Days in Milk [day]", y = "SCC [k]") +
  theme_bw() +
  theme(text = element_text(size = 22)) +
  theme(legend.position = "none")
ggsave("C:/Users/zjt234/PhD/Thesis/figures/sim_infections_Mossa.tiff", width = 40, height = 20, units = "cm", dpi=300)

