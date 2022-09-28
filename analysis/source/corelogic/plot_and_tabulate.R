#INPUT:
# "with_rel_inc_all.csv"
# "with_rel_inc_foreclosure.csv"

#OUTPUT:
# "density_error_all.png"
# "density_error_foreclosure.png"
# "probability_underwater_one_panel_all.png"
# "probability_underwater_one_panel_foreclosure.png"

library(lubridate)
library(gt)

setwd("~/repo/strategic")
source("prelim.R")

greys <- brewer.pal("Greys", n = 9)


# truncate cauchy distribution at -1
conditional_cauchy <- function(x, location, scale) {
  (pcauchy(x, location, scale) - pcauchy(-1, location, scale)) / (1 - pcauchy(-1, location, scale))
}


# numeric derivative of conditional_cauchy()
density_cc <- function(x, location, scale) {
  (conditional_cauchy(x + 0.0000001, location, scale) - conditional_cauchy(x, location, scale)) / 0.0000001
}


# numerically minimize squared distance to given quantile
get_quantile <- function(q, lc, sc) {
  quant_dist <- function(x) {
    (conditional_cauchy(x, lc, sc) - q)^2
  }

  optimize(f = quant_dist, interval = c(-1, 20000))$minimum
}

dist <- function(median, iqr) {
  function(params) {
    (median - get_quantile(0.5, params[1], params[2]))^2 +
      (iqr - get_quantile(0.75, params[1], params[2]) +
        get_quantile(0.25, params[1], params[2]))^2
  }
}


get_quantiles <- function(error_dist, truncation) {
  error_dist <- error_dist %>%
    filter(
      error < quantile(error, 1 - truncation),
      error > quantile(error, truncation)
    )

  error_dist %>%
    summarise(
      q50 = quantile(error, 0.5),
      iqr = quantile(error, 0.75) - quantile(error, 0.25),
      type = "main_est"
    ) %>%
    bind_rows(
      error_dist %>%
        summarise(
          q50 = quantile(error, 0.5),
          iqr = 2 * (quantile(error, 0.75) - quantile(error, 0.25)),
          type = "scaled_est"
        )
    ) %>%
    bind_rows(
      error_dist %>%
        mutate(error = (error + 1) / 0.91 - 1) %>%
        summarise(
          q50 = quantile(error, 0.5),
          iqr = quantile(error, 0.75) - quantile(error, 0.25),
          type = "biased_est"
        )
    )
}

error_dist <- map_dfr(
  c("all", "foreclosure"),
  ~ {
    read_csv(str_c(
      "/project2/pnoel/probertson/repo/strategic/",
      "analysis/release/corelogic/with_rel_inc_",
      .x,
      ".csv"
    )) %>%
      filter(curr_sale_amount > 1000, prev_sale_amount > 1000) %>% # remove nominal transactions
      mutate(
        predicted_sale_amount = prev_sale_amount * hpi_increase,
        error = (predicted_sale_amount / curr_sale_amount - 1),
        sample = .x
      ) %>%
      mutate(
        time_diff = as.numeric((date - prev_sale) / 365),
        change = curr_sale_amount / prev_sale_amount,
        yearly_change = change^(1 / time_diff),
        valid_yearly_change = abs(yearly_change - 1) < 0.5
      ) %>%
      select(
        fips, error, prev_sale_amount, predicted_sale_amount,
        valid_yearly_change,
        prev_sale, curr_sale_amount, date,
        sample
      ) %>%
      filter(valid_yearly_change == 1)
  }
) %>%
  ungroup()


estimates <- map_dfr(
  c(0, 0.025),
  ~ get_quantiles(
    error_dist %>% group_by(sample),
    .x
  ) %>%
    mutate(truncation = .x)
) %>%
  mutate(
    params = map2(
      q50, iqr,
      ~ optim(c(0.0101, 0.125), dist(.x, .y))$par
    ),
    location = map_dbl(params, 1),
    scale = map_dbl(params, 2)
  ) %>%
  select(-params) %>%
  arrange(sample, type) %>%
  filter(!(sample == "foreclosure" & type == "biased_est"))

function_est <- estimates %>%
  group_by(sample) %>%
  mutate(function_val = map2(
    location, scale,
    ~ tibble(
      error = seq(-99, 130) / 100,
      density = density_cc(error, .x, .y)
    )
  )) %>%
  unnest()

map(
  c("all", "foreclosure"),
  ~ {
    error_dist %>%
      ungroup() %>%
      filter(sample == .x) %>%
      mutate(error = if_else(error > quantile(error, 0.999),
        quantile(error, 0.999),
        error
      )) %>%
      ggplot() +
      geom_histogram(aes(
        x = error,
        y = ..density..,
        fill = "obs"
      ),
      binwidth = 0.01,
      alpha = 0.5
      ) +
      geom_line(aes(
        x = error, y = density,
        colour = type
      ),
      data = function_est %>%
        filter(
          truncation == 0,
          type == "main_est",
          sample == .x
        )
      ) +
      scale_x_continuous(labels = function(x) scales::percent(x)) +
      scale_colour_manual(
        name = "Parametric estimates",
        breaks = c("main_est"),
        labels = c(main_est = "Parametric estimate"),
        values = c(
          main_est = "#218380",
          biased_est = "#D81159",
          scaled_est = "#FFBC42"
        )
      ) +
      scale_fill_manual(
        name = NULL,
        breaks = c("obs"),
        labels = c("Non-parametric estimate"),
        values = "grey50"
      ) +
      labs(
        subtitle = "Density",
        y = "",
        x = "Percentage error in home price"
      ) +
      fte_theme() +
      theme(
        legend.position = c(0.76, 0.8),
        legend.text = element_text(size = rel(1)),
        plot.title.position = "plot",
        plot.subtitle = element_text(
          color = greys[7],
          size = 18
        ),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)
      ) +
      coord_cartesian(
        xlim = c(-1, 1.3),
        ylim = c(0, 4)
      )
    ggsave(str_c(
      "analysis/release/corelogic/density_error_",
      .x,
      ".png"
    ),
    width = 6, height = 4
    )
  }
)

ltv <- tibble(
  ltv = seq(1, 300),
  error_size = (100 - ltv) / ltv
)

estimated_probs <- estimates %>%
  group_by(sample) %>%
  mutate(data = map2(
    location, scale,
    ~ ltv %>%
      mutate(
        parametric = "yes",
        type = .y,
        prob_under = 1 - conditional_cauchy(error_size, .x, .y)
      )
  )) %>%
  unnest()

mean_error <- error_dist %>%
  group_by(sample) %>%
  mean(error = mean(error))

non_para_probs <- map_dfr(
  c("all", "foreclosure"),
  ~ {
    ltv %>%
      rowwise() %>%
      mutate(prob_under = 1 - mean(filter(error_dist, sample == .x)$error < error_size)) %>%
      mutate(
        parametric = "no",
        sample = .x
      )
  }
)

estimated_probs <- expand_grid(
  type = c(
    "main_est",
    "scaled_est",
    "biased_est"
  ),
  truncation = c(0, 0.025)
) %>%
  mutate(data = list(non_para_probs)) %>%
  unnest() %>%
  bind_rows(estimated_probs)


labels <- tribble(
  ~ltv, ~prob_under, ~type, ~type_colour, ~label,
  72, 0.95, "main_est", "np", "non-parametric\nestimate",
  170, 0.83, "main_est", "main_est", "parametric\nestimate"
) %>%
  mutate(type = factor(type, levels = c(
    "main_est",
    "biased_est",
    "scaled_est"
  )))

map(
  c("all", "foreclosure"),
  ~ {
    estimated_probs %>%
      filter(
        truncation == 0,
        sample == .x
      ) %>%
      mutate(
        type_colour = if_else(parametric == "yes",
          type,
          "np"
        ),
        type = factor(type, levels = c(
          "main_est",
          "biased_est",
          "scaled_est"
        ))
      ) %>%
      filter(type == "main_est") %>%
      ggplot() +
      aes(
        x = ltv, y = prob_under,
        colour = type_colour
      ) %>%
      geom_line() +
      geom_label(
        data = labels,
        aes(
          x = ltv, y = prob_under,
          label = label,
          colour = type_colour
        ),
        label.size = 0,
        label.r = unit(1, "lines"),
        size = 3,
        family = "serif"
      ) +
      fte_theme() +
      theme(
        panel.grid.minor = element_line(),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = greys[7])
      ) +
      scale_x_continuous(
        name = "Observed loan-to-value ratio", minor_breaks = c(50, 150),
        limits = c(0, 220),
        breaks = c(0, 100, 200),
        labels = function(x) scales::percent(x / 100)
      ) +
      scale_colour_manual(values = c(
        main_est = "#218380",
        biased_est = "#D81159",
        scaled_est = "#FFBC42",
        np = "grey50"
      )) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        y = "",
        subtitle = "Probability actually underwater"
      )
    ggsave(str_c(
      "analysis/release/corelogic/probability_underwater_one_panel_",
      .x, ".png"
    ),
    width = 4, height = 4
    )
  }
)