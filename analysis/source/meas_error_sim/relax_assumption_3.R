# relax_assumption_3.R
# Peter Ganong
# March 16, 2021
# Run simulations relaxing assumption 3 and explore
# performance of various estimators

#INPUT (NONE)

#OUTPUT
# "document_bias.png"
# "efficacy_method.tex"
# "binscatter_facet.png"
# "show_all_defaults_wrong.png"
# "why_all_defaults_wrong.png"
# "how_transition_to_default_solves.png"

theme_set(fte_theme() +
  theme(legend.text = element_text(size = 14)))
color_palette <- c("#2b83ba", "#abdda4")
grey_palette <- brewer.pal("Greys", n = 9)

# define functions ----
simulate_once <- function(params, corr) {
  test_that(
    "no one always defaults aka Y(0,0) = 0",
    expect_gte(
      -params["constant_a"],
      params["psi"]
    )
  )
  test_that(
    "abovewater default possible",
    expect_lte(
      -params["constant_a"],
      params["psi"] + params["beta_avg"] + params["beta_disperse"]
    )
  ) # nolint

  set.seed(0)
  n_periods_per_agent <- 10
  n_periods <- params["n_agents"] * n_periods_per_agent

  df <- tibble(
    id = rep(1:params["n_agents"],
      each = n_periods_per_agent
    ),
    epsilon = rep(runif(params["n_agents"], max = params["psi"]),
      each = n_periods_per_agent
    ),
    eta = runif(n_periods,
      max = params["sigma"]
    ),
    t_unit = runif(n_periods),
    beta = runif(n_periods,
      min = params["beta_avg"],
      max = params["beta_avg"] + params["beta_disperse"]
    )
  )
  if (!corr) {
    df <- df %>%
      mutate(t = t_unit < params["prob_life_event"])
  } else {
    df <- df %>%
      mutate(
        t = t_unit < params["prob_life_event"] * (2 * epsilon / params["psi"])
      )
  }

  df <-
    df %>%
    mutate(
      base = params["constant_a"] + epsilon,
      y = base + eta + beta * t > 0,
      `Y if no life event` = base + eta > 0,
      y_abovewater = base + beta * t > 0,
    ) %>%
    group_by(id) %>%
    mutate(
      period = row_number(),
      lag_y = lag(y),
      lag2_y = lag(y, 2),
      lag_y_aw = lag(y_abovewater),
      lag_t = lag(t),
      lag2_t = lag(lag_t),
      diff_t = c(NA, diff(t))
    ) %>%
    ungroup()
  df <-
    df %>%
    filter(y) %>%
    group_by(id) %>%
    summarise(period_first_default = min(period), .groups = "drop") %>%
    right_join(df, by = "id")

  df <-
    df %>%
    filter(y_abovewater) %>%
    group_by(id) %>%
    summarise(period_first_default_aw = min(period), .groups = "drop") %>%
    right_join(df, by = "id")
}

sum_t <- function(df) {
  df %>%
    filter(period >= 2) %>%
    transmute(
      `Default (new)` = t == 1,
      `Period before default` = lag_t == 1
    ) %>%
    summarise_all(~ round(mean(.), 3))
}

tabulate <- function(df) {
  life_events_abovewater <-
    df %>%
    filter(y_abovewater, !lag_y_aw) %>%
    sum_t() %>%
    bind_cols(df %>%
      filter(y_abovewater) %>%
      summarise(`Default (any)` = round(mean(t), 3))) %>%
    mutate(key = "Abovewater")

  life_events_underwater <-
    df %>%
    filter(y, !lag_y) %>%
    sum_t() %>%
    bind_cols(df %>%
      filter(y) %>%
      summarise(`Default (any)` = round(mean(t), 3))) %>%
    mutate(key = "Underwater")

  life_events_all <-
    df %>%
    summarise(round(mean(t), 3)) %>%
    pull()

  tmp <-
    bind_rows(life_events_abovewater, life_events_underwater) %>%
    pivot_longer(-key) %>%
    pivot_wider(names_from = key) %>%
    rename(key = name) %>%
    pivot_longer(-key) %>%
    add_row(
      key = "All borrowers",
      name = "Underwater",
      value = life_events_all,
      .before = 0
    )

  alpha_xsec <-
    round(((tmp %>%
      filter(key == "Default (any)", name == "Underwater") %>%
      pull(value) - life_events_all) /
      (1 - life_events_all)), 3)

  alpha_panel <-
    round(
      ((tmp %>%
        filter(
          key == "Default (new)",
          name == "Underwater"
        ) %>%
        pull(value))
      - ((tmp %>%
          filter(
            key == "Period before default",
            name == "Underwater"
          ) %>%
          pull(value)))
      ) /
        ((tmp %>%
          filter(
            key == "Default (new)",
            name == "Abovewater"
          ) %>%
          pull(value)) -
          (tmp %>%
            filter(
              key == "Period before default",
              name == "Abovewater"
            ) %>%
            pull(value))),
      3
    )

  alpha_panel_alt <-
    round(
      (tmp %>%
        filter(
          key == "Default (new)",
          name == "Underwater"
        ) %>%
        pull(value) - life_events_all) /
        (tmp %>%
          filter(
            key == "Default (new)",
            name == "Abovewater"
          ) %>%
          pull(value) - life_events_all),
      3
    )

  alpha_true_xsec <- df %>%
    filter(y) %>%
    summarise(round(mean(1 - `Y if no life event`), 3), .groups = "drop") %>%
    pull()

  alpha_true_panel <- df %>%
    filter(y, !lag_y) %>%
    summarise(round(mean(1 - `Y if no life event`), 3), .groups = "drop") %>%
    pull()

  tmp %>%
    bind_rows(
      tribble(
        ~key, ~value, ~method, ~source,
        "Alpha", alpha_xsec, "All defaults", "Estimate",
        "Alpha", alpha_panel_alt, "Transition to default", "Estimate",
        "Alpha", alpha_panel, "Transition to default (new LEs)", "Estimate",
        "Alpha", alpha_true_xsec, "All defaults", "True",
        "Alpha", alpha_true_panel, "Transition to default", "True",
        "Alpha", alpha_true_panel, "Transition to default (new LEs)", "True"
      ) %>% mutate(name = "")
    )
}

simulate_twice <- function(params) {
  # fix params that don't vary across simulations
  params["beta_disperse"] <- 0
  params["constant_a"] <- -5

  bind_rows(
    simulate_once(params, corr = FALSE) %>% mutate(scenario = "Independent"),
    simulate_once(params, corr = TRUE) %>% mutate(scenario = "Correlated")
  )
}


estimate_and_bias <- function(params) {
  print(params)
  df_pooled <- simulate_twice(params)

  # note: ideally we would refactor tabulate to work with group_by(). If done
  # successfully we would be able to get rid of all this copy-paste garbage here.
  tmp <-
    bind_rows(
      tabulate(df_pooled %>%
        filter(scenario == "Independent")) %>%
        mutate(`Life events & default` = "Independent"),
      tabulate(df_pooled %>%
        filter(scenario == "Correlated")) %>%
        mutate(`Life events & default` = "Correlated")
    ) %>%
    pivot_wider(names_from = "Life events & default") %>%
    select(-name) %>%
    filter(str_detect(key, "Alpha")) %>%
    pivot_longer(Independent:Correlated) %>%
    pivot_wider(names_from = source) %>%
    mutate(bias = Estimate - True) %>%
    as.data.frame()

  tmp %>%
    select(method, name, bias) %>%
    pivot_wider(names_from = method, values_from = bias) %>%
    left_join(tmp %>%
      filter(method == "Transition to default") %>%
      select(name, True),
    by = "name"
    )
}


# understand why the panel estimator comes in too low sometimes ----
# axis 1 to elim bias: lower probability of life events
# (little change in underlying \alpha)
# axis 2 to elim bias: lower epsilon and raise

params_panel_wrong <- c(
  n_agents = 1000,
  prob_life_event = 0.5,
  psi = 2.5,
  sigma = 2.75,
  beta_avg = 3,
  constant_a = -5
)
df_pooled <- simulate_twice(params_panel_wrong)
df_pooled %>%
  filter(scenario == "Independent", !lag2_y, !lag_y, y, period >= 3) %>%
  summarise_at(vars(t, lag_t, lag2_t), mean)
df_pooled %>% summarise(mean(t))

params_df_panel_wrong <-
  tribble(
    ~n_agents, ~prob_life_event, ~psi, ~sigma, ~beta_avg, ~constant_a,
    10000, 0.5, 2.5, 2.75, 3, -5,
  )
params_df_panel_wrong %>%
  transpose() %>%
  simplify_all() %>%
  map_dfr(estimate_and_bias)

# true alpha is 1, every default is caused by a life event
# yet panel gives 0.815. why? because P(life event) in pre-period is 0.179
# it would be better if we ignored the life events in the pre-period and
# just did P(life event) at date of new default...

# end troubleshooting ----

# run main analysis ----
params_df <-
  tribble(
    ~n_agents, ~prob_life_event, ~psi, ~sigma, ~beta_avg, ~constant_a,
    10000, 0.5, 5, 4, 0.5, -5,
    10000, 0.5, 4.5, 4, 1, -5,
    10000, 0.5, 4, 4, 1.5, -5,
    10000, 0.5, 3.5, 4, 2, -5,
    10000, 0.5, 3, 4, 2.5, -5,
    10000, 0.5, 2.5, 4, 3, -5,
    10000, 0.1, 2.5, 2.75, 3, -5
  )

for_plot <- params_df %>%
  transpose() %>%
  simplify_all() %>%
  map_dfr(estimate_and_bias)


for_plot %>%
  mutate(scenario = ifelse(name == "Correlated",
    "Correlated (relax assumption 3)",
    "Independent (maintain assumption 3)"
  )) %>%
  select(-name) %>%
  pivot_longer(contains("default")) %>%
  filter(name != "Transition to default (new LEs)") %>%
  ggplot(aes(x = True, y = value, color = name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(
    color = "",
    y = "Bias (estimated alpha minus true alpha)",
    x = "True alpha (share of defaults caused by life events)"
  ) +
  scale_color_manual(values = color_palette) +
  facet_wrap(vars(scenario)) +
  theme(legend.position = "bottom")
ggsave("document_bias.png",
  path = out_path,
  height = 4.5, width = 8
)



# analyze corner scenario in-depth ----
params_hypothetical <- c(
  n_agents = 10000,
  prob_life_event = 0.5,
  psi = 5,
  sigma = 4,
  beta_avg = 0.5
)
df_pooled <- simulate_twice(params_hypothetical)

# these stats are being ported by hand into the table, need to automate
df_independent <- tabulate(df_pooled %>% filter(scenario == "Independent"))
df_correlated <- tabulate(df_pooled %>% filter(scenario == "Correlated"))


get_tbl <- function(df_in) {
  df <- df_in %>%
    mutate(order = case_when(
      key == "All borrowers" & name == "Underwater" ~ 1,
      key == "Default (any)" & name == "Underwater" ~ 2,
      key == "Default (any)" & name == "Abovewater" ~ 3,
      key == "Alpha" & method == "All defaults" & source == "Estimate" ~ 4,
      key == "Alpha" & method == "All defaults" & source == "True" ~ 5,
      key == "Period before default" & name == "Underwater" ~ 7,
      key == "Default (new)" & name == "Underwater" ~ 8,
      key == "Default (new)" & name == "Abovewater" ~ 9,
      key == "Alpha" & method == "Transition to default" & source == "Estimate" ~ 10, # nolint
      key == "Alpha" & method == "Transition to default" & source == "True" ~ 11
    )) %>%
    filter(!is.na(order))

  df %>%
    bind_rows(
      tribble(
        ~value, ~order,
        df$value[df$order == 4] - df$value[df$order == 5], 6,
        df$value[df$order == 10] - df$value[df$order == 11], 12
      )
    ) %>%
    arrange(order) %>%
    transmute(order,
      value = if_else(order %in% c(1:3, 7:9),
        percent(value, accuracy = 1),
        as.character(round(value, digits = 2))
      )
    )
}

tex_tbl <- get_tbl(df_independent) %>%
  rename(Independent = value) %>%
  left_join(get_tbl(df_correlated) %>%
    rename(Correlated = value),
  by = "order"
  ) %>%
  mutate(
    label = case_when(
      order %in% c(1, 7) ~ "All underwater [1]",
      order %in% c(2, 8) ~ "Underwater: defaulters [2]",
      order %in% c(3, 9) ~ "Above water: defaulters [3]",
      order %in% c(4, 10) ~ "ddd \\hat{\\alpha}ddd = (2 - 1)/(3-1)",
      order %in% c(5, 11) ~ "ddd \\alpha ddd",
      order %in% c(6, 12) ~ "ddd \\hat{\\alpha} - \\alpha ddd"
    ),
    empty = ""
  ) %>%
  bind_rows(tribble(
    ~empty, ~order,
    "Share with life event P(T*)", 0.5,
    "Share of defaults caused by life events", 3.5,
    "Share with life event P(T*)", 6.5,
    "Share of defaults caused by life events", 9.5
  )) %>%
  arrange(order) %>%
  select(
    Statistic = empty,
    `Group/Formula` = label,
    Independent, Correlated,
    order
  ) %>%
  gt() %>%
  tab_row_group(
    group = "\"Transition to default\" method for estimating ddd \\alpha ddd",
    rows = order > 6
  ) %>%
  tab_row_group(
    group = "\"All defaults\" method for estimating ddd \\alpha ddd",
    rows = order <= 6
  ) %>%
  cols_hide("order") %>%
  fmt_missing(2:4, missing_text = "") %>%
  as_latex() %>%
  str_replace("\\\\toprule", "") %>%
  str_replace("llll", "llcc") %>%
  str_replace(
    "\\\\captionsetup\\[table\\]\\{labelformat=empty,skip=1pt\\}",
    ""
  ) %>%
  str_replace_all("longtable", "tabular") %>%
  str_replace_all(
    "multicolumn\\{1\\}\\{l\\}",
    "multicolumn\\{4\\}\\{c\\}"
  ) %>%
  str_replace_all(
    "Share with life event P\\(T\\*\\) &",
    "\\\\multicolumn\\{2\\}\\{l\\}\\{Share with life event P\\(T\\*\\)\\}"
  ) %>% # nolint
  str_replace_all(
    "Share of defaults caused by life events &",
    "\\\\multicolumn\\{2\\}\\{l\\}\\{Share of defaults caused by life events\\}"
  ) %>% # nolint
  str_replace_all("textbackslash\\{\\}", "") %>%
  str_replace_all("ddd", "$") %>%
  str_replace_all("\\\\\\{", "\\{") %>%
  str_replace_all("\\\\\\}", "\\}") %>%
  suppressWarnings()


write_lines(
  tex_tbl,
  str_c(
    out_path,
    "efficacy_method.tex"
  )
)

####################################################

df_pooled %>%
  filter(scenario == "Correlated") %>%
  transmute(epsilon, t = as.numeric(t)) %>%
  correlate()


df_pooled %>%
  group_by(
    scenario = fct_relevel(factor(scenario), "Independent"),
    epsilon_bin = -floor(epsilon * 4) / 4 + max(df_pooled$epsilon)
  ) %>%
  summarise(
    `Life event` = mean(t),
    `Default` = mean(y),
    `Default (assume no life events)` = mean(`Y if no life event`),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  pivot_longer(`Life event`:`Default (assume no life events)`) %>%
  mutate(name = fct_relevel(factor(name), "Life event")) %>%
  ggplot(aes(epsilon_bin, value,
    color = scenario, shape = scenario
  )) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(15, 16)) +
  scale_color_manual(values = c(color_palette[1], "#9e9ac8")) +
  facet_wrap(vars(name)) +
  labs(
    x = "Borrower cost of default",
    y = "Rate",
    color = "",
    shape = ""
  ) +
  theme(legend.position = "bottom")
ggsave("binscatter_facet.png",
  path = out_path,
  height = 4.5, width = 8
)


collapse_by_group <- function(df) {
  df %>%
    group_by(scenario) %>%
    count(`Default type` = fct_rev(factor(case_when(
      !t ~ "Obviously strategic (no life event)",
      t & !`Y if no life event` ~ "Coincide with life event, caused by life event", # nolint
      TRUE ~ "Coincide with life event, not caused by life event"
    ))))
}

bind_rows(
  df_pooled %>%
    filter(y) %>%
    collapse_by_group() %>%
    mutate(method = "All defaults"),
  df_pooled %>%
    filter(y, !lag_y) %>%
    collapse_by_group() %>%
    mutate(method = "Transitions to default")
) %>%
  group_by(method, scenario) %>%
  mutate(share = round(n / sum(n), 3)) %>%
  ggplot(aes(fill = `Default type`, y = share, x = scenario)) +
  geom_col(position = "fill") +
  geom_text(aes(label = percent(share, accuracy = 0.1)),
    size = 3,
    position = position_stack(vjust = 0.3)
  ) +
  guides(fill = guide_legend(ncol = 1)) +
  scale_fill_manual(values = c(color_palette, "#fc8d59")) +
  geom_hline(yintercept = mean(df_pooled$y)) +
  labs(y = "Share of defaults by presence and causation of life event") + # nolint
  scale_y_continuous(labels = scales::percent) +
  annotate("text",
    x = 1.5, y = 0.44, vjust = "top",
    label = "Unconditional share of life events"
  ) +
  facet_wrap(vars(method)) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(size = 9)
  )
ggsave("show_all_defaults_wrong.png",
  path = out_path,
  height = 4.5, width = 8
)

df_pooled %>%
  mutate(
    epsilon_max = max(epsilon),
    scenario = ifelse(scenario == "Correlated",
      "Correlated (relax assumption 3)",
      "Independent (maintain assumption 3)"
    )
  ) %>%
  filter(period >= 2) %>%
  group_by(
    epsilon_bin = (-floor(epsilon * 2) / 2 + epsilon_max) / epsilon_max,
    scenario
  ) %>%
  count(`Default type` = fct_rev(factor(case_when(
    y & !t ~ "Obviously strategic (no life event)",
    y & t & !`Y if no life event` ~ "Coincide with life event, caused by life event", # nolint
    y & t & `Y if no life event` ~ "Coincide with life event, not caused by life event", # nolint
    TRUE ~ "No default"
  )))) %>%
  mutate(
    share = n / sum(n),
    scenario = factor(scenario,
      levels = c(
        "Independent (maintain assumption 3)",
        "Correlated (relax assumption 3)"
      )
    )
  ) %>%
  filter(`Default type` != "No default") %>%
  ggplot(aes(x = epsilon_bin, y = share, fill = `Default type`)) +
  geom_area() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(color_palette, "#fc8d59")) +
  guides(fill = guide_legend(ncol = 1)) +
  facet_wrap(vars(scenario)) +
  labs(
    x = "Borrower cost of default (normalized)",
    y = "Default rate by presence and causation of life event"
  ) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(size = 9)
  )
ggsave("/why_all_defaults_wrong.png",
  path = out_path,
  height = 4.5, width = 8
)

params_match_data <- c(
  n_agents = 10000,
  prob_life_event = 0.1,
  psi = 2.5,
  sigma = 2.75,
  beta_avg = 3
)
df_pooled %>%
  mutate(params = "Hypothetical") %>%
  bind_rows(
    simulate_twice(params_match_data) %>%
      mutate(params = "Realistic")
  ) %>%
  group_by(params) %>%
  mutate(epsilon_max = max(epsilon)) %>%
  filter(
    scenario == "Correlated",
    period >= 2
  ) %>%
  group_by(params,
    epsilon_bin = (-floor(epsilon * 4) / 4 + epsilon_max) / epsilon_max
  ) %>% # nolint
  summarise(
    `No transition to default (drop)` = mean(y & lag_y),
    `Transition to default (keep)` = mean(y & !lag_y),
    .groups = "keep"
  ) %>%
  pivot_longer(contains("default"),
    names_to = "Default type"
  ) %>%
  ggplot(aes(x = epsilon_bin, y = value, fill = `Default type`)) +
  geom_area() +
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = color_palette) +
  facet_wrap(vars(params)) +
  labs(
    x = "Borrower cost of default (normalized)",
    y = "Default rate by lagged default status"
  ) +
  theme(legend.position = "bottom")
ggsave("how_transition_to_default_solves.png",
  path = out_path,
  height = 4.5, width = 8
)
