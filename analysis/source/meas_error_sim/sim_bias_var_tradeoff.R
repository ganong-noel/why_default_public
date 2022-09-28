#INPUT (NONE)

#OUTPUT
# "simulation_se.png"

# Generate fake data
treatment_cutoff <- 0
P_T <- 0.5
P_S <- 0.25
E_Y_given_T <- 1
E_Y_given_no_T <- P_S
E_Y <- E_Y_given_T * P_T + E_Y_given_no_T * (1 - P_T)
true_alpha <- (E_Y - E_Y_given_no_T) / E_Y
true_alpha
ATE <- E_Y_given_T - E_Y_given_no_T
ATE

set.seed(1)
n <- 10000

shared_data <-
  tibble(
    atp_seed = runif(n),
    strategic_seed = runif(n),
    epsilon = rnorm(n)
  )

generate_data <- function(shared_data, params) {
  shared_data %>%
    mutate(
      T_a = atp_seed < P_T,
      T_a_tilde = 0.5 - T_a + (epsilon * params$sd_eps),
      strategic_coin = strategic_seed < P_S,
      Y = case_when(strategic_coin | T_a ~ 1, TRUE ~ 0),
      Y_above = case_when(T_a ~ 1, TRUE ~ 0),
      group = params$group
    )
}

params_list <- tribble(
  ~group, ~sd_eps,
  "Baseline", 0.05,
  "Increase measurement error in treatment", 0.5
) %>%
  purrr::transpose()

tmp <- params_list %>% map_dfr(function(x) generate_data(shared_data, x))

# SE functions derived with delta method
se_bayes <- function(b, g, p, var_b, var_g, var_p) {
  sqrt((var_b * (g - p)^2 + var_g * (p - b)^2 + var_p * (b - g)^2) / (g - p)^4)
}

se_hat <- function(a, b, c, var_a, var_b, var_c) {
  sqrt((var_a * b^2 + var_b * a^2 + var_c * a^2 * b^2 / c^2) / c^2)
}


# Calculate ATE using regression
fit_baseline <- lm(Y ~ T_a_tilde,
  data = tmp %>%
    filter(group == "Baseline")
)
fit_noise <- lm(Y ~ T_a_tilde,
  data = tmp %>%
    filter(group == "Increase measurement error in treatment")
)
group <- c("Baseline", "Increase measurement error in treatment")
ate_measured <- c(
  summary(fit_baseline)$coefficients[2, 1] * -1,
  summary(fit_noise)$coefficients[2, 1] * -1
)
var_ate <- c(
  summary(fit_baseline)$coefficients[2, 2]^2,
  summary(fit_noise)$coefficients[2, 2]^2
)
df_ate_measured <- data.frame(group, ate_measured, var_ate)
df_ate_measured

# Calculate inputs for alpha_bayes
beta <-
  tmp %>%
  filter(Y == 1) %>%
  group_by(group) %>%
  summarise(
    beta = mean(T_a_tilde),
    var_beta = var(T_a_tilde) / n()
  )

gamma <-
  tmp %>%
  filter(Y_above == 1) %>%
  group_by(group) %>%
  summarise(
    gamma = mean(T_a_tilde),
    var_gamma = var(T_a_tilde) / n()
  )

phi <-
  tmp %>%
  group_by(group) %>%
  summarise(
    phi = mean(T_a_tilde),
    var_phi = var(T_a_tilde) / n()
  )

# Calculate both alphas
sim <- tmp %>%
  group_by(group) %>%
  summarise(
    prob_t_a_tilde = mean(T_a_tilde < 0),
    var_prob_t_a_tilde = var(T_a_tilde < 0) / n,
    prob_y = mean(Y),
    var_prob_y = var(Y) / n,
    rescale_measured = prob_t_a_tilde / prob_y
  ) %>%
  inner_join(df_ate_measured, by = "group") %>%
  inner_join(beta, by = "group") %>%
  inner_join(gamma, by = "group") %>%
  inner_join(phi, by = "group") %>%
  ungroup() %>%
  transmute(group,
    ate_measured,
    se_ate = var_ate^0.5,
    prob_t_a_tilde,
    prob_y,
    alpha_hat = ate_measured * rescale_measured,
    se_hat = se_hat(
      a = ate_measured,
      b = prob_t_a_tilde,
      c = prob_y,
      var_a = var_ate,
      var_b = var_prob_t_a_tilde,
      var_c = var_prob_y
    ),
    beta,
    gamma,
    phi,
    alpha_bayes = (beta - phi) / (gamma - phi),
    se_bayes = se_bayes(
      b = beta,
      g = gamma,
      p = phi,
      var_b = var_beta,
      var_g = var_gamma,
      var_p = var_phi
    )
  )

sim

# Plot results
df_label <- tibble(
  group = "Increase measurement error in treatment",
  estimate = factor("hat", c("hat", "bayes")),
  label = "true \U03B1"
)

sim %>%
  pivot_longer(
    cols = starts_with("alpha"),
    names_to = "estimate",
    names_prefix = "alpha_",
    values_to = "alpha"
  ) %>%
  pivot_longer(
    cols = starts_with("se"),
    names_to = "estimate_se",
    names_prefix = "se_",
    values_to = "se"
  ) %>%
  mutate(
    se = if_else(estimate == estimate_se,
      se,
      NA_real_
    ),
    estimate = factor(estimate, c("hat", "bayes"))
  ) %>%
  filter(!is.na(se) | estimate == "true") %>%
  select(-estimate_se) %>%
  unique() %>%
  mutate(a_true = case_when(
    group == "Baseline" ~ true_alpha,
    group == "Increase measurement error in treatment" ~ true_alpha
  )) %>%
  ggplot(aes(x = estimate, y = alpha)) +
  geom_errorbar(aes(
    ymin = alpha - 1.96 * se,
    ymax = alpha + 1.96 * se
  ),
  width = 0.1
  ) +
  geom_point() +
  geom_hline(aes(
    yintercept = a_true,
    color = group
  ),
  linetype = "dashed"
  ) +
  geom_text(
    data = df_label,
    aes(
      x = estimate,
      y = true_alpha + 0.01,
      label = label
    ),
    color = grey_palette[7],
    hjust = 1.5
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_x_discrete(labels = c("Back-of-the-envelope \nmethod", "Bayes method")) +
  scale_color_manual(values = c("#f7695f", "#6ea1c9")) +
  fte_theme() +
  labs(
    x = "",
    y = expression(paste(alpha, " estimate"))
  ) +
  theme(plot.title.position = "plot") +
  facet_wrap(~group)

ggsave("analysis/release/meas_error_sim/simulation_se.png",
  height = 4.5, width = 8
)
