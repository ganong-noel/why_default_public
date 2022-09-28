# Produces Figure 7

source(file.path(code_path, "sarmi_processing_functions.R"))

gn_data_inc <-
  read_excel(
    make_path(
      chase_stats
    ),
    sheet = "tbl_reg_ltv_mortgage"
  ) %>%
  transmute(
    underwater_ltv_bins = labels,
    delta_inc = Estimate,
    ymin = delta_inc - 1.96 * `Cluster.s.e.`,
    ymax = delta_inc + 1.96 * `Cluster.s.e.`,
    ltv = str_extract(underwater_ltv_bins, "[0-9]{3}%"),
    ltv = str_extract(ltv, "[0-9]{3}"),
    ltv = (as.numeric(ltv) + 1),
    ltv_bins = bin_scores(ltv, 101, 141, 20),
    ltv_bins = fct_recode(ltv_bins, "< 100%" = "< 101%")
  ) %>%
  filter(!is.na(ltv_bins)) %>%
  group_by(ltv_bins) %>%
  summarise(zero = mean(delta_inc)) %>%
  mutate(minus_12 = 0) %>%
  gather(-ltv_bins,
    key = "time_to_exit",
    value = normalized_delta_inc
  ) %>%
  mutate(time_to_exit = ifelse(time_to_exit == "zero", 0, -12)) %>%
  transmute(
    ref = "gn",
    source = "Data",
    run = 0,
    ltv_bins,
    time_to_exit,
    normalized_delta_inc
  )


####################
# Read Data        #
####################

aggregate_mortgage_leaver_data <-
  function(pattern, n = 5) {
    aggregated_mortgage_leavers <-
      read_multiple_sims(pattern,
        n,
        actions = c(
          "default",
          "cash_out",
          "no_action",
          "underwater_no_d"
        )
      ) %>%
      bin_sims() %>%
      process_income_change_data() %>%
      mutate(title = pattern)
  }

####################
# Plot comparisons #
####################

patterns <-
  list(
    "stigma_0_bh_shock_0",
    "stigma_90_bh_shock_20"
  )
stigma_level <- c(0, .90)

inc_plots_data <- map2(
  patterns, stigma_level,
  function(pattern, stigma_level) {
    pattern %>%
      aggregate_mortgage_leaver_data(n = 10) %>%
      mutate(stigma = stigma_level)
  }
)

out_names <- list("no_stigma", "with_stigma")
list_num <- seq(1, 2)

map2(
  out_names, list_num,
  function(out_name, num) {
    make_income_drop_bar_two_panel(
      inc_plots_data[[num]],
      gn_data_inc
    )[[3]] %>%
      ggsave(
        filename =
          file.path(
            out_path,
            glue::glue("delta_inc_at_default_cc_v_chase_{out_name}.png")
          ),
        .,
        width = 8, height = 4.5
      )
  }
)