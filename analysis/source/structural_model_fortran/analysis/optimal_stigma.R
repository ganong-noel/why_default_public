# compares runs of the model with different stigma values to the Chase data to
# determine which stigma value creates the best fit

library(tidyverse)
library(yaml)
library(rprojroot)
library(readxl)
library(broom)
library(RColorBrewer) # nolint


make_path <- is_git_root$make_fix_file()

code_path <-
  make_path("analysis/source/structural_model_fortran/analysis/")

# sarmi_processing_functions.R relies on code_path
source(file.path(code_path, "sarmi_processing_functions.R"))

config <- yaml.load_file(make_path("analysis/config.yml"))
source("prelim.R")
data_path <- config$data_path$cc_simulation
out_path <- make_path("analysis/release/structural_model_fortran/")

gn_data_inc <-
  read_excel(
    make_path(
      "analysis/input/disclose/latest/gn_strategic_latest.xls"
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
      mutate(title = pattern) %>%
      filter(
        move_shock == "no move shock",
        time_to_exit %in% c(0, 4)
      ) %>%
      filter(!ltv_bins_unadjusted %in% c("< 41%", "41-60%", "61-80%")) %>%
      mutate(
        ltv_bins = fct_recode(
          ltv_bins_unadjusted,
          "< 100%" = "81-100%"
        ),
        ltv_bins = fct_drop(ltv_bins, only = c("< 41%", "41-60%", "61-80%"))
      ) %>%
      group_by(ltv_bins) %>%
      transmute(
        time_to_exit = ifelse(time_to_exit != 0, -12, 0),
        normalized_delta_inc = (income - last(income)) / mpreal,
        source = "Model",
        n
      ) %>%
      bind_rows(gn_data_inc) %>%
      ungroup() %>%
      mutate(ltv_bins = fct_rev(ltv_bins)) %>%
      filter(time_to_exit == 0)
  }


inc_drop_diff <- function(data) {
  sq_dif_df <-
    data %>%
    dplyr::select(-ref, -run, -n) %>%
    filter(ltv_bins != "< 100%") %>%
    spread(key = source, value = normalized_delta_inc) %>%
    mutate(
      dif = Data - Model,
      dif2 = dif * dif
    ) %>%
    ungroup() %>%
    dplyr::select(ltv_bins, dif2) %>%
    unique()

  sum_dif <- sum(sq_dif_df$dif2)
}



####################
# Plot comparisons #
####################
namelist <- list()
namelist[1] <- "stigma_80_bh_shock_20"
namelist[2] <- "stigma_90_bh_shock_20"
namelist[3] <- "stigma_100_bh_shock_20"

patterns <- namelist


stigma_options_df <- map(
  patterns[1:3],
  function(pattern) {
    pattern %>%
      aggregate_mortgage_leaver_data(n = 10)
  }
)

stigma_options <- list()
for (i in seq(1, length(stigma_options_df), by = 1)) {
  stigma_options[i] <- inc_drop_diff(stigma_options_df[[i]])
}

optimal <-
  tibble(
    dif = as.numeric(stigma_options),
    run = namelist
  ) %>%
  arrange(dif)

stigma_value <-
  optimal %>%
  mutate(
    optimal_stigma = as.numeric(str_sub(run, 8, 9)),
    optimal_stigma = if_else(optimal_stigma == 10,
      1,
      optimal_stigma / 100
    )
  ) %>%
  filter(dif == min(dif)) %>%
  dplyr::select(optimal_stigma)

write_csv(stigma_value, file.path(out_path, "optimal_stigma_value.csv"))
