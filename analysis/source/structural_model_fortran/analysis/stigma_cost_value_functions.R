# calculates the consumption cost of stigma at our optimal level calculated in
# `optimal_stigma.R`

rm(list = ls())
library(tidyverse)
library(yaml)
library(rprojroot)
library(readxl)
library(broom)
library(RColorBrewer) # nolint


make_path <- is_git_root$make_fix_file()
out_path <- make_path("analysis/release/structural_model_fortran/")


code_path <-
  make_path("analysis/source/structural_model_fortran/analysis/")

# sarmi_processing_functions.R relies on code_path
source(file.path(code_path, "sarmi_processing_functions.R"))

config <- yaml.load_file(make_path("analysis/config.yml"))
source("prelim.R")
data_path <- config$data_path$cc_simulation

pattern <- "stigma_0_bh_shock_0"
n <- 10

u_calculation <-
  read_multiple_sims(
    pattern,
    n,
    actions = c(
      "default",
      "cash_out",
      "no_action",
      "underwater_no_d",
      "no_longer_mortgagor"
    )
  ) %>%
  bin_sims() %>%
  mutate(moveshock = simmove == "move shock") %>%
  group_by(id, run) %>%
  mutate(moveshock = max(moveshock)) %>%
  ungroup() %>%
  filter(age == 1, moveshock == 0) %>%
  summarise(u = mean(simvstay))

mean_u <- u_calculation$u

our_stigma <-
  read_csv(file.path(out_path, "optimal_stigma_value.csv"))[[1]]
stigma_values <- c(-0.05, -our_stigma)
stigma_cost <- map(
  stigma_values,
  function(stigma) {
    tibble(
      stigma_val = stigma,
      pct_change = -stigma / (mean_u + stigma)
    )
  }
) %>%
  bind_rows()
write_csv(stigma_cost, file.path(out_path, "stigma_cost.csv"))
