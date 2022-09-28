# Produces figure A-17

require(tidyverse)

Set2 <- brewer.pal("Set2", n = 7)
colors <- c(Set2[4], "black")
shapes <- c(18, 21)
our_names <- c("hh", "sim")
legend_labels <- c("Campbell and Cocco\n(2015)", "Replication")

names(colors) <- our_names
names(shapes) <- our_names
names(legend_labels) <- our_names

pattern <- "stigma_0_bh_shock_0"
title <- "HH: High yield rates, high income risk"
included_run <- "hh"

sims_data <-
  read_multiple_sims(
    pattern = "stigma_0_bh_shock_0",
    n = 10,
    actions = c(
      "default",
      "underwater_no_d",
      "cash_out",
      "no_action",
      "no_longer_mortgagor"
    )
  ) %>%
  ungroup()

sims_data <- process_simulation(sim_data = sims_data)

make_dot_chart(
  pattern,
  title,
  included_run,
  subtitle = "Diamond: Campbell and Cocco (2015); Circle: Replication",
  save_table_2_output = TRUE,
  sims_data = sims_data
)
