library(haven)
library(testthat)
library(lubridate)
# note that this script is configured to be run from a bash script.
# with arguments passed from this script.

# If you are running this script interactively, run the code inside the FALSE
# if statement below.

if (FALSE) {
  scratch_dir <- "/scratch/midway2/probert2/CRISM_temp_files/" # nolint
  crism_data <- "/project2/databases/crism-mcdash/" # nolint
  corelogic_dir <- "/project2/pnoel/data/" # nolint
}

source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")


mcd_to_year <- function(x) {
  floor(((1980 * 12) + x - 1) / 12)
}


add_bins <- function(df) {
  df %>%
    mutate(
      LTV_binned = case_when(
        LTV <= 0.6 ~ "<= 60%",
        LTV <= 0.7 ~ "61 - 70%",
        LTV <= 0.8 ~ "71 - 80%",
        LTV <= 0.9 ~ "81 - 90%",
        LTV <= 1 ~ "91 - 100%",
        LTV > 1 ~ "> 100%"
      ),
      LTV_binned = factor(LTV_binned,
        levels = c(
          "<= 60%",
          "61 - 70%",
          "71 - 80%",
          "81 - 90%",
          "91 - 100%",
          "> 100%"
        )
      )
    ) %>%
    return()
}

extended_add_bins <- function(df) {
  df %>%
    mutate(
      LTV_binned = case_when(
        LTV <= 0.2 ~ "0 - 20%",
        LTV <= 0.4 ~ "21 - 40%",
        LTV <= 0.6 ~ "41 - 60%",
        LTV <= 0.8 ~ "61 - 80%",
        LTV <= 1 ~ "81 - 100%",
        LTV <= 1.2 ~ "101 - 120%",
        LTV <= 1.4 ~ "121 - 140%",
        LTV <= 1.6 ~ "141 - 160%",
        LTV <= 1.8 ~ "161 - 180%",
        LTV <= 2 ~ "181 - 200%",
        LTV <= 2.2 ~ "201 - 220%",
        LTV > 2.2 ~ "> 220%"
      ),
      LTV_binned = factor(LTV_binned,
        levels = c(
          "0 - 20%",
          "21 - 40%",
          "41 - 60%",
          "61 - 80%",
          "81 - 100%",
          "101 - 120%",
          "121 - 140%",
          "141 - 160%",
          "161 - 180%",
          "181 - 200%",
          "201 - 220%",
          "> 220%"
        )
      )
    ) %>%
    return()
}

stata_sample <- str_c(scratch_dir, "stata_sample/")
temp_directory <- str_c(scratch_dir, "r_output/")

if (load_for_plot) {
  cid_observed_dates <-
    readRDS(str_c(temp_directory, "overall/cid_observed.rds"))
  equity <- readRDS(str_c(temp_directory, "overall/equity.rds"))
  loan_cid_corresp <-
    readRDS(str_c(temp_directory, "overall/loan_cid.rds"))
  loan_orig <- readRDS(str_c(temp_directory, "overall/loan.rds")) %>%
    select(loan_id, dti_ratio, appraisal_amt, fico_orig, orig_amt,
      prop_state, prop_zip, close_dt, occupancy_type, mort_type,
      termination_dt,
      termination_type = termiation_type
    )
  matched_loans <- readRDS(str_c(temp_directory, "overall/matched_loans.rds"))
}




equity %>%
  filter(no_double_count == 1) %>%
  mutate(no_double_count = 0) %>%
  anti_join(equity, by = c("loan_id", "no_double_count")) %>%
  rbind(equity) %>%
  filter(
    !is.na(LTV),
    appraisal_amt != 0,
    appraisal_amt < 3000000,
    LTV < 2.5
  ) %>%
  left_join(loan_orig %>% select(loan_id, mort_type)) %>%
  filter(mort_type %in% c(1, 4)) %>%
  inner_join(matched_loans) %>%
  arrange(loan_id, bal_orig) %>%
  group_by(loan_id) %>%
  filter(row_number() == 1) %>%
  ungroup() -> equity


balance_at_first_default <-
  readRDS(str_c(temp_directory, "overall/first_default.rds")) %>%
  semi_join(loan_cid_corresp)

equity_at_default <- equity %>%
  left_join(balance_at_first_default) %>%
  select(
    loan_id,
    LTV,
    cid,
    as_of_mon_id
  )


for_plot <- equity %>%
  select(loan_id) %>%
  left_join(equity_at_default) %>%
  filter(
    !is.na(as_of_mon_id),
    !is.na(LTV)
  ) %>%
  mutate(
    year = mcd_to_year(as_of_mon_id),
    month = as_of_mon_id - (year - 1980) * 12,
    month = str_pad(month, width = 2, side = "left", pad = "0"),
    date = lubridate::ymd(str_c(year, month, "01"))
  ) %>%
  select(-as_of_mon_id, -year, -month)



for_plot %>%
  add_bins() %>%
  group_by(date, LTV_binned) %>%
  summarise(number_loans = n()) %>%
  group_by(date) %>%
  mutate(proportion_loans = number_loans / sum(number_loans)) %>%
  ggplot() +
  aes(date, proportion_loans, fill = fct_rev(LTV_binned)) +
  geom_area() +
  fte_theme() +
  labs(
    fill = "LTV at default",
    x = NULL
  ) +
  scale_fill_manual(values = c(
    "#2B83BA",
    RColorBrewer::brewer.pal(5, "Greens")[5:1]
  )) +
  scale_y_continuous(
    name = "Share of defaulters",
    labels = scales::percent
  ) +
  scale_x_date(name = NULL, date_breaks = "2 years", date_labels = "%Y") +
  theme(
    panel.ontop = TRUE,
    panel.grid.major = element_line(colour = "white"),
    panel.background = element_rect(fill = NA),
    legend.position = "right",
    legend.title = element_text()
  )


ggsave("analysis/release/CRISM/share_by_LTV.png",
  width = 8, height = 4.5, unit = "in"
)

# Share of defaulter with LTV >100%, > 200% and >220%
default_binned_extended <- for_plot %>%
  filter(
    date >= ymd("20080101"),
    date <= ymd("20150831")
  ) %>%
  extended_add_bins() %>%
  group_by(LTV_binned) %>%
  summarise(number_loans = n()) %>%
  ungroup() %>%
  mutate(proportion_loans = number_loans / sum(number_loans))

share_220 <- default_binned_extended %>%
  filter(LTV_binned == "> 220%") %>%
  pull(proportion_loans)

share_200 <- default_binned_extended %>%
  filter(LTV_binned %in% c("201 - 220%", "> 220%")) %>%
  summarise(proportion_loans = sum(proportion_loans)) %>%
  pull(proportion_loans)

share_100 <- default_binned_extended %>%
  filter(!LTV_binned %in% c(
    "0 - 20%", "21 - 40%", "41 - 60%",
    "61 - 80%", "81 - 100%"
  )) %>%
  summarise(proportion_loans = sum(proportion_loans)) %>%
  pull(proportion_loans)

test_that(
  "Share of default with > 100%",
  expect_equal(share_100, 0.477,
    tolerance = 0.001
  )
)

test_that(
  "Share of default with > 200%",
  expect_equal(share_200, 0.012,
    tolerance = 0.001
  )
)

test_that(
  "Share of default with > 220%",
  expect_equal(share_220, 0.005,
    tolerance = 0.001
  )
)
