source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")


if (FALSE) {
  scratch_dir <- "/scratch/midway2/tcejka/CRISM_temp_files/"
  temp_directory <- "/scratch/midway2/tcejka/CRISM_temp_files/r_output/"
}


if (load_for_plot) {
  cid_observed_dates <-
    readRDS(str_c(temp_directory, "overall/cid_observed.rds"))
  equity <- readRDS(str_c(temp_directory, "overall/equity.rds"))
  loan_cid_corresp <-
    readRDS(str_c(temp_directory, "overall/loan_cid.rds"))
  loan_orig <- readRDS(str_c(temp_directory, "overall/loan.rds")) %>%
    select(loan_id, dti_ratio, appraisal_amt, fico_orig, orig_amt,
      prop_state, close_dt, occupancy_type, mort_type,
      termination_dt,
      termination_type = termiation_type
    )
  matched_loans <- readRDS(str_c(temp_directory, "overall/matched_loans.rds"))
  del_90_with_straight <- readRDS(str_c(
    temp_directory,
    "overall/del_90_with_key.rds"
  ))
}


stata_sample <- str_c(scratch_dir, "stata_sample/")
temp_directory <- str_c(scratch_dir, "r_output/")

# source Dobbie Goldsmith Pinkham (2015)
# "Debtor protection and the Great Recession"(table A-F)
non_recourse_states <- c(
  AK = 50,
  AZ = 2,
  CA = 4,
  IA = 14,
  MN = 22,
  MT = 25,
  ND = 33,
  OR = 36,
  WA = 46,
  WI = 48
)

observation_period <- cid_observed_dates %>%
  group_by(cid) %>%
  filter(n() <= 2) %>%
  ungroup() %>%
  spread(change_type, efx_month) %>%
  rename(
    first_observed = start,
    last_observed = stop
  ) %>%
  mutate(last_observed = if_else(is.na(last_observed),
    as.double(max(required_months)),
    as.double(last_observed)
  ))

mcd_to_year <- function(x) {
  floor(((1980 * 12) + x - 1) / 12)
}


nrow_equity_prior <- equity %>%
  filter(no_double_count == 0) %>%
  nrow()


test_that(
  "Correct number of loans in equity from read in",
  expect_equal(956804L, nrow_equity_prior)
)


equity <- equity %>%
  filter(
    !is.na(LTV),
    appraisal_amt != 0,
    appraisal_amt < 3000000,
    LTV < 2.5
  ) %>%
  left_join(loan_orig %>% select(loan_id, mort_type)) %>%
  filter(mort_type %in% c(1, 4)) %>%
  inner_join(del_90_with_straight %>%
    inner_join(matched_loans) %>%
    group_by(loan_id) %>%
    arrange(month, desc(bal_orig)) %>%
    filter(row_number() == 1))

nrow_equity_post <- equity %>%
  filter(no_double_count == 0) %>%
  nrow()

test_that(
  "31.2 sample with invalid appraisal, LTV for unable to be merged",
  expect_equal((nrow_equity_prior - nrow_equity_post) / nrow_equity_prior,
    0.312,
    tol = 0.001
  )
)


# Find the number of borrowers with LTV in 0-60,
# find average distance in Giacoletti standard deviation
# to underwater borrowers
test_that(
  "Errors are more than 3 standard deviation on average",
  equity %>%
    mutate(LTV = LTV * 100) %>%
    filter(
      LTV < 60,
      no_double_count == 0
    ) %>%
    summarise(mean_LTV = mean(LTV)) %>%
    pull(mean_LTV) %>%
    (function(x) (100 - x) / 18.7) %>%
    expect_equal(., 3.063, tol = 0.001)
)


first_lien_defaults <- equity %>%
  filter(no_double_count == 0) %>%
  select(loan_id, default_dt = month) %>%
  left_join(loan_cid_corresp) %>%
  select(cid, default_dt)


default_by_sample_all <- equity %>%
  mutate(
    default_month = month,
    default_year = mcd_to_year(default_month)
  ) %>%
  left_join(loan_cid_corresp) %>%
  left_join(observation_period) %>%
  mutate(
    period_before = default_month - first_observed,
    period_after = last_observed - default_month,
    twelve_sample = period_before >= 12 & period_after >= 3,
    eighteen_sample = period_before >= 18
  ) %>%
  gather(sample_type, in_sample, contains("sample")) %>%
  filter(in_sample)


test_that(
  "One observation per loan",
  expect_equal(default_by_sample_all %>%
    filter(sample_type == "twelve_sample") %>%
    filter(no_double_count == 0) %>%
    group_by(loan_id) %>%
    tally() %>%
    summarise(all(n == 1)) %>%
    pull(), TRUE)
)

reg_sample <- default_by_sample_all %>%
  filter(sample_type == "twelve_sample") %>%
  filter(no_double_count == 0) %>%
  select(loan_id, cid, default_month, underwater, default_year) %>%
  left_join(del_90_with_straight) %>%
  mutate(
    key_et_al =
      if_else(key_et_al,
        "Straight to 180dpd while current on revolving",
        "Not straight to 180dpd or not current on revolving"
      )
  )



plot_data <- reg_sample %>%
  mutate(
    year = mcd_to_year(default_month),
    month = default_month - (year - 1980) * 12,
    month = str_pad(month, width = 2, side = "left", pad = "0"),
    date = lubridate::ymd(str_c(year, month, "01")),
    date = lubridate::floor_date(date, unit = "quarter")
  ) %>%
  group_by(key_et_al, date, underwater) %>%
  tally() %>%
  filter(date < lubridate::ymd("20151001")) %>%
  group_by(date, underwater) %>%
  mutate(prop = n / sum(n))


plot_data %>%
  group_by(underwater) %>%
  filter(key_et_al == "Straight to 180dpd while current on revolving") %>%
  filter(date >= lubridate::ymd("20081001")) %>%
  summarise(prop = mean(prop)) %>%
  write_csv("analysis/release/CRISM/keys_stats.csv")

plot_data %>%
  filter(key_et_al == "Straight to 180dpd while current on revolving") %>%
  ungroup() %>%
  ggplot() +
  aes(date, prop, colour = underwater) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%b-%y") +
  labs(
    y = "Share of mortgage defaults that are\nstraight and otherwise current",
    colour = "Among:",
    x = NULL
  ) +
  scale_colour_manual(
    values = c(
      `FALSE` = "#ABDDA4",
      `TRUE` = "#2B83BA"
    ),
    labels = c(
      `FALSE` = "Above water defaulters",
      `TRUE` = "Underwater defaulters"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  fte_theme() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = rel(1.1))
  )


ggsave("analysis/release/CRISM/keys_replication.png", width = 6, height = 4)
