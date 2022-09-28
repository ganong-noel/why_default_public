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


stata_sample <- str_c(scratch_dir, "stata_sample/")
temp_directory <- str_c(scratch_dir, "r_output/")



loan_cid_corresp <- tibble(
  cid = numeric(),
  loan_id = numeric()
)

equifax_open_term <- tibble(
  cid = numeric(),
  opendt = numeric(),
  orig_bal = numeric(),
  efx_month = numeric(),
  balance = numeric(),
  loan_type = character(),
  change_type = character()
)


cid_observed_dates <- tibble(
  cid = numeric(),
  change_type = character(),
  efx_month = numeric()
)

new_addresses <- tibble(
  cid = numeric(),
  addr_dt = numeric()
)

for (mcd_month in required_months) {
  equifax_extra <- import_equifax_extra(str_c(
    stata_sample,
    "update_",
    mcd_month,
    ".csv"
  ))


  equifax_main <- import_equifax_main(str_c(
    stata_sample,
    "main_",
    mcd_month,
    ".csv"
  ))


  loan_cid_corresp <- equifax_main %>%
    select(loan_id, cid) %>%
    union(loan_cid_corresp)


  equifax_full <- equifax_main %>%
    left_join(equifax_extra, by = c("loan_id", "cid", "efx_month")) %>%
    select(-loan_id) %>%
    arrange(cid) %>%
    group_by(cid) %>%
    filter(row_number() == 1) %>%
    ungroup()


  rm(equifax_main, equifax_extra)

  equifax_with_clean_dates <- equifax_full %>%
    select(-zipcode, -addr_code, -ficov5) %>%
    clean_na_efx() %>%
    efx_dates_as_mcdash_months() %>%
    mutate(efx_month = efx_month + 120) %>% # equifax months
    rename_all(~ str_replace(., "_lrg", "_1lrg"))

  rm(equifax_full)

  equifax_with_clean_dates %>%
    select(cid, addr_dt) %>%
    union(new_addresses) -> new_addresses

  # CHECK TOO MANY LOANS
  if (mcd_month == required_months[1]) {
    equifax_with_clean_dates %>%
      filter(fm_num >= 3 | ces_num >= 3 | heloc_num >= 3) %>%
      pull(cid) -> too_many_loans

    equifax_with_clean_dates <- equifax_with_clean_dates %>%
      filter(
        fm_num < 3 | is.na(fm_num),
        ces_num < 3 | is.na(ces_num),
        heloc_num < 3 | is.na(heloc_num)
      ) %>%
      mutate(
        fm_3lrg_opendt = NA_real_,
        heloc_3lrg_opendt = NA_real_,
        ces_3lrg_opendt = NA_real_
      )

    equifax_two_months <- equifax_with_clean_dates
  } else {
    equifax_with_clean_dates %>%
      filter(fm_num > 3 | ces_num > 3 | heloc_num > 3) %>%
      pull(cid) %>%
      union(too_many_loans) -> too_many_loans

    equifax_two_months <- equifax_with_clean_dates %>%
      bind_rows(equifax_with_clean_dates_lag) %>%
      filter(
        fm_num <= 3 | is.na(fm_num),
        ces_num <= 3 | is.na(ces_num),
        heloc_num <= 3 | is.na(heloc_num)
      ) %>%
      group_by(cid) %>%
      mutate(enter_with_too_many = ((n() == 1) & (efx_month == mcd_month) &
        (fm_num == 3 | ces_num == 3 | heloc_num == 3))) %>%
      ungroup()

    equifax_two_months %>%
      filter(enter_with_too_many) %>%
      pull(cid) %>%
      union(too_many_loans) -> too_many_loans

    equifax_two_months <- filter(equifax_two_months, !enter_with_too_many) %>%
      select(-enter_with_too_many)

    rm(equifax_with_clean_dates_lag)
  }

  rm(equifax_with_clean_dates)

  equifax_two_months %>%
    group_by(cid) %>%
    filter(n_distinct(efx_month) == 1) %>%
    ungroup() %>%
    select(cid, efx_month) %>%
    mutate(change_type = ifelse(efx_month == mcd_month, "start", "stop")) %>%
    bind_rows(cid_observed_dates) -> cid_observed_dates

  loan_level_df <- equifax_two_months %>%
    arrange(desc(cid), efx_month) %>%
    clean_efx_against_prev_month() %>%
    select(
      cid,
      matches("fm|ces|heloc"),
      efx_month
    ) %>%
    add_third_largest_balances(c("fm", "ces", "heloc")) %>%
    add_third_largest_date(c("fm", "ces", "heloc")) %>%
    reshape_loans(c("fm", "ces", "heloc"))

  # Find open and termination dates
  loan_level_df %>%
    group_by(cid, opendt, bal_orig) %>%
    filter(n_distinct(efx_month) == 1) %>%
    ungroup() %>%
    mutate(change_type = ifelse(efx_month == mcd_month, "open", "term")) %>%
    select(cid, opendt, bal_orig, loan, efx_month, bal, change_type) %>%
    rbind(equifax_open_term) -> equifax_open_term # becomes efx side of match

  saveRDS(filter(loan_level_df, mcd_month == efx_month),
    str_c(temp_directory, "month/month_balance/mb_", mcd_month, ".rds"),
    compress = FALSE
  )
  rm(loan_level_df)

  equifax_with_clean_dates_lag <- equifax_two_months %>%
    filter(efx_month == mcd_month)

  rm(equifax_two_months)
}


equifax_open_term %>%
  mutate(too_many_loans = cid %in% too_many_loans) -> equifax_open_term
saveRDS(
  loan_cid_corresp,
  str_c(temp_directory, "overall/loan_cid.rds")
)
saveRDS(
  equifax_open_term,
  str_c(temp_directory, "overall/efx_open_term.rds")
)
saveRDS(
  cid_observed_dates,
  str_c(temp_directory, "overall/cid_observed.rds")
)

saveRDS(
  new_addresses,
  str_c(temp_directory, "overall/new_addresses.rds")
)
