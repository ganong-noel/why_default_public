source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")

equity_df <- tibble(
  loan_id = numeric(),
  as_of_mon_id = numeric(),
  prin_bal_amt = numeric(),
  second_lien_bal = numeric(),
  total_balance = numeric(),
  no_double_count = numeric(),
  close_dt = numeric(),
  is_msa = logical(),
  prop_cbsa = numeric(),
  prop_zip = numeric(),
  appraisal_amt = numeric(),
  rel_inc_zip = numeric(),
  rel_inc_cbsa = numeric(),
  rel_inc = numeric(),
  LTV = numeric(),
  equity = numeric(),
  underwater = logical()
)

if (FALSE) {
  scratch_dir <- "/scratch/midway2/wuthenow/CRISM_temp_files/" # nolint
  crism_data <- "/project2/databases/crism-mcdash/" # nolint
  corelogic_dir <- "/project2/pnoel/data/" # nolint
}

stata_sample <- str_c(scratch_dir, "stata_sample/")
temp_directory <- str_c(scratch_dir, "r_output/")

hpi_cbsa_all <- read_csv(str_c(scratch_dir, "corelogic/hpi_cbsa.csv"))
hpi_zip_all <- read_csv(str_c(scratch_dir, "corelogic/hpi_zip.csv"))


loan_location <- read_rds(str_c(temp_directory, "overall/loan.rds")) %>%
  filter(mort_type %in% c(1, 4)) %>%
  select(loan_id,
    prop_cbsa = cbsa_md_id,
    prop_zip,
    appraisal_amt,
    close_dt
  )

if (load_for_second_lien) {
  matched_loans <-
    readRDS(str_c(temp_directory, "overall/matched_loans.rds"))
  all_loans <-
    readRDS(str_c(temp_directory, "overall/all_loans.rds"))
  loan_cid_corresp <-
    readRDS(str_c(temp_directory, "overall/loan_cid.rds"))
  balance_at_first_default <-
    readRDS(str_c(temp_directory, "overall/first_default.rds")) %>%
    semi_join(loan_cid_corresp)
  balance_at_foreclosure_start <-
    readRDS(str_c(temp_directory, "overall/foreclosure_start.rds")) %>%
    semi_join(loan_cid_corresp) %>%
    arrange(loan_id, as_of_mon_id) %>%
    group_by(loan_id) %>%
    filter(as_of_mon_id == first(as_of_mon_id)) %>%
    ungroup()
  balance_at_foreclosure_end <-
    readRDS(str_c(temp_directory, "overall/foreclosure_end.rds")) %>%
    semi_join(loan_cid_corresp) %>%
    arrange(loan_id, as_of_mon_id) %>%
    group_by(loan_id) %>%
    filter(as_of_mon_id == first(as_of_mon_id)) %>%
    ungroup()
}

# I start by producing a many-to-many association of loan_ids to second liens
corresp <- matched_loans %>%
  ungroup() %>%
  left_join(all_loans, by = "cid", suffix = c("_first", "_second")) %>%
  filter(loan_second %in% c("ces", "heloc")) %>%
  mutate(
    opened_later = opendt_first <= opendt_second + 3,
    lower_balance = bal_orig_first > bal_orig_second,
    plausible = opened_later & lower_balance
  ) %>%
  filter(plausible) %>%
  group_by(cid, bal_orig_second, opendt_second, loan_second) %>%
  mutate(
    no_double_count_ind =
      as.numeric(plausible * bal_orig_first == max(plausible * bal_orig_first))
  ) %>%
  ungroup() %>%
  select(
    loan_id = loan_id_first,
    cid,
    bal_orig = bal_orig_second,
    opendt = opendt_second,
    loan = loan_second,
    no_double_count_ind
  ) %>%
  left_join(tibble(
    no_double_count_ind = c(0, 1, 1),
    no_double_count = c(0, 1, 0)
  )) %>%
  select(-no_double_count_ind) %>%
  semi_join(loan_cid_corresp)

rm(all_loans)
matched_loans <- select(
  matched_loans,
  loan_id, cid, bal_orig, opendt, loan
)

for (mcd_month in required_months) {
  mb <- readRDS(str_c(
    temp_directory, "month/month_balance/mb_", mcd_month, ".rds"
  ))

  second_balance <- corresp %>%
    left_join(mb) %>%
    group_by(loan_id, no_double_count) %>%
    summarise(second_lien_bal = sum(bal, na.rm = T)) %>%
    ungroup()

  cummulative_upb <- read_dta(str_c(
    stata_sample, "defaulters/default_",
    mcd_month, ".dta"
  )) %>%
    rename(
      loan_id = LoanId,
      payment_status = PaymentStatus,
      investor_id = InvestorId
    ) %>%
    semi_join(matched_loans)

  cummulative_upb <- cummulative_upb %>%
    mutate(no_double_count = 0) %>%
    bind_rows(cummulative_upb %>%
      mutate(no_double_count = 1)) %>%
    left_join(second_balance) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    mutate(
      total_balance = UPB + second_lien_bal,
      as_of_mon_id = as.numeric(mcd_month)
    )

  rm(second_balance)
  # Every loan should have 2 entries here
  # just check on the first and every 10 tries
  if (mcd_month %in% seq(
    required_months[1],
    required_months[length(required_months)],
    10
  )) {
    testthat::test_that("each loan has two observations", expect_equal(cummulative_upb %>%
      group_by(loan_id) %>%
      tally() %>%
      pull(n) %>%
      mean(), 2))
  }

  hpi_cbsa <-
    hpi_cbsa_all %>%
    calc_rel_change_hpi(mcd_month)

  hpi_zip <-
    hpi_zip_all %>%
    calc_rel_change_hpi(mcd_month)

  equity_df <- cummulative_upb %>%
    left_join(loan_location) %>%
    left_join(hpi_zip,
      by = c(prop_zip = "prop_zip", close_dt = "hpi_dt")
    ) %>%
    rename(rel_inc_zip = rel_inc) %>%
    left_join(hpi_cbsa,
      by = c(prop_cbsa = "prop_cbsa", close_dt = "hpi_dt")
    ) %>%
    rename(rel_inc_cbsa = rel_inc) %>%
    mutate(
      rel_inc = ifelse(!is.na(rel_inc_zip), rel_inc_zip, rel_inc_cbsa),
      LTV = total_balance / (appraisal_amt * rel_inc),
      equity = 1 - LTV,
      underwater = equity < 0
    )

  print(mcd_month)

  rm(cummulative_upb)

  saveRDS(
    equity_df,
    str_c(temp_directory, "month/equity/equity_", mcd_month, ".rds")
  )

  rm(equity_df)
}


rm(
  loan_location, cummulative_upb, equity_df,
  second_balance, corresp, hpi_cbsa_all, hpi_zip_all,
  matched_loans
)

### Now calculate the counts for all of the months in required months

loan_file <- readRDS(str_c(temp_directory, "overall/loan.rds")) %>%
  select(
    LoanId = loan_id, close_dt,
    OccupancyId = occupancy_type,
    fico_orig, dti_ratio, cltv_orig = ltv_ratio,
    loan_type, io_flg, int_type, arm_init_rate,
    documentation_flg, orig_month, balloon_flg,
    mort_type
  )

first_defaults <- readRDS(str_c(temp_directory, "overall/first_default.rds"))

# loan level files
for (mcd_month in required_months) {
  loan_status <- read_dta(str_c(
    stata_sample, "defaulters/default_",
    mcd_month,
    ".dta"
  ))

  status_occupancy <- loan_file %>%
    inner_join(loan_status) %>%
    mutate(type = case_when(
      PaymentStatus %in% c("X") ~ "foreclosure complete",
      PaymentStatus %in% c("F") ~ "in foreclosure pre-sale",
      PaymentStatus %in% c("R") ~ "in foreclosure postsale or REO",
      PaymentStatus %in% c(3, 4) ~ "defaulter 90",
      TRUE ~ "other"
    )) %>%
    select(LoanId, close_dt, type, OccupancyId,
      fico_orig, dti_ratio, cltv_orig,
      loan_type, io_flg, int_type, arm_init_rate,
      current_rate = InterestRate, documentation_flg,
      orig_month, balloon_flg
    )

  rm(loan_status)

  equity_in_period <- readRDS(str_c(
    temp_directory,
    "month/equity/equity_",
    mcd_month, ".rds"
  )) %>%
    filter(!is.na(underwater)) %>%
    select(
      underwater, loan_id, no_double_count, second_lien_bal,
      prop_cbsa, LTV
    )


  loan_lv <- status_occupancy %>%
    inner_join(equity_in_period, by = c(LoanId = "loan_id")) %>%
    filter(
      LTV <= 2.5,
      no_double_count == 0
    ) %>%
    select(-no_double_count) %>%
    mutate(multiple_second_liens = second_lien_bal != 0) %>%
    left_join(loan_cid_corresp, by = c(LoanId = "loan_id")) %>%
    group_by(cid) %>%
    mutate(number_first_liens = n()) %>%
    mutate(month = mcd_month)


  rm(status_occupancy)

  rm(equity_in_period)

  saveRDS(
    loan_lv,
    str_c(temp_directory, "month/loan_lv/loan_lv", mcd_month, ".rds")
  )

  rm(loan_lv)

  gc()
}

foreclosure_start <- readRDS(str_c(temp_directory, "overall/foreclosure_start.rds"))

foreclosure_start <- balance_at_first_default %>%
  select(loan_id, del_dt = as_of_mon_id) %>%
  left_join(balance_at_foreclosure_start) %>%
  mutate(foreclosure_within_12 = if_else(!is.na(as_of_mon_id),
    as_of_mon_id - del_dt <= 12,
    FALSE
  ))



foreclosure_counts <- tibble(
  underwater = logical(),
  month = numeric(),
  foreclosure_within_12 = numeric(),
  n = numeric()
)


unmatched_sample_counts <- tibble(
  OccupancyId = numeric(),
  number_loans = numeric(),
  month = numeric(),
  type = character()
)

matched_sample_counts <- tibble(
  underwater = logical(),
  type = character(),
  number_loans = numeric(),
  month = numeric()
)

add_bins_80_100 <- function(df) {
  df %>%
    mutate(
      ltv_bin = case_when(
        LTV <= 0.8 ~ "(0.01% - 80%]",
        LTV <= 1 ~ "(80 - 100%]",
        LTV > 1 ~ "(100 - 300%]"
      ),
      ltv_bin = factor(ltv_bin,
        levels = c(
          "(100 - 300%]",
          "(80 - 100%]",
          "(0.01% - 80%]"
        )
      )
    ) %>%
    return()
}

for (mcd_month in required_months) {
  loan_status <- read_dta(str_c(
    stata_sample, "defaulters/default_",
    mcd_month,
    ".dta"
  ))

  status_occupancy <- loan_file %>%
    inner_join(loan_status) %>%
    mutate(type = case_when(
      PaymentStatus %in% c("X", "F", "R") ~ "in foreclosure",
      PaymentStatus %in% c(3, 4) ~ "defaulter",
      TRUE ~ "other"
    )) %>%
    select(LoanId, type, OccupancyId, mort_type)

  rm(loan_status)

  scratch_loans <- str_c(scratch_dir, "r_output/month/loan_lv")
  loan_i <- readRDS(str_c(
    scratch_loans,
    "/loan_lv", mcd_month, ".rds"
  )) %>%
    ungroup() %>%
    transmute(LoanId,
      cid,
      orig_year = mcd_to_year(orig_month)
    )

  unmatched_sample_counts <- status_occupancy %>%
    left_join(select(loan_i, LoanId, orig_year), by = "LoanId") %>%
    group_by(OccupancyId, type, mort_type, orig_year) %>%
    summarise(number_loans = n()) %>%
    mutate(month = mcd_month) %>%
    bind_rows(unmatched_sample_counts)

  equity_in_period <- readRDS(str_c(
    temp_directory,
    "month/equity/equity_",
    mcd_month, ".rds"
  )) %>%
    filter(!is.na(underwater)) %>%
    select(
      underwater, loan_id, no_double_count,
      second_lien_bal, LTV
    )

  matched_sample_counts <- status_occupancy %>%
    inner_join(equity_in_period, by = c(LoanId = "loan_id")) %>%
    mutate(
      multiple_second_liens = second_lien_bal != 0,
      round_ltv = round(LTV, digits = 1)
    ) %>%
    filter(LTV <= 3 & LTV > 0.01) %>%
    add_bins_80_100() %>%
    left_join(loan_cid_corresp, by = c(LoanId = "loan_id")) %>%
    left_join(loan_i, by = c("LoanId", "cid")) %>%
    mutate(subprime = mort_type == 4) %>%
    group_by(cid, no_double_count) %>%
    mutate(number_first_liens = n()) %>%
    group_by(
      underwater, type, no_double_count,
      OccupancyId, number_first_liens,
      multiple_second_liens, mort_type,
      ltv_bin, subprime, orig_year
    ) %>%
    summarise(number_loans = n()) %>%
    mutate(month = mcd_month) %>%
    bind_rows(matched_sample_counts)

  rm(status_occupancy, loan_i)

  foreclosure_counts <- equity_in_period %>%
    filter(no_double_count == 0) %>%
    mutate(month = mcd_month) %>%
    inner_join(foreclosure_start, by = c(
      month = "del_dt",
      loan_id = "loan_id"
    )) %>%
    group_by(underwater, foreclosure_within_12, month) %>%
    tally() %>%
    bind_rows(foreclosure_counts)
  rm(equity_in_period)

  gc()
}

saveRDS(matched_sample_counts, str_c(temp_directory, "overall/matched_counts_", required_months[1], ".rds"))
saveRDS(unmatched_sample_counts, str_c(temp_directory, "overall/unmatched_counts_", required_months[1], ".rds"))
saveRDS(foreclosure_counts, str_c(temp_directory, "overall/foreclosure_counts_", required_months[1], ".rds"))


map_dfr(required_months, ~ readRDS(str_c(
  temp_directory,
  "month/equity/equity_",
  .x, ".rds"
)) %>%
  filter(!is.na(underwater)) %>%
  semi_join(first_defaults %>%
    filter(as_of_mon_id == .x),
  by = "loan_id"
  ) %>%
  select(underwater, loan_id, as_of_mon_id, no_double_count, second_lien_bal, appraisal_amt, investor_id, LTV)) %>%
  saveRDS(str_c(temp_directory, "overall/equity_", required_months[1], ".rds"))
