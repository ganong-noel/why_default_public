source("analysis/source/CRISM/R_scripts/00_prelim.R")

if (load_for_match) {
  equifax_open_term <-
    readRDS(str_c(temp_directory, "overall/efx_open_term.rds"))

  mcdash_loan <- readRDS(str_c(temp_directory, "overall/loan.rds")) %>%
    select(loan_id, close_dt, orig_amt, termination_dt)
  loan_cid_corresp <-
    readRDS(str_c(temp_directory, "overall/loan_cid.rds"))
}


equifax_open_term %>%
  filter(!too_many_loans) -> no_dropouts_df

rm(equifax_open_term)

# this is essentially just a spread (but I supect it will be faster )
efx_loan_level <- no_dropouts_df %>%
  filter(change_type == "open") %>%
  rename(
    orig_bal_observed = bal,
    opendt_observed = efx_month
  ) %>%
  select(-change_type) %>%
  full_join(
    no_dropouts_df %>%
      filter(change_type == "term") %>%
      rename(
        term_bal = bal,
        termdt = efx_month
      ) %>%
      select(-change_type)
  )

rm(no_dropouts_df)

efx_loan_level %>%
  ungroup() %>%
  left_join(loan_cid_corresp) %>%
  left_join(mcdash_loan) %>%
  mutate(
    abs_date_diff = abs(close_dt - opendt),
    abs_bal_diff = abs(orig_amt - bal_orig),
    term_date_diff = abs(termination_dt - termdt),
    match = abs_date_diff <= 1 & abs_bal_diff < 10000
  ) %>%
  arrange(desc(match), abs_date_diff, abs_bal_diff, term_date_diff) %>%
  group_by(cid, opendt, bal_orig, loan) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(loan_id = ifelse(match, loan_id, NA)) %>%
  select(
    cid, opendt, bal_orig, loan,
    termdt, term_bal, loan_id
  ) -> mcdash_equifax

rm(efx_loan_level)

all_loans <- mcdash_equifax %>%
  left_join(mcdash_loan)

rm(mcdash_equifax)

matched_loans <- all_loans %>%
  filter(!is.na(loan_id))

saveRDS(all_loans, str_c(temp_directory, "overall/all_loans.rds"))
saveRDS(
  matched_loans,
  str_c(temp_directory, "overall/matched_loans.rds")
)
