source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")


all_delinquencies <- tibble(
  cid = numeric(),
  month = numeric(),
  status = numeric(),
  opendt = numeric(),
  bal_orig = numeric()
)

for (mcd_month in required_months) {
  current_status_all <- read_dta(str_c(
    stata_sample, "del_",
    mcd_month, ".dta"
  )) %>%
    transmute(
      cid = cnid,
      fm_1_status = li102,
      fm_1_opendt = li60,
      fm_1_bal_orig = li58,
      fm_2_status = li103,
      fm_2_opendt = li333,
      fm_2_bal_orig = li104,
      bcard_bal = li218,
      rcard_bal = li221,
      bcard_cur_bal = li239,
      rcard_cur_bal = li275,
      delinquent = fm_1_status %in% seq(2, 5) |
        fm_2_status %in% seq(2, 5)
    ) %>%
    filter(delinquent) %>%
    clean_na_efx() %>%
    efx_dates_as_mcdash_months() %>%
    group_by(cid) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(-delinquent) %>%
    mutate_at(vars(contains("card")), ~ replace(., is.na(.), 0)) %>%
    mutate(
      any_del_card =
        bcard_bal + rcard_bal > bcard_cur_bal + rcard_cur_bal,
      no_cards = bcard_bal + rcard_bal == 0
    ) %>%
    select(
      -bcard_bal,
      -rcard_bal,
      -bcard_cur_bal,
      -rcard_cur_bal
    )


  current_status_largest <-
    current_status_all %>%
    select(
      cid, contains("_1_"),
      any_del_card,
      no_cards
    ) %>%
    rename_all(~ str_remove(., "fm_1_")) %>%
    filter(status %in% seq(2, 5))

  current_status_2nd_largest <-
    current_status_all %>%
    select(
      cid, contains("_2_"),
      any_del_card,
      no_cards
    ) %>%
    rename_all(~ str_remove(., "fm_2_")) %>%
    filter(status %in% seq(2, 5))

  rm(current_status_all)

  all_delinquencies <-
    bind_rows(
      current_status_largest,
      current_status_2nd_largest
    ) %>%
    mutate(month = mcd_month) %>%
    bind_rows(all_delinquencies)

  rm(
    current_status_2nd_largest,
    current_status_largest
  )
  print(mcd_month)
}


saveRDS(all_delinquencies, str_c(temp_directory, "overall/efx_del_mult_", min(required_months), ".rds"))
# want first delinquencies and whether they are straight
