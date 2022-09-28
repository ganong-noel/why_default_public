library(rlang)
library(tidyverse)


collate_files <- function(type) {
  map_dfr(
    list.files(str_c(temp_directory, "overall"),
      str_c(type, "_[0-9]{3}"),
      full.names = TRUE
    ),
    ~ readRDS(.x)
  ) %>%
    write_rds(str_c(
      temp_directory, "overall/",
      type, ".rds"
    ))
}

mcd_to_year <- function(x) {
  floor(((1980 * 12) + x - 1) / 12)
}

mcd_to_month <- function(x) {
  {
    x - (mcd_to_year(x) - 1980) * 12
  } %>%
    str_pad(2, "left", 0)
}

##### Multiply used functions #####
parse_lvar <- function(loan_type, loan_number, variable) {
  if (loan_number != "overall") {
    rlang::parse_expr(str_c(loan_type, "_", loan_number, "lrg_", variable))
  } else if (loan_number == "overall") {
    rlang::parse_expr(str_c(loan_type, "_", variable))
  }
}


##### Functions from do files 0 ####
import_equifax_main <- function(data_path) {
  read_csv(data_path,
    col_types = cols_only(
      loan_id = col_double(),
      period = col_double(),
      cnid = col_double(),
      efx_period = col_double(),
      zipcode = col_double(),
      li56 = col_double(),
      li57 = col_double(),
      li58 = col_double(),
      li59 = col_double(),
      li60 = col_double(),
      li62 = col_double(),
      li63 = col_double(),
      li67 = col_double(),
      li68 = col_double(),
      li93 = col_character(),
      li104 = col_double(),
      li112 = col_double(),
      li215 = col_double(),
      li216 = col_double(),
      li332 = col_double(),
      li333 = col_double(),
      li334 = col_double(),
      li335 = col_double(),
      li336 = col_double(),
      li337 = col_double()
    )
  ) %>%
    select(
      loan_id,
      cid = cnid,
      zipcode,
      efx_month = efx_period,
      addr_code = li93,
      ces_bal_orig = li68,
      ces_num = li62,
      ces_lrg_opendt = li334,
      ces_2lrg_opendt = li335,
      fm_bal = li57,
      fm_bal_orig = li332,
      fm_num = li56,
      fm_lrg_bal = li59,
      fm_lrg_bal_orig = li58,
      fm_lrg_opendt = li60,
      fm_2lrg_bal = li112,
      fm_2lrg_bal_orig = li104,
      fm_2lrg_opendt = li333,
      heloc_bal = li67,
      heloc_num = li63,
      heloc_lrg_opendt = li336,
      heloc_2lrg_opendt = li337,
      auto_bal_bank = li215,
      auto_bal_fin = li216
    ) %>%
    return()
}

import_equifax_extra <- function(data_path) {
  read_csv(data_path,
    col_types = cols_only(
      loan_id = col_double(),
      cnid = col_double(),
      efx_period = col_double(),
      conf = col_double(),
      bcn50 = col_double(),
      li66 = col_double(),
      li69 = col_double(),
      li94 = col_double(),
      li110 = col_double(),
      li111 = col_double(),
      li113 = col_double(),
      li114 = col_double(),
      li115 = col_double(),
      li116 = col_double(),
      li322 = col_double(),
      li323 = col_double()
    )
  ) %>%
    select(
      loan_id,
      cid = cnid,
      efx_month = efx_period,
      addr_dt = li94,
      ficov5 = bcn50,
      ces_bal = li66,
      heloc_bal_orig = li69,
      # this is a credit limit
      ces_lrg_bal = li113,
      ces_lrg_bal_orig = li110,
      ces_2lrg_bal = li114,
      ces_2lrg_bal_orig = li111,
      heloc_lrg_bal = li115,
      heloc_lrg_bal_orig = li322,
      heloc_2lrg_bal = li116,
      heloc_2lrg_bal_orig = li323
    ) %>%
    return()
}

import_mcdash_loan <- function(data_path) {
  read_dta(data_path) %>%
    select(
      # nolint start
      # cannot avoid the variables on RHS being camel case
      loan_id = LoanId,
      close_dt = ClosingMonth,
      orig_amt = OriginalLoanAmount,
      prop_type = PropertyTypeId,
      prop_state = State,
      prop_zip = ZIP,
      cbsa_md_id = CBSA_MetroDivId,
      appraisal_amt = OriginalPropertyValue,
      occupancy_type = OccupancyId,
      fico_orig = OriginalCreditScore,
      dti_ratio = DTIHousingRatio,
      mort_type = MortgageTypeId,
      loan_type = LoanTypeID,
      int_type = InterestTypeId,
      purpose_type_mcdash = PurposeOfLoanId,
      io_flg = IsIO,
      term_nmon = OriginalTerm,
      arm_init_rate = OriginalInterestRate,
      ltv_ratio = CLTV,
      termination_dt = PIFMonth,
      termiation_type = PIFId,
      documentation_flg = DocumentationId,
      orig_month = OriginationMonth,
      balloon_flg = BalloonId
      # nolint end
    ) %>%
    return()
}




import_mcdash_monthly <- function(data_path) {
  read_dta(data_path) %>%
    transmute(
      # nolint start
      # cannot avoid the variables on RHS being camel case
      loan_id = LoanId,
      as_of_mon_id = as.numeric(str_extract(data_path, "[0-9]{3}")),
      prin_bal_amt = UPB,
      mba_stat = PaymentStatus,
      # nolint end
    ) %>%
    return()
}

clean_na_efx <- function(data_frame) {
  data_frame %>%
    mutate_at(vars(matches("bal|lim")), ~ replace(., . >= 9999994, NA)) %>%
    mutate_at(vars(ends_with("num")), ~ replace(., . >= 96, 0)) %>%
    mutate_at(vars(ends_with("opendt")), ~ replace(., . >= 999994, NA)) %>%
    return()
}





efx_dates_as_mcdash_months <- function(data_frame) {
  #' The dates are originally in a numeric format with the written as YYYYMM
  #' The McDash time variables are expressed as months since 1980
  data_frame %>%
    mutate_at(
      vars(contains("dt")),
      ~ floor(. / 100) * 12 + . - floor(. / 100) * 100 - 23760
    )
}





add_third_largest_balances <- function(data_frame, loan_types) {
  for (loan_type in loan_types) {
    loan_num <- parse_lvar(loan_type, "overall", "num")
    for (variable in c("bal", "bal_orig")) {
      data_frame <- data_frame %>%
        mutate(
          !!parse_lvar(loan_type, 3, variable) :=
            !!parse_lvar(loan_type, "overall", variable) -
              !!parse_lvar(loan_type, 1, variable) -
                !!parse_lvar(loan_type, 2, variable),
          !!parse_lvar(loan_type, 3, variable) :=
            ifelse(
              !!parse_lvar(loan_type, 3, variable) > 0,
              !!parse_lvar(loan_type, 3, variable),
              0
            )
        )
    }
  }
  return(data_frame)
}


add_third_largest_date <- function(data_frame, loan_types) {
  #' This function takes a dataframe of consumer-row observations for 2 months
  #' and adds the opening date for the third largest loan in the later month.
  #'
  #' It is a complicated task because the third largest loan in a given period
  #' may not be the third largest loan in the previous period.
  #'
  #' If it is not, we still want the loan to have the correct origination date.
  #'
  #' As such I need to check against the origination balances of all loans in the
  #' previous period.
  #'
  #' The function takes a vector of loan types ("ces", "fm" and "heloc") as an
  #' argument, these loan types are used to parse variable names for checking
  #' and adding dates.

  for (loan_type in loan_types) {
    loan_num <- parse_lvar(loan_type, "overall", "num")
    data_frame <- data_frame %>%
      mutate(
        !!parse_lvar(loan_type, 3, "opendt") :=
          if_else(
            (mcd_month == efx_month) & (lag(cid) == cid), # no need to group_by
            case_when(
              is.na(!!parse_lvar(loan_type, 3, "bal_orig")) ~ NA_real_,
              !!parse_lvar(loan_type, 3, "bal_orig") == 0 ~ NA_real_,
              !!parse_lvar(loan_type, 3, "bal_orig") ==
                lag(!!parse_lvar(loan_type, 3, "bal_orig")) ~
                lag(!!parse_lvar(loan_type, 3, "opendt")),
              is.na(!!parse_lvar(loan_type, 3, "opendt")) &
                ((!!loan_num - lag(!!loan_num)) >= 1) ~ as.double(mcd_month),
              !!parse_lvar(loan_type, 3, "opendt") != 0 &
                !!parse_lvar(loan_type, 1, "opendt") ==
                  lag(!!parse_lvar(loan_type, 1, "opendt")) &
                !!parse_lvar(loan_type, 2, "opendt") ==
                  lag(!!parse_lvar(loan_type, 2, "opendt")) ~ as.double(mcd_month),
              !!parse_lvar(loan_type, 3, "bal_orig") ==
                lag(!!parse_lvar(loan_type, 2, "bal_orig")) ~
                lag(!!parse_lvar(loan_type, 2, "opendt")),
              !!parse_lvar(loan_type, 3, "bal_orig") ==
                lag(!!parse_lvar(loan_type, 1, "bal_orig")) ~
                lag(!!parse_lvar(loan_type, 1, "opendt"))
            ),
            !!parse_lvar(loan_type, 3, "opendt")
          ),
        !!parse_lvar(loan_type, 3, "bal_orig") :=
          ifelse(
            !!parse_lvar(loan_type, 3, "bal_orig") == 0,
            NA_real_,
            !!parse_lvar(loan_type, 3, "bal_orig")
          )
      )
  }
  return(data_frame)
}





add_match_for_second_lien <- function(merged_month_data_frame,
                                      bal_thresh = 1000,
                                      date_thresh = 4) {
  #' This function finds whether a given second lien could be associated with a
  #' first lien in the data. The matching is as in the BHFV code (which matches)
  #' ignoring the date for third liens
  merged_month_data_frame %>%
    rename_all(~ sub("_lrg", "_1lrg", .)) %>%
    add_third_largest_balances(c("fm", "heloc", "ces")) %>%
    mutate(
      bal_diff1 = abs(orig_amt - fm_1lrg_bal_orig),
      bal_diff2 = abs(orig_amt - fm_2lrg_bal_orig),
      bal_diff3 = abs(orig_amt - fm_3lrg_bal_orig),
      date_diff1 = abs(fm_1lrg_opendt - close_dt),
      date_diff2 = abs(fm_2lrg_opendt - close_dt)
    ) %>%
    mutate(
      match = case_when(
        bal_diff1 < 1 &
          date_diff1 == 0 ~ 1,
        bal_diff2 < 1 &
          date_diff2 == 0 ~ 2,
        bal_diff1 < bal_thresh &
          date_diff1 <= date_thresh &
          (date_diff1 <= date_diff2 |
            is.na(date_diff2)) ~ 1,
        bal_diff2 < bal_thresh &
          date_diff2 <= date_thresh ~ 2,
        bal_diff3 < bal_thresh ~ 3,
        TRUE ~ NA_real_
      )
    ) %>%
    return()
}

add_plausible_second_lien <- function(matched_data_frame,
                                      largest = FALSE) {
  #' This function returns a dataframe which for each loan_id records the
  #' total balance of second liens for each. The "largest" option refers to
  #' whether the balance of second liens should only be added to the largest
  #' first lien it matches, or whether it should be added to all of them.
  matched_data_frame %>%
    select(
      loan_id,
      cid,
      match,
      contains("heloc"),
      contains("ces"),
      contains("fm"),
      efx_month
    ) %>%
    mutate(
      mort_orig = case_when(
        match == 1 ~ fm_1lrg_bal_orig,
        match == 2 ~ fm_2lrg_bal_orig,
        match == 3 ~ fm_3lrg_bal_orig
      ),
      mort_date = case_when(
        match == 1 ~ fm_1lrg_opendt,
        match == 2 ~ fm_2lrg_opendt,
        match == 3 ~ 10000
      )
    ) %>%
    select(
      contains("lrg"),
      contains("mort"),
      -contains("fm"),
      -ends_with("num"),
      match,
      loan_id,
      cid,
      efx_month
    ) %>%
    filter(!is.na(mort_orig)) -> match_on


  match_on %>%
    reshape_loans(c("heloc", "ces")) %>%
    left_join(match_on %>% select(-contains("heloc|ces"))) %>%
    distinct() %>% # want to avoid multiple counting from joins
    filter(!is.na(bal)) %>%
    mutate(
      opened_later = mort_date <= opendt + 3,
      lower_balance = mort_orig > bal_orig,
      plausible = opened_later &
        lower_balance
    ) -> plausible_second_liens

  if (!largest) {
    plausible_second_liens %>%
      mutate(second_balance_sum = ifelse(plausible, bal, 0)) %>%
      group_by(loan_id) %>%
      summarise(second_balance = sum(second_balance_sum, na.rm = T)) %>%
      return()
  } else {
    plausible_second_liens %>%
      group_by(cid, bal_orig, opendt) %>% # these identify a loan
      # if there are multiple matches, for a given second lien we want to match
      # to the largest first lien (4-match) sorts the matches in reverse order
      mutate(
        plausible =
          plausible * (4 - match) == max(plausible * (4 - match))
      ) %>%
      mutate(second_balance_sum = ifelse(plausible, bal, 0)) %>%
      group_by(loan_id) %>%
      summarise(second_balance = sum(second_balance_sum, na.rm = T)) %>%
      return()
  }
}



correct_origination_balances <-
  function(data_frame, loan_type_and_nums) {
    data_frame <-
      data_frame %>%
      mutate(same_cid = cid == lag(cid)) # avoid calculating each loop
    for (loan_type in loan_type_and_nums) {
      loan_orig <- rlang::parse_expr(str_c(loan_type, "_bal_orig"))
      loan_dt <- rlang::parse_expr(str_c(loan_type, "_opendt"))
      loan_num <-
        rlang::parse_expr(str_replace(loan_type, "[1-2]lrg", "num"))

      data_frame %>% mutate(
        same_loan = !!loan_dt == lag(!!loan_dt) &
          same_cid &
          !!loan_num == lag(!!loan_num) &
          abs(!!loan_orig / lag(!!loan_orig) - 1) < 0.2,
        !!loan_orig := ifelse(same_loan %in% TRUE,
          lag(!!loan_orig),
          !!loan_orig
        )
      ) -> data_frame
    }
    return(data_frame)
  }


correct_num_from_dates <- function(data_frame, loan_types) {
  for (loan_type in loan_types) {
    open1 <- rlang::parse_expr(str_c(loan_type, "_1lrg_opendt"))
    open2 <- rlang::parse_expr(str_c(loan_type, "_2lrg_opendt"))
    num <- rlang::parse_expr(str_c(loan_type, "_num"))

    data_frame %>% mutate(
      !!num := case_when(
        is.missing(!!open1) & is.missing(!!open2) ~ 0,
        xor(is.missing(!!open1), is.missing(!!open2)) ~ 1,
        !is.missing(!!open1) & !is.missing(!!open2) & !!num < 2 ~ 2,
        TRUE ~ !!num
      )
    ) -> data_frame
  }
  return(data_frame)
} # This function is no longer used.



clean_efx_against_prev_month <- function(full_data_frame) {
  full_data_frame %>%
    rename_all(~ sub("_lrg", "_1lrg", .)) %>%
    correct_origination_balances(c(
      "fm_1lrg",
      "fm_2lrg",
      "ces_1lrg",
      "ces_2lrg",
      "heloc_1lrg",
      "heloc_2lrg"
    )) %>%
    return()
}


reshape_loans <- function(data_frame, loan_types) {
  map(loan_types, ~ filter_to_variable(data_frame, .x)) %>%
    map(split_to_number) %>%
    flatten() %>%
    bind_rows()
}

filter_to_variable <- function(data_frame, loan_type) {
  data_frame %>%
    select(
      contains(loan_type),
      cid,
      efx_month
    ) %>%
    select(
      contains("lrg"),
      cid,
      efx_month
    ) %>%
    mutate(loan = loan_type)
}

split_to_number <- function(data_frame) {
  map(
    as.character(1:3),
    ~ data_frame %>%
      select(contains(.x), cid, loan, efx_month) %>%
      rename_all(~ sub(".*lrg_", "", .)) %>%
      filter(!is.na(bal), !is.na(bal_orig), !is.na(opendt))
  )
}

##### Functions for HPI cleaning #####
calc_rel_change_hpi <- function(hpi_df, mcd_month) {
  hpi_df %>%
    filter(hpi_dt == mcd_month) %>%
    rename(hpi_now = hpi) %>%
    select(-hpi_dt) %>%
    right_join(hpi_df) %>%
    rename(hpi_orig = hpi) %>%
    mutate(rel_inc = hpi_now / hpi_orig) %>%
    select(-hpi_now, -hpi_orig)
}
