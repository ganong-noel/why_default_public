library(tidyverse)
library(haven)
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


balance_at_first_default <-
  tibble(
    loan_id = numeric(),
    as_of_mon_id = numeric(),
    prin_bal_amt = numeric()
  )


balance_at_foreclosure_start <-
  tibble(
    loan_id = numeric(),
    as_of_mon_id = numeric(),
    prin_bal_amt = numeric()
  )


balance_at_foreclosure_end <-
  tibble(
    loan_id = numeric(),
    as_of_mon_id = numeric(),
    prin_bal_amt = numeric()
  )

time_at_first_30 <-
  tibble(
    loan_id = numeric(),
    as_of_mon_id = numeric()
  )

cured_loans <-
  tibble(
    loan_id = numeric(),
    as_of_mon_id = numeric(),
    mba_stat = character()
  )


for (mcd_month in required_months) {
  # read in the mcd_month file
  a <- Sys.time()

  month_data <- import_mcdash_monthly(str_c(
    stata_sample,
    "defaulters/default_",
    mcd_month,
    ".dta"
  ))


  month_data %>%
    filter(mba_stat == "1") %>%
    anti_join(time_at_first_30, by = "loan_id") %>%
    select(loan_id, as_of_mon_id) %>%
    rbind(time_at_first_30) -> time_at_first_30


  month_data %>%
    filter(mba_stat %in% c("3", "4")) %>%
    anti_join(balance_at_first_default, by = "loan_id") %>%
    select(loan_id, as_of_mon_id, prin_bal_amt) %>%
    rbind(balance_at_first_default) -> balance_at_first_default


  month_data %>%
    filter(mba_stat %in% c("P", "C", "1", "2")) %>%
    semi_join(balance_at_first_default, by = "loan_id") %>%
    anti_join(cured_loans, by = "loan_id") %>%
    rbind(cured_loans) -> cured_loans


  month_data %>%
    filter(mba_stat %in% c("F")) %>%
    anti_join(balance_at_foreclosure_start, by = "loan_id") %>%
    semi_join(balance_at_first_default, by = "loan_id") %>% # need to observe foreclosure after default
    select(loan_id, as_of_mon_id, prin_bal_amt) %>%
    rbind(balance_at_foreclosure_start) -> balance_at_foreclosure_start

  month_data %>%
    filter(mba_stat %in% c("X")) %>%
    anti_join(balance_at_foreclosure_end, by = "loan_id") %>%
    select(loan_id, as_of_mon_id, prin_bal_amt) %>%
    rbind(balance_at_foreclosure_end) -> balance_at_foreclosure_end

  rm(month_data)
  gc()

  print(mcd_month)
  print(Sys.time() - a)
}

saveRDS(
  balance_at_first_default,
  str_c(
    temp_directory, "overall/first_default_",
    min(required_months), ".rds"
  )
)


saveRDS(
  balance_at_foreclosure_start,
  str_c(
    temp_directory, "overall/foreclosure_start_",
    min(required_months), ".rds"
  )
)


saveRDS(
  balance_at_foreclosure_end,
  str_c(
    temp_directory, "overall/foreclosure_end_",
    min(required_months), ".rds"
  )
)


saveRDS(
  cured_loans,
  str_c(
    temp_directory, "overall/cured_loans_",
    min(required_months), ".rds"
  )
)

saveRDS(
  time_at_first_30,
  str_c(
    temp_directory, "overall/time_30_",
    min(required_months), ".rds"
  )
)

del_90_with_straight <- balance_at_first_default %>%
  select(-prin_bal_amt) %>%
  left_join(time_at_first_30 %>% rename(del_30 = as_of_mon_id),
    by = "loan_id"
  ) %>%
  mutate(
    diff_first = as_of_mon_id - del_30,
    straight = diff_first == 2,
    long_del = diff_first >= 6,
    num_months_cat =
      case_when(
        diff_first == 2 ~ "2 months from first delinquency",
        diff_first %in% c(3, 4, 5, 6) ~
          "3 to 6 months from first delinquency",
        diff_first > 6
        ~ "More than 6 months from first delinquency"
      )
  ) %>%
  select(-diff_first) %>%
  mutate(
    straight = if_else(straight,
      "Three consecutive missed payments",
      "Residual"
    ),
    long_del = if_else(long_del, "Delinquent at t = -6",
      "Not delinquent at t = -6"
    )
  )


saveRDS(
  del_90_with_straight,
  str_c(
    temp_directory, "overall/straight_LPS_",
    min(required_months), ".rds"
  )
)
