if (FALSE) {
  setwd("/home/tcejka/repo/strategic/")
}

source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")

if (FALSE) {
  scratch_dir <- "/scratch/midway2/tcejka/CRISM_temp_files/"
  temp_directory <- "/scratch/midway2/tcejka/CRISM_temp_files/r_output/"
}

walk(c("equity", "del_90_with_key"), collate_files)

collate_files("equity")

map_dfr(
  list.files(str_c(temp_directory, "overall"),
    str_c("equity", "_[0-9]{3}"),
    full.names = TRUE
  ),
  ~ readRDS(.x)
) %>%
  arrange(loan_id, no_double_count, as_of_mon_id) %>%
  group_by(loan_id, no_double_count) %>%
  slice(1) %>% # only want first instance of default
  # This will have a large effect on sample size since
  # since observations are duplicated in the map/reduce
  ungroup() %>%
  saveRDS(str_c(
    temp_directory,
    "overall/equity.rds"
  ))




all_delinquencies <- map_dfr(
  list.files(str_c(temp_directory, "overall"),
    str_c("efx_del_mult", "_[0-9]{3}"),
    full.names = TRUE
  ),
  ~ readRDS(.x)
)

del_90_with_straight <- all_delinquencies %>%
  select(-any_del_card, -no_cards) %>%
  arrange(cid, opendt, bal_orig, month) %>%
  group_by(cid, opendt, bal_orig) %>%
  filter(first(status) == 2) %>%
  mutate(diff_first = month - first(month)) %>%
  ungroup() %>%
  filter(status %in% seq(4, 5)) %>%
  mutate(
    straight = diff_first == 2,
    long_del = diff_first >= 6
  ) %>%
  group_by(cid, opendt, bal_orig) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(
    num_months_cat =
      case_when(
        diff_first == 2 ~ "2 months from first delinquency",
        diff_first %in% c(3, 4, 5, 6) ~ "3 to 6 months from first delinquency",
        diff_first > 6 ~ "More than 6 months from first delinquency"
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

saveRDS(del_90_with_straight, str_c(
  temp_directory,
  "overall/straight_del_mult.rds"
))

del_90_with_straight %>%
  select(-status) %>%
  rename(default_month = month) %>%
  left_join(all_delinquencies) %>%
  mutate(months_since_default = month - default_month) %>%
  filter(months_since_default %in% seq(0, 3)) %>%
  arrange(cid, opendt, bal_orig, months_since_default) %>%
  group_by(cid, opendt, bal_orig) %>%
  mutate(
    always_current = !any(any_del_card),
    still_delinquent = sum(status) == 19,
    ever_card = any(!no_cards)
  ) %>%
  filter(months_since_default == 0) %>%
  mutate(key_et_al = always_current &
    still_delinquent &
    ever_card) %>%
  select(-months_since_default) %>%
  saveRDS(str_c(
    temp_directory,
    "overall/del_90_with_key.rds"
  ))
