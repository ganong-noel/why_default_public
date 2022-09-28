library(tidyverse)
source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")

if (FALSE) {
  scratch_loans <- "/scratch/midway2/wuthenow/CRISM_temp_files/r_output/month/loan_lv"
  scratch_overall <- "/scratch/midway2/wuthenow/CRISM_temp_files/r_output/overall"
}

scratch_loans <- str_c(scratch_dir, "r_output/month/loan_lv")
scratch_overall <- str_c(scratch_dir, "r_output/overall")

# To take a 10% of loans from loan_lv data: filter(str_sub(as.character(LoanId),-1,-1) == "1")

df <- map_dfr(337:428, ~ readRDS(str_c(
  scratch_loans,
  "/loan_lv", .x, ".rds"
)) %>%
  ungroup() %>%
  filter(type != "in foreclosure") %>%
  transmute(
    LoanId,
    month,
    prop_cbsa,
    orig_month,
    close_dt,
    in_default = if_else(type %in% c("foreclosure initiated", "defaulter 90"),
      1,
      0
    ),
    is_primary = case_when(
      OccupancyId == 1 ~ 1,
      OccupancyId == 9 ~ NA_real_,
      TRUE ~ 0
    ),
    fico_orig,
    dti_ratio,
    cltv_orig,
    LTV,
    is_io = io_flg,
    current_rate,
    full_documentation = case_when(
      documentation_flg == 1 ~ 1,
      documentation_flg == 9 ~ NA_real_,
      is.na(documentation_flg) ~ NA_real_,
      TRUE ~ 0
    ),
    is_arm = case_when(
      int_type %in% c(2, 3) ~ 1,
      TRUE ~ 0
    ),
    init_rate = arm_init_rate,
    is_balloon = case_when(
      balloon_flg == 1 ~ 1,
      balloon_flg == 9 ~ NA_real_,
      balloon_flg == 0 ~ 0
    )
  ))


in_path <- "analysis/input/data/guren"

gam <- read_csv(str_c(in_path, "/guren_df.csv")) %>%
  select(-cbsa_name)

hpi_regional <- read_csv(str_c(in_path, "/regional_hpi.csv"))

hpi_regional_year <- hpi_regional %>%
  arrange(cbsa, year, month) %>%
  group_by(cbsa, year) %>%
  summarise_all(first) %>%
  ungroup() %>%
  select(-month)

# additional controls
guren_yearly <- read_csv(str_c(in_path, "/guren_yearly_crtl.csv"))
guren_monthly <- read_csv(str_c(in_path, "/guren_monthly_crtl.csv"))

# yearly data
df_year <- df %>%
  arrange(LoanId, month) %>%
  group_by(LoanId) %>%
  mutate(was_default = if_else(cumsum(in_default) > 1,
    1,
    0
  )) %>%
  filter(
    was_default == 0,
    !is.na(orig_month)
  ) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  mutate(
    mos_since_orig = month - orig_month,
    year = mcd_to_year(month),
    orig_year = mcd_to_year(orig_month),
    underwater = as.numeric(LTV > 1)
  ) %>%
  group_by(LoanId, year) %>%
  summarise(
    is_primary = first(is_primary),
    fico_orig = first(fico_orig),
    dti_ratio = first(dti_ratio),
    cltv_orig = first(cltv_orig),
    is_io = first(is_io),
    init_rate = first(init_rate),
    full_documentation = first(full_documentation),
    is_arm = first(is_arm),
    is_balloon = first(is_balloon),
    orig_year = first(orig_year),
    stop_year = max(mos_since_orig),
    start_year = min(mos_since_orig),
    cbsa = first(prop_cbsa),
    default = max(in_default),
    LTV = mean(LTV),
    underwater = mean(underwater)
  ) %>%
  mutate(
    underwater = round(underwater, digits = 0),
    subprime = as.numeric(fico_orig < 670)
  ) %>%
  ungroup() %>%
  group_by(LoanId) %>%
  mutate(wrong_orig_dt = max(start_year < 0)) %>%
  filter(wrong_orig_dt != 1) %>%
  select(-wrong_orig_dt) %>%
  ungroup() %>%
  left_join(gam, by = "cbsa") %>%
  left_join(hpi_regional_year, by = c("cbsa", "year")) %>%
  left_join(guren_yearly, by = c("cbsa", "year"))

write_csv(df_year, str_c(scratch_overall, "/loan_year_large.csv"))


df_month <- df %>%
  rename(cbsa = prop_cbsa) %>%
  arrange(LoanId, month) %>%
  group_by(LoanId) %>%
  mutate(was_default = if_else(cumsum(in_default) > 1,
    1,
    0
  )) %>%
  filter(
    was_default == 0,
    !is.na(orig_month)
  ) %>%
  mutate(
    mos_since_orig = month - orig_month,
    stop_month = mos_since_orig + 1,
    start_month = mos_since_orig,
    year = mcd_to_year(month),
    orig_year = mcd_to_year(orig_month),
    underwater = as.numeric(LTV > 1),
    subprime = as.numeric(fico_orig < 670),
    wrong_orig_dt = max(mos_since_orig < 0)
  ) %>%
  filter(wrong_orig_dt != 1) %>%
  select(-wrong_orig_dt) %>%
  ungroup() %>%
  rename(mcd_month = month) %>%
  mutate(month = as.numeric(mcd_to_month(mcd_month))) %>%
  left_join(gam, by = "cbsa") %>%
  left_join(hpi_regional, by = c("cbsa", "year", "month")) %>%
  left_join(guren_monthly, by = c("cbsa", "year", "month"))

write_csv(df_month, str_c(scratch_overall, "/loan_month_large.csv"))

rm(df)
