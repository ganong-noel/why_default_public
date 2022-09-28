library(tidyverse)
library(testthat)
library(lubridate)
library(Hmisc)
library(gt)
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")
source("analysis/source/CRISM/R_scripts/00_prelim.R")
filter <- dplyr::filter
lag <- dplyr::lag
matches <- dplyr::matches
is_null <- purrr::is_null

chase_sum_stats <- readxl::read_xls("analysis/input/disclose/latest/gn_strategic_latest.xls",
  sheet = "chase_benchmarking_statistics"
)

crism_counts <- map_dfr(
  list.files(str_c(temp_directory, "overall"),
    "^matched_counts_[0-9]{3}",
    full.names = TRUE
  ),
  ~ readRDS(.x) %>%
    ungroup() %>%
    mutate(
      matching = "Matched",
      type = str_replace(type, "overall", "total")
    )
) %>%
  bind_rows(map_dfr(
    list.files(str_c(temp_directory, "overall"),
      "^unmatched_counts_[0-9]{3}",
      full.names = TRUE
    ),
    ~ readRDS(.x)
  ) %>%
    ungroup() %>%
    mutate(matching = "All")) %>%
  mutate(date = ymd(str_c(mcd_to_year(month), mcd_to_month(month), "01")))

crism_date_df <- crism_counts %>%
  select(date) %>%
  distinct()

date_samples <- tibble(
  date = list(
    crism_date_df %>%
      filter(
        date >= ymd("20080101"),
        date <= ymd("20150831")
      ) %>%
      pull(date),
    crism_date_df %>%
      filter(year(date) == 2011) %>%
      pull(date)
  ),
  sample = c("full", "2011")
) %>%
  unnest(date)




crism_counts <- inner_join(crism_counts, date_samples)

crism_def_by_subprime <- crism_counts %>%
  filter(
    type != "in foreclosure",
    mort_type == 4
  ) %>%
  group_by(month, matching, sample) %>%
  summarise(share_default = sum(number_loans * as.numeric(type == "defaulter")) /
    sum(number_loans)) %>%
  group_by(matching, sample) %>%
  summarise(default_rate_subprime = mean(share_default)) %>%
  mutate(
    source = if_else(matching == "All",
      "LPS",
      "CRISM"
    ),
    group = "All mortgages"
  ) %>%
  ungroup() %>%
  full_join(crism_counts %>%
    filter(
      type != "in foreclosure",
      mort_type != 4
    ) %>%
    group_by(month, matching, sample) %>%
    summarise(share_default = sum(number_loans * as.numeric(type == "defaulter")) /
      sum(number_loans)) %>%
    group_by(matching, sample) %>%
    summarise(default_rate_non_subprime = mean(share_default)) %>%
    mutate(source = if_else(matching == "All",
      "LPS",
      "CRISM"
    )) %>%
    ungroup())

crism_orig_year <- crism_counts %>%
  filter(type != "in foreclosure") %>%
  group_by(matching, sample) %>%
  summarise(
    p25_orig_year = wtd.quantile(orig_year,
      weights = number_loans,
      probs = 0.25,
      na.rm = TRUE
    ),
    p50_orig_year = wtd.quantile(orig_year,
      weights = number_loans,
      probs = 0.5,
      na.rm = TRUE
    ),
    p75_orig_year = wtd.quantile(orig_year,
      weights = number_loans,
      probs = 0.75,
      na.rm = TRUE
    )
  ) %>%
  mutate(group = "All mortgages") %>%
  bind_rows(
    crism_counts %>%
      filter(type == "defaulter") %>%
      group_by(matching, sample) %>%
      summarise(
        p25_orig_year = wtd.quantile(orig_year,
          weights = number_loans,
          probs = 0.25,
          na.rm = TRUE
        ),
        p50_orig_year = wtd.quantile(orig_year,
          weights = number_loans,
          probs = 0.5,
          na.rm = TRUE
        ),
        p75_orig_year = wtd.quantile(orig_year,
          weights = number_loans,
          probs = 0.75,
          na.rm = TRUE
        )
      ) %>%
      mutate(group = "Defaulters")
  ) %>%
  mutate(source = if_else(matching == "All",
    "LPS",
    "CRISM"
  ))

calc_sum_stats <- function(df, var_in, sample_) {
  df %>%
    group_by({{ var_in }}, type, date, sample) %>%
    summarise(number_loans = sum(number_loans)) %>%
    arrange(sample, {{ var_in }}, date, type) %>%
    group_by(sample, {{ var_in }}, date) %>%
    mutate(cumnum = cumsum(number_loans)) %>%
    select(-number_loans) %>%
    pivot_wider(
      names_from = {{ var_in }},
      values_from = cumnum
    ) %>%
    mutate(var_out = `TRUE` / (`TRUE` + `FALSE`)) %>%
    select(type, date, var_out) %>%
    group_by(type, sample) %>%
    summarise(var_out = mean(var_out)) %>%
    mutate(
      source = sample_,
      group = if_else(str_detect(type, "default"),
        "Defaulters",
        "All mortgages"
      )
    ) %>%
    ungroup() %>%
    select(-type)
}

crism_sum_stats <- crism_counts %>%
  filter(
    matching == "Matched",
    no_double_count == 0,
    type != "in foreclosure"
  ) %>%
  calc_sum_stats(
    var_in = underwater,
    sample_ = "CRISM"
  ) %>%
  rename(share_under = var_out)

lps_sum_stats_subprime <- crism_counts %>%
  filter(
    matching == "All",
    type != "in foreclosure"
  ) %>%
  mutate(subprime = mort_type == 4) %>%
  calc_sum_stats(
    var_in = subprime,
    sample_ = "LPS"
  ) %>%
  rename(share_subprime = var_out)

crism_sum_stats_subprime <- crism_counts %>%
  filter(
    matching == "Matched",
    no_double_count == 0,
    type != "in foreclosure",
    !is.na(subprime)
  ) %>%
  calc_sum_stats(
    var_in = subprime,
    sample_ = "CRISM"
  ) %>%
  rename(share_subprime = var_out) %>%
  bind_rows(lps_sum_stats_subprime)

crism_sum_stats <- crism_counts %>%
  filter(type != "in foreclosure") %>%
  group_by(month, matching, sample) %>%
  summarise(share_default = sum(number_loans * as.numeric(type == "defaulter")) /
    sum(number_loans)) %>%
  group_by(matching, sample) %>%
  summarise(default_rate = mean(share_default)) %>%
  mutate(
    source = if_else(matching == "All",
      "LPS",
      "CRISM"
    ),
    group = "All mortgages"
  ) %>%
  ungroup() %>%
  select(-matching) %>%
  full_join(crism_sum_stats) %>%
  full_join(crism_sum_stats_subprime) %>%
  full_join(crism_def_by_subprime) %>%
  full_join(crism_orig_year)

get_shares_occupancy <- function(df) {
  df %>%
    summarise(
      share_primary = sum(number_loans * as.numeric(OccupancyId == 1) /
        sum(number_loans)),
      share_investor = sum(number_loans * as.numeric(OccupancyId == 3) /
        sum(number_loans))
    )
}


groups_df <- tibble(
  type = c("defaulter", "defaulter", "other"),
  group = c("Defaulters", "Defaulters", "All mortgages")
)

crism_sum_stats <- crism_counts %>%
  filter(OccupancyId %in% seq(1, 4)) %>%
  inner_join(groups_df) %>%
  group_by(month, group, sample, matching) %>%
  get_shares_occupancy() %>%
  group_by(group, sample, matching) %>%
  select(-month) %>%
  summarise_all(mean) %>%
  mutate(source = if_else(matching == "All",
    "LPS",
    "CRISM"
  )) %>%
  full_join(crism_sum_stats)


foreclosure_counts <- map_dfr(
  list.files(str_c(temp_directory, "overall"),
    "^foreclosure_counts",
    full.names = TRUE
  ),
  ~ readRDS(.x)
) %>%
  pivot_wider(
    names_from = foreclosure_within_12,
    values_from = n
  ) %>%
  mutate(share = `1` / (`1` + `0`)) %>%
  mutate(
    date = ymd(str_c(mcd_to_year(month), mcd_to_month(month), "01")),
    cal_month = month(date),
    year = year(date)
  ) %>%
  inner_join(date_samples)

crism_sum_stats <- foreclosure_counts %>%
  group_by(underwater, sample) %>%
  summarise(share = mean(share)) %>%
  pivot_wider(
    names_from = underwater,
    values_from = share
  ) %>%
  transmute(
    foreclosure_prob_above = `FALSE`,
    foreclosure_prob_under = `TRUE`,
    source = "CRISM",
    group = "Defaulters",
    sample
  ) %>%
  full_join(crism_sum_stats)

variable_labels <- tibble(
  variable = c(
    "default_rate",
    "default_rate_subprime",
    "default_rate_non_subprime",
    "share_under",
    "share_subprime",
    "share_primary",
    "share_investor",
    "p25_orig_year",
    "p50_orig_year",
    "p75_orig_year",
    "foreclosure_prob_above",
    "foreclosure_prob_under"
  ),
  ` ` = c(
    "90 day delinquency rate",
    "90 day delinquency rate on subprime loans",
    "90 day delinquency rate on non-subprime loans",
    "Share underwater",
    "Share subprime",
    "Share primary occupant",
    "Share investor",
    "Origination year (25th percentile)",
    "Origination year (50th percentile)",
    "Origination year (75th percentile)",
    "Share with foreclosure within year (above water)",
    "Share with foreclosure within year (underwater)"
  )
)



chase_sum_stats <- chase_sum_stats %>%
  rename(
    foreclosure_prob_above = foreclosure_rate_abovewater,
    foreclosure_prob_under = foreclosure_rate_underwater
  ) %>%
  rename_at(vars(contains("mean_")), ~ str_remove(., "mean_"))

chase_orig_suprime <- readxl::read_xls("analysis/input/disclose/latest/gn_strategic_latest.xls",
  sheet = "age_orig_subprime_stats"
) %>%
  filter(year == 2011) %>%
  transmute(
    source = "Chase",
    group = if_else(group == "defaulters", "Defaulters", "All mortgages"),
    p25_orig_year = p25_orig,
    p50_orig_year = p50_orig,
    p75_orig_year = p75_orig,
    share_subprime = subprime_rate,
    sample = "2011"
  ) %>%
  mutate_at(
    vars(p25_orig_year:p75_orig_year),
    ~ substr(as.character(.), 1, 4) %>% as.numeric(.)
  )


# output tables
origination_stats <- c(
  "Share investor",
  "Share primary occupant",
  "Share subprime",
  "Origination year (25th percentile)",
  "Origination year (50th percentile)",
  "Origination year (75th percentile)"
)

performance_stats <- c(
  "90 day delinquency rate",
  "90 day delinquency rate on subprime loans",
  "90 day delinquency rate on non-subprime loans",
  "Share underwater",
  "Share with second lien",
  "Share with foreclosure within year (above water)",
  "Share with foreclosure within year (underwater)"
)

mba_tbl <- tribble(
  ~Sample, ~Benchmark, ~MBA,
  "All mortgages", "Share subprime", "8.6%",
  "Defaulters", "Share subprime", "30%",
  "All mortgages", "90 day delinquency rate", "3.6%",
  "All mortgages", "90 day delinquency rate on subprime loans", "13%",
  "All mortgages", "90 day delinquency rate on non-subprime loans", "2.8%"
)

output_tbl <- bind_rows(crism_sum_stats, chase_sum_stats) %>%
  bind_rows(chase_orig_suprime) %>%
  select(-matching) %>%
  filter(sample == 2011) %>%
  pivot_longer(
    cols = -c(group, source, sample),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  distinct() %>%
  select(-sample) %>%
  pivot_wider(
    id_cols = c(group, variable),
    names_from = source,
    values_from = value
  ) %>%
  arrange(group, is.na(Chase), is.na(CRISM), is.na(LPS)) %>%
  left_join(variable_labels) %>%
  mutate(fv = str_detect(variable, "foreclosure")) %>%
  arrange(fv) %>%
  transmute(
    Sample = group,
    Benchmark = ` `, JPMCI = Chase, CRISM, McDash = LPS
  ) %>%
  mutate_if(is.numeric, ~ case_when(
    . <= 0.1 ~ scales::percent(., accuracy = 0.1),
    . > 0.1 & . < 1 ~ scales::percent(., accuracy = 1),
    TRUE ~ as.character(.)
  )) %>%
  mutate_all(~ if_else(str_detect(., "NA"),
    "",
    .
  )) %>%
  left_join(mba_tbl, by = c("Sample", "Benchmark"))

output_tbl %>%
  filter(Benchmark %in% origination_stats) %>%
  mutate(Benchmark = factor(Benchmark, levels = origination_stats)) %>%
  arrange(Sample, Benchmark) %>%
  mutate(Benchmark = as.character(Benchmark)) %>%
  stargazer::stargazer(
    summary = FALSE,
    float = FALSE,
    rownames = FALSE,
    out = "analysis/release/CRISM/origination_tab.tex"
  )

output_tbl %>%
  filter(Benchmark %in% performance_stats) %>%
  mutate(Benchmark = factor(Benchmark, levels = performance_stats)) %>%
  arrange(Sample, Benchmark) %>%
  mutate(
    JPMCI = if_else(Benchmark == "90 day delinquency rate on subprime loans", # to-be corrected once the disclosure is released
      "13%",
      JPMCI
    ),
    JPMCI = if_else(Benchmark == "90 day delinquency rate on non-subprime loans",
      "2.6%",
      JPMCI
    )
  ) %>%
  mutate(Benchmark = as.character(Benchmark)) %>%
  stargazer::stargazer(
    summary = FALSE,
    float = FALSE,
    rownames = FALSE,
    out = "analysis/release/CRISM/performance_tab.tex"
  )


##################################
# Default rate and shares by LTV #
##################################

build_binned_table <- function(months) {
  by_bin <- crism_counts %>%
    filter(
      type != "in foreclosure",
      number_first_liens <= 2,
      matching == "Matched",
      month %in% months
    ) %>%
    group_by(ltv_bin, type) %>%
    summarise(number_loans = sum(number_loans)) %>%
    pivot_wider(
      id_cols = ltv_bin,
      names_from = type,
      values_from = number_loans
    ) %>%
    ungroup() %>%
    bind_rows(summarise_all(., ~ if (is.numeric(.)) sum(.) else "All")) %>%
    mutate(
      all = other + defaulter,
      default_rate = defaulter / all,
      share = all / sum(all[which(ltv_bin != "All")]),
      share_defaulter = defaulter / sum(defaulter[which(ltv_bin != "All")])
    ) %>%
    select(ltv_bin, default_rate, share, share_defaulter)
  return(by_bin)
}

months_2009 <- 349:360
months_2011 <- 373:384
months_2013 <- 397:408

share_and_def_tbl <- left_join(build_binned_table(months_2009),
  build_binned_table(months_2011),
  by = "ltv_bin",
  suffix = c("_2009", "_2011")
) %>%
  left_join(build_binned_table(months_2013),
    by = "ltv_bin",
    suffix = c("", "_2013")
  )

share_and_def_tbl %>%
  mutate_if(is.numeric, ~ scales::percent(., accuracy = 0.1)) %>%
  gt() %>%
  tab_spanner(
    label = "2009",
    column = default_rate_2009:share_defaulter_2009
  ) %>%
  tab_spanner(
    label = "2011",
    column = default_rate_2011:share_defaulter_2011
  ) %>%
  tab_spanner(
    label = "2013",
    column = default_rate:share_defaulter
  ) %>%
  as_latex() %>%
  as.character() %>%
  cat(file = "analysis/release/CRISM/share_and_def.tex")

share_and_def_tbl %>%
  write_csv("analysis/release/CRISM/share_and_def.csv")
