# INPUT:
# "ability_to_pay_by_ltv.R$data_with_our_variables"
# "gn_strategic_latest.xls"
# "share_and_def.csv"

# OUTPUT:
# "psid_default_and_share.csv"
# "psid_age_p.csv"
# "def_share_2009.tex"
# "def_share_2011.tex"
# "borrower_age.tex"

# aweights are described
# https://www.stata.com/support/faqs/statistics/weights-and-summary-statistics/
ghow_filtered_data <-
  data_with_our_variables %>%
  filter(
    year %in% c(2009, 2011, 2013),
    ltv <= 3,
    ltv > 0.0001,
    has_mort == 1,
    emply %in% c(1, 2, 3, 5),
    !is.na(def_over_90),
    lag_def_90 %in% c(0, NA),
    mpay_tot >= 0,
    pr_tot_heloc > 0,
    !is.na(hpi_cumul_2),
    !is.na(sex),
    !is.na(empl_yoy_1),
    !is.na(naics_1)
  ) %>%
  mutate(
    aweight_multiplier = n() / sum(weight)
  ) %>%
  select(
    hh_id, year, age, ltv, weight,
    inc, lag_inc,
    def_over_30, def_over_60, def_over_90,
    aweight_multiplier,
    ltv_status
  )

##########
# Tables #
##########

by_year_tab_90 <- function(yr) {
  bind_rows(
    ghow_filtered_data %>%
      filter(year == yr) %>%
      mutate(
        ltv_bin = case_when(
          ltv <= 0.8 ~ "(0.01% - 80%]",
          ltv <= 1 ~ "(80 - 100%]",
          ltv > 1 ~ "(100 - 300%]",
          TRUE ~ "All"
        ),
        ltv_bin = factor(ltv_bin, levels = c(
          "(100 - 300%]",
          "(80 - 100%]",
          "(0.01% - 80%]",
          "All"
        ))
      ) %>%
      group_by(ltv_bin) %>%
      summarise(
        default_rate = wtd.mean(def_over_90, weights = weight),
        n = sum(weight)
      ) %>%
      ungroup() %>%
      transmute(ltv_bin,
        default_rate,
        share = n / sum(n)
      ),
    ghow_filtered_data %>%
      filter(year == yr) %>%
      summarise(
        default_rate = wtd.mean(def_over_90, weights = weight),
        n = sum(weight)
      ) %>%
      ungroup() %>%
      transmute(
        ltv_bin = "All",
        default_rate,
        share = n / sum(n)
      )
  )
}

left_join(by_year_tab_90(2009),
  by_year_tab_90(2011),
  by = "ltv_bin",
  suffix = c("_2009", "_2011")
) %>%
  left_join(by_year_tab_90(2013),
    by = "ltv_bin",
    suffix = c("", "_2013")
  ) %>%
  write_csv("analysis/release/combined_benchmarks/psid_default_and_share.csv")

# by year table
all_mortg_tab <- ghow_filtered_data %>%
  group_by(year) %>%
  summarise(
    p25_age_all = wtd.quantile(age, weights = weight, 0.25),
    p50_age_all = wtd.quantile(age, weights = weight, 0.50),
    p75_age_all = wtd.quantile(age, weights = weight, 0.75)
  )

defaulter_tab <- ghow_filtered_data %>%
  filter(def_over_90 == 1) %>%
  group_by(year) %>%
  summarise(
    p25_age_def = wtd.quantile(age, weights = weight, 0.25),
    p50_age_def = wtd.quantile(age, weights = weight, 0.50),
    p75_age_def = wtd.quantile(age, weights = weight, 0.75)
  )

left_join(all_mortg_tab,
  defaulter_tab,
  by = "year"
) %>%
  write_csv("analysis/release/combined_benchmarks/psid_age_p.csv")

#####################
# format tex tables #
#####################

# default rate and shares
bin_walk <- tribble(
  ~stat_bin,
  ~ltv_bin,
  "(-0.01,0.8]",
  "(0.01% - 80%]",
  "(0.8,1]",
  "(80 - 100%]",
  "(1,3]",
  "(100 - 300%]"
)

chase_stat <- read_xls("analysis/input/disclose/latest/gn_strategic_latest.xls",
  sheet = "default_rate_ltv_year"
) %>%
  left_join(bin_walk, by = "stat_bin") %>%
  select(-stat_bin) %>%
  mutate(source = "JPMCI")

psid_stats <-
  read_csv("analysis/release/combined_benchmarks/psid_default_and_share.csv")

crism_stats <-
  read_csv("analysis/release/CRISM/share_and_def.csv")

stats <- map2_dfr(
  list(psid_stats, crism_stats), c("PSID", "CRISM"),
  ~ .x %>%
    pivot_longer(
      cols = -ltv_bin,
      names_to = "var",
      values_to = "val"
    ) %>%
    mutate(
      year = as.numeric(str_extract(var, pattern = "[0-9]{4}")),
      year = if_else(is.na(year), 2013, year),
      var = str_replace_all(var, "[:digit:]", ""),
      var = str_replace_all(var, "_", ""),
      source = .y
    )
) %>%
  filter(var != "sharedefaulter") %>%
  mutate(var = if_else(var == "defaultrate", "default_rate", "share")) %>%
  pivot_wider(
    id_cols = c(ltv_bin, year, source),
    names_from = var,
    values_from = val
  ) %>%
  bind_rows(chase_stat)

map(
  c(2009, 2011),
  ~ stats %>%
    filter(
      year == .x,
      ltv_bin != "All"
    ) %>%
    pivot_wider(
      id_cols = ltv_bin,
      names_from = source,
      values_from = c(default_rate, share)
    ) %>%
    mutate_if(is.numeric, percent_format(accuracy = 0.1)) %>%
    mutate(ltv_bin = case_when(
      ltv_bin == "(100 - 300%]" ~ "LTV > 100",
      ltv_bin == "(80 - 100%]" ~ "80 < LTV leq 100",
      ltv_bin == "(0.01% - 80%]" ~ "LTV leq 80"
    )) %>%
    select(
      ltv_bin,
      default_rate_JPMCI, default_rate_CRISM, default_rate_PSID,
      share_JPMCI, share_CRISM, share_PSID
    ) %>%
    gt() %>%
    tab_spanner(
      label = "Default rate",
      columns = default_rate_JPMCI:default_rate_PSID
    ) %>%
    tab_spanner(
      label = "Share",
      columns = share_JPMCI:share_PSID
    ) %>%
    cols_label(
      ltv_bin = "LTV bin",
      default_rate_JPMCI = "JPMCI",
      default_rate_CRISM = "CRISM",
      default_rate_PSID = "PSID",
      share_JPMCI = "JPMCI",
      share_CRISM = "CRISM",
      share_PSID = "PSID"
    ) %>%
    as_latex() %>%
    as.character() %>%
    str_replace_all("leq ", "$\\\\leq$") %>%
    str_replace_all(">", "$>$") %>%
    str_replace_all("<", "$<$") %>%
    str_replace_all("longtable", "tabular") %>%
    str_replace_all("toprule", "hline\\\\hline") %>%
    cat(file = str_c(
      "analysis/release/combined_benchmarks/def_share_",
      .x,
      ".tex"
    ))
)

# borrower age
chase_age <- read_xls("analysis/input/disclose/latest/gn_strategic_latest.xls",
  sheet = "age_orig_subprime_stats"
) %>%
  select(year, group, ends_with("age")) %>%
  mutate(
    group = substr(group, 1, 3),
    source = "JPMCI"
  ) %>%
  pivot_wider(
    id_cols = c(year, source),
    names_from = group,
    values_from = p25_age:p75_age
  )

psid_age <- read_csv("analysis/release/combined_benchmarks/psid_age_p.csv") %>%
  mutate(source = "PSID")

chase_age %>%
  bind_rows(psid_age) %>%
  filter(year == 2011) %>%
  select(
    source,
    p25_age_all,
    p50_age_all,
    p75_age_all,
    p25_age_def,
    p50_age_def,
    p75_age_def
  ) %>%
  mutate(
    across(
      starts_with("p"),
      round,
      0
    )
  ) %>%
  gt() %>%
  tab_spanner(
    label = "All borrowers",
    columns = p25_age_all:p75_age_all
  ) %>%
  tab_spanner(
    label = "Defaulters",
    columns = p25_age_def:p75_age_def
  ) %>%
  cols_label(
    source = "Percentile",
    p25_age_all = "25\\textsuperscriptth",
    p50_age_all = "50\\textsuperscriptth",
    p75_age_all = "75\\textsuperscriptth",
    p25_age_def = "25\\textsuperscriptth",
    p50_age_def = "50\\textsuperscriptth",
    p75_age_def = "75\\textsuperscriptth"
  ) %>%
  as_latex() %>%
  as.character() %>%
  str_replace_all("textbackslash\\{\\}", "") %>%
  str_replace_all("longtable", "tabular") %>%
  str_replace_all("textsuperscriptth", "textsuperscript{th}") %>%
  str_replace_all("toprule", "hline\\\\hline") %>%
  cat(file = "analysis/release/combined_benchmarks/borrower_age.tex")