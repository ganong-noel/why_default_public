# Created: 10/19/21
# Last update: 09/15/22

# INPUT:
# "psid_kfh_2013_3.dta"
# "gn_strategic_latest.xls"

# OUTPUT:
# "empirical_cdf_resid_inc_90ltv_dpd_60.png"
# "empirical_cdf_resid_inc_dpd_60.pdf"
# "empirical_cdf_resid_inc_va_dpd_60.png"
# "comp_strategic_est_raw_90ltv_dpd_60.png"
# "comp_strategic_est_raw_dpd_60.png"

# The code cleans data into 2 tidy formats:
# tidy_with_groups: winsorization of 2.5% from each side
# tidy_with_groups_ecdf: no winsorization (for ecdfs)

##### Set data cleaning and aesthetic parameters ####
winsor_min_max <- 80000 # income less housing and consumption winsorisation
greys <- brewer.pal("Greys", n = 9)
color_lyx_teal <- brewer.pal(n = 9, name = "BuGn")[5]
color_lyx_purple <- brewer.pal(n = 9, name = "Purples")[5]
colors <- c(
  `Underwater default` = "#2B83BA",
  `Above water default` = color_lyx_teal,
  `All underwater` = color_lyx_purple
)

colors_pooled <- c(
  `consumption` = "#fdae61", "",
  `chase_consumption` = "#d7191c"
)
color_labels_90 <- c(
  `Underwater default` = "LTV > 90 defaults",
  `Above water default` = "LTV < 90 defaults",
  `All underwater` = "All underwater borrowers"
)

color_labels_100 <- c(
  `Underwater default` = "Underwater defaults",
  `Above water default` = "Above water defaults",
  `All underwater` = "All underwater borrowers"
)

##### Load data #####
# This is a big file and takes a two or three minutes to load
kyles_data_raw <-
  read_dta(make_path(config$data_path$data, "psid_kfh_2013_3.dta"))

##### Import data #####
data_with_our_variables <-
  kyles_data_raw %>%
  transmute(
    hh_id = famidpn,
    year = year,
    age = age_,
    sex = sex_,
    hpi_cumul_2 = ifelse(max_year_move >= 2009, 0, hpi_cumul),
    empl_yoy_1,
    naics_1,
    pr_tot_heloc,
    ltv = cltv_heloc,
    emply = lfs_indiv_,
    has_mort = imort,
    weight = fw_,
    household_head = (head_ == 10 & seq_ == 1),
    def_over_90 = i_90_,
    def_over_60 = i_60_,
    def_over_30 = i_30_,
    divorce_shock = (divorce == 1),
    inc = inc_,
    post_tax_inc = (inc_ - fiitax_ - siitax_ - fica_),
    post_tax_inc_w_shock =
      post_tax_inc - labor_inc_ * (!lfs_indiv_ %in% c(1)) -
        wlabor_inc_ * (!lfs_indiv_sp %in% c(1) | divorce_shock == 1),
    can_pay = total_cons_raw_ < post_tax_inc,
    can_pay_w_shock = total_cons_raw_ < post_tax_inc_w_shock,
    northeast = (st_ == 9 | st_ == 33 | st_ == 34 | st_ == 25 | st_ == 36 |
      st_ == 23 | st_ == 42 | st_ == 44 | st_ == 50),
    mideast = (st_ == 17 | st_ == 18 | st_ == 19 | st_ == 20 | st_ == 26 |
      st_ == 27 | st_ == 29 | st_ == 31 | st_ == 38 |
      st_ == 39 | st_ == 46 | st_ == 55),
    south = (st_ == 1 | st_ == 5 | st_ == 10 | st_ == 11 | st_ == 12 |
      st_ == 13 | st_ == 21 | st_ == 22 | st_ == 24 | st_ == 28 |
      st_ == 37 | st_ == 40 | st_ == 45 | st_ == 47 | st_ == 48 |
      st_ == 51 | st_ == 54),
    west = (
      st_ == 2 | st_ == 4 | st_ == 6 | st_ == 8 | st_ == 15 | st_ == 16 |
        st_ == 30 | st_ == 32 | st_ == 35 | st_ == 41 |
        st_ == 49 | st_ == 53 | st_ == 56),
    cp_va_resid = case_when(
      pr_ < 80000 & funo_ == 1 & northeast == TRUE ~ 390,
      pr_ < 80000 & funo_ == 2 & northeast == TRUE ~ 654,
      pr_ < 80000 & funo_ == 3 & northeast == TRUE ~ 788,
      pr_ < 80000 & funo_ == 4 & northeast == TRUE ~ 888,
      pr_ < 80000 & funo_ == 5 & northeast == TRUE ~ 921,
      pr_ < 80000 & funo_ == 6 & northeast == TRUE ~ 996,
      pr_ < 80000 & funo_ >= 7 & northeast == TRUE ~ 1071,
      pr_ >= 80000 & funo_ == 1 & northeast == TRUE ~ 450,
      pr_ >= 80000 & funo_ == 2 & northeast == TRUE ~ 775,
      pr_ >= 80000 & funo_ == 3 & northeast == TRUE ~ 909,
      pr_ >= 80000 & funo_ == 4 & northeast == TRUE ~ 1025,
      pr_ >= 80000 & funo_ == 5 & northeast == TRUE ~ 1062,
      pr_ >= 80000 & funo_ == 6 & northeast == TRUE ~ 1142,
      pr_ >= 80000 & funo_ >= 7 & northeast == TRUE ~ 1222,
      pr_ < 80000 & funo_ == 1 & mideast == TRUE ~ 382,
      pr_ < 80000 & funo_ == 2 & mideast == TRUE ~ 641,
      pr_ < 80000 & funo_ == 3 & mideast == TRUE ~ 772,
      pr_ < 80000 & funo_ == 4 & mideast == TRUE ~ 868,
      pr_ < 80000 & funo_ == 5 & mideast == TRUE ~ 902,
      pr_ < 80000 & funo_ == 6 & mideast == TRUE ~ 977,
      pr_ < 80000 & funo_ >= 7 & mideast == TRUE ~ 1052,
      pr_ >= 80000 & funo_ == 1 & mideast == TRUE ~ 441,
      pr_ >= 80000 & funo_ == 2 & mideast == TRUE ~ 738,
      pr_ >= 80000 & funo_ == 3 & mideast == TRUE ~ 889,
      pr_ >= 80000 & funo_ == 4 & mideast == TRUE ~ 1003,
      pr_ >= 80000 & funo_ == 5 & mideast == TRUE ~ 1039,
      pr_ >= 80000 & funo_ == 6 & mideast == TRUE ~ 1119,
      pr_ >= 80000 & funo_ >= 7 & mideast == TRUE ~ 1199,
      pr_ < 80000 & funo_ == 1 & south == TRUE ~ 382,
      pr_ < 80000 & funo_ == 2 & south == TRUE ~ 641,
      pr_ < 80000 & funo_ == 3 & south == TRUE ~ 772,
      pr_ < 80000 & funo_ == 4 & south == TRUE ~ 868,
      pr_ < 80000 & funo_ == 5 & south == TRUE ~ 902,
      pr_ < 80000 & funo_ == 6 & south == TRUE ~ 977,
      pr_ < 80000 & funo_ >= 7 & south == TRUE ~ 1052,
      pr_ >= 80000 & funo_ == 1 & south == TRUE ~ 441,
      pr_ >= 80000 & funo_ == 2 & south == TRUE ~ 738,
      pr_ >= 80000 & funo_ == 3 & south == TRUE ~ 889,
      pr_ >= 80000 & funo_ == 4 & south == TRUE ~ 1003,
      pr_ >= 80000 & funo_ == 5 & south == TRUE ~ 1039,
      pr_ >= 80000 & funo_ == 6 & south == TRUE ~ 1119,
      pr_ >= 80000 & funo_ >= 7 & south == TRUE ~ 1199,
      pr_ < 80000 & funo_ == 1 & west == TRUE ~ 425,
      pr_ < 80000 & funo_ == 2 & west == TRUE ~ 713,
      pr_ < 80000 & funo_ == 3 & west == TRUE ~ 859,
      pr_ < 80000 & funo_ == 4 & west == TRUE ~ 967,
      pr_ < 80000 & funo_ == 5 & west == TRUE ~ 1004,
      pr_ < 80000 & funo_ == 6 & west == TRUE ~ 1079,
      pr_ < 80000 & funo_ >= 7 & west == TRUE ~ 1154,
      pr_ >= 80000 & funo_ == 1 & west == TRUE ~ 491,
      pr_ >= 80000 & funo_ == 2 & west == TRUE ~ 823,
      pr_ >= 80000 & funo_ == 3 & west == TRUE ~ 990,
      pr_ >= 80000 & funo_ == 4 & west == TRUE ~ 1117,
      pr_ >= 80000 & funo_ == 5 & west == TRUE ~ 1158,
      pr_ >= 80000 & funo_ == 6 & west == TRUE ~ 1238,
      pr_ >= 80000 & funo_ >= 7 & west == TRUE ~ 1318
    ),
    can_pay_va_w_shock = cp_va_resid <
      (post_tax_inc_w_shock - mort_cons_raw_ -
        insur_tax_cons_raw_ - alimony_cons_raw_) / 12,
    inc_after_consumption = post_tax_inc_w_shock - total_cons_raw_,
    inc_after_cons_va = post_tax_inc_w_shock - mort_cons_raw_ -
      insur_tax_cons_raw_ - alimony_cons_raw_ - cp_va_resid * 12,
    cltv_90 = cltv_heloc >= .90,
    mpay_tot,
    house_exp = mort_cons_raw_ + insur_tax_cons_raw_,
    consumption = total_cons_fam_wohousing_,
    total_cons_raw_,
    mort_cons_raw_,
    insur_tax_cons_raw_,
    alimony_cons_raw_,
    unemployed = (lfs_indiv_ %in% c(2, 3)) | (lfs_indiv_sp %in% c(2, 3)),
    ltv_status = if_else(cltv_heloc > 1, "Underwater", "Above water"),
    t = -1
  ) %>%
  filter(household_head == TRUE) %>%
  group_by(hh_id) %>%
  arrange(year) %>%
  mutate(
    lag_def_90 = lag(def_over_90),
    lag_def_60 = lag(def_over_60),
    lag_inc = lag(inc),
    lag_house_exp = lag(house_exp),
    lag_mpay_tot = lag(mpay_tot),
    lag_consumption = lag(consumption),
    lag_t = lag(year) - year - 1,
    share_inc_for_housing_exp = (inc / lag_house_exp),
    lag_share_inc_housing = (lag_inc / lag_house_exp),
    delta_share_inc_housing = (inc - lag_inc) / lag_house_exp,
    inc_after_consumption_normalized = inc_after_consumption / house_exp
  ) %>%
  ungroup()

ghow_filtered_data <-
  data_with_our_variables %>%
  filter(
    year %in% c(2009, 2011, 2013),
    ltv <= 2.5,
    has_mort == 1,
    age >= 24,
    age <= 65,
    emply %in% c(1, 2, 3, 5),
    mpay_tot >= 0,
    pr_tot_heloc > 0,
    !is.na(hpi_cumul_2),
    !is.na(sex),
    !is.na(empl_yoy_1),
    !is.na(naics_1)
  )

# aweights are described
# https://www.stata.com/support/faqs/statistics/weights-and-summary-statistics/
ghow_clean <- function(data) {
  data %>%
    mutate(
      aweight_multiplier = n() / sum(weight),
      inc_after_consumption =
        if_else(abs(inc_after_consumption) > winsor_min_max,
          sign(inc_after_consumption) * winsor_min_max,
          inc_after_consumption
        ),
      inc_after_cons_va =
        if_else(abs(inc_after_cons_va) > winsor_min_max,
          sign(inc_after_cons_va) * winsor_min_max,
          inc_after_cons_va
        )
    ) %>%
    transmute(hh_id,
      year,
      ltv,
      weight,
      inc,
      lag_inc,
      def_over_30,
      def_over_60,
      def_over_90,
      income_for_consumption = post_tax_inc_w_shock,
      house_exp,
      lag_house_exp,
      ltv_status,
      unemployed,
      va_consumption = mort_cons_raw_ + insur_tax_cons_raw_ +
        alimony_cons_raw_ + cp_va_resid * 12,
      total_consumption = total_cons_raw_,
      resid_subsistence = inc_after_cons_va,
      resid_consumption = inc_after_consumption,
      aweight_multiplier
    )
}

ghow_filtered_data_dpd60 <- ghow_filtered_data %>%
  filter(!is.na(def_over_60), lag_def_60 %in% c(0, NA)) %>%
  ghow_clean(.) %>%
  mutate(
    default = def_over_60,
    dpd = 60
  )

ghow_filtered_data_dpd90 <- ghow_filtered_data %>%
  filter(!is.na(def_over_90), lag_def_90 %in% c(0, NA)) %>%
  ghow_clean(.) %>%
  mutate(
    default = def_over_90,
    dpd = 90
  )

#### Share of unemployed underwater and above water ####

shares_unemployed <- ghow_filtered_data_dpd60 %>%
  filter(year %in% c(2009, 2011)) %>% # years in GHOW WP 2015
  mutate(pwts = weight * aweight_multiplier) %>%
  group_by(ltv_status, unemployed) %>%
  summarise(n = sum(pwts)) %>%
  group_by_at(vars(-unemployed, -n)) %>%
  mutate(
    n_unemployed = round(n),
    population = round(sum(n)),
    share = percent(round(n / sum(n), 3), 0.1)
  ) %>%
  filter(unemployed == 1) %>%
  select(-unemployed, -n)

test_that(
  "Share unemployed above water is 7.3%",
  expect_equal(
    shares_unemployed %>%
      filter(ltv_status == "Above water") %>%
      pull(share),
    "7.3%"
  )
)

test_that(
  "Share unemployed underwater is 12.2%",
  expect_equal(
    shares_unemployed %>%
      filter(ltv_status == "Underwater") %>%
      pull(share),
    "12.2%"
  )
)

rm(shares_unemployed)

# duplicate dataset to allow for different DPD cutoffs
ghow_list <- list(ghow_filtered_data_dpd60, ghow_filtered_data_dpd90)
names(ghow_list) <- c("dpd_60", "dpd_90")

# duplicate dataset to allow for different LTV cutoffs
ghow_list_appended_ltv <- map(ghow_list, ~ .x %>%
  mutate(ltv_cutoff = 1) %>%
  bind_rows(.x %>% mutate(ltv_cutoff = 0.9)))

##### Tidy data for alpha quantile estimates analysis #####

# Current code sets a 2.5% winsorization (from each side).
winsor_q_threshold <- 0.025

# winsor function
weighted_winsor <- function(variable, weight, winsor) {
  low_cut <- wtd.quantile(variable, weight, winsor, na.rm = T)
  high_cut <- wtd.quantile(variable, weight, 1 - winsor, na.rm = T)
  case_when(
    variable < low_cut ~ low_cut,
    variable > high_cut ~ high_cut,
    TRUE ~ variable
  )
}

# I construct a dataframe that records the outcome variable and the cutoff
# variable in a "tidy" way
# (i.e. one column for the outcome and one for the cutoff).
make_tidy_ghow_winsorized <- function(data) {
  data %>%
    mutate(underwater = ltv > ltv_cutoff) %>%
    group_by(underwater, default, ltv_cutoff) %>%
    mutate(
      winsor_diff = weighted_winsor(
        inc - lag_inc,
        weight,
        winsor_q_threshold
      ),
      resid_subsistence =
        weighted_winsor(
          income_for_consumption - va_consumption,
          weight,
          winsor_q_threshold
        ),
      resid_consumption =
        weighted_winsor(
          income_for_consumption - total_consumption,
          weight,
          winsor_q_threshold
        ),
      house_exp = weighted_winsor(
        house_exp,
        weight,
        winsor_q_threshold
      )
    ) %>%
    group_by(hh_id, ltv_cutoff) %>%
    arrange(ltv_cutoff, year) %>%
    group_by(underwater, year, ltv_cutoff) %>%
    mutate(resid_winsor_change_mortgage = winsor_diff /
      Hmisc::wtd.mean(lag_house_exp, weights = weight, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-inc, -lag_inc) %>%
    pivot_longer(
      cols = contains("resid"),
      names_prefix = "resid_",
      names_to = "residual_income_type",
      values_to = "residual_income"
    ) %>%
    mutate(residual_income_quarterly = residual_income / 4)
}

tidy_ghow_winsorized <- map(ghow_list_appended_ltv, make_tidy_ghow_winsorized)

# If a household appears in a given year, it should be represented 10 times
# within that year. 2x for two LTV cutoffs x 3 residual_income_type values
map(tidy_ghow_winsorized, ~ .x %>%
  group_by(year, hh_id) %>%
  tally() %>%
  transmute(test = n == 6) %>%
  pull(test) %>%
  all() %>%
  expect_true())

# Want to classify our defaulters into three groups, underwater defaulters,
# above water defaulters and the reference category (all underwater borrowers).
group_categorisation <- tibble(
  default = c(1, 1, 1, 0),
  underwater = c(TRUE, FALSE, TRUE, TRUE),
  group = c(
    "Underwater default",
    "Above water default",
    "All underwater",
    "All underwater"
  )
)

# duplicate the underwater defaulters
tidy_with_groups_winsorized <-
  map(tidy_ghow_winsorized, ~ inner_join(.x, group_categorisation) %>%
    group_by(group, residual_income_type) %>%
    mutate(pwts = weight * aweight_multiplier))

##### Tidy data for ECDFS and shares can pay plot #####
make_tidy_ghow <- function(data) {
  data %>%
    mutate(underwater = ltv > ltv_cutoff) %>%
    group_by(underwater, year) %>%
    ungroup() %>%
    pivot_longer(
      cols = contains("resid"),
      names_prefix = "resid_",
      names_to = "residual_income_type",
      values_to = "residual_income"
    ) %>%
    mutate(residual_income_quarterly = residual_income / 4)
}

tidy_ghow <- map(ghow_list_appended_ltv, make_tidy_ghow)

tidy_with_groups <- map(tidy_ghow, ~ inner_join(.x, group_categorisation) %>%
  group_by(group) %>%
  mutate(pwts = weight * aweight_multiplier))

##### Plot ECDFS ####
winsor_min_max_quarterly <- winsor_min_max / 4

cdf_end_points <-
  tibble(
    ecdf = rep(c(0, 1), 3),
    residual_income_quarterly = rep(
      c(
        -winsor_min_max_quarterly,
        winsor_min_max_quarterly
      ),
      3
    ),
    group = rep(c(
      "Above water default",
      "Underwater default",
      "All underwater"
    ),
    each = 2
    )
  )

groups_ecdf <- map(tidy_with_groups, ~ .x %>%
  group_by(ltv_cutoff, residual_income_type) %>%
  filter(!is.na(residual_income_quarterly)) %>%
  group_by(ltv_cutoff, residual_income_type, group) %>%
  arrange(group, residual_income_quarterly) %>%
  mutate(ecdf = cumsum(weight) / sum(weight)) %>%
  ungroup())

# Aesthetics are changed to align with Chase plot in R2 letter
plot_ghow_ecdf <- function(data, x_axis_label, color_labels) {
  data %>%
    ggplot(aes(residual_income_quarterly, ecdf, color = group)) +
    geom_step(stat = "identity") +
    fte_theme() +
    labs(
      x = x_axis_label,
      y = "",
      subtitle = "Cumulative distribution function"
    ) +
    theme(
      legend.position = c(0, .75),
      legend.text = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.justification = c(0, 0),
      plot.title.position = "plot",
      plot.subtitle = element_text(
        size = 12,
        color = greys[7]
      ),
      panel.grid.major.x = element_line(
        color = greys[3],
        size = .25
      )
    ) +
    scale_x_continuous(
      labels = label_comma(prefix = "$"),
      # labels = scales::dollar,
      breaks = seq(
        -winsor_min_max_quarterly, winsor_min_max_quarterly,
        winsor_min_max_quarterly / 2
      ),
      limits = c(-winsor_min_max_quarterly, winsor_min_max_quarterly)
    ) +
    scale_color_manual(
      values = colors,
      labels = color_labels
    )
}

main_plots <-
  map(
    groups_ecdf,
    ~ .x %>%
      filter(residual_income_type %in% c("consumption", "subsistence")) %>%
      group_by(ltv_cutoff, residual_income_type) %>%
      arrange(residual_income_type, desc(ltv_cutoff)) %>%
      nest() %>%
      ungroup() %>%
      mutate(
        x_axis_label = if_else(
          residual_income_type == "consumption",
          "Quarterly income at default minus expenses (including mortgage due)",
          "Quarterly income minus subsistence expenses (including mortgage due)"
        ),
        color_labels = list(
          color_labels_100, color_labels_90,
          color_labels_100, color_labels_90
        )
      ) %>%
      mutate(
        data =
          map(
            data,
            ~ bind_rows(.x, cdf_end_points) %>%
              arrange(group, ecdf) %>%
              mutate(group = factor(group,
                levels = c(
                  "Underwater default",
                  "Above water default",
                  "All underwater"
                )
              ))
          ),
        plot = pmap(
          list(
            data,
            x_axis_label,
            color_labels
          ),
          ~ plot_ghow_ecdf(..1, ..2, ..3)
        )
      )
  )

file_names_paper <- c(
  "empirical_cdf_resid_inc_dpd_60.pdf",
  "empirical_cdf_resid_inc_90ltv_dpd_60.png",
  "empirical_cdf_resid_inc_va_dpd_60.png"
)

plots_for_paper <- c(
  (main_plots$dpd_60 %>%
    filter(
      ltv_cutoff == 1,
      residual_income_type == "consumption"
    ))$plot,
  (main_plots$dpd_60
    %>% filter(
      ltv_cutoff == 0.9,
      residual_income_type == "consumption"
    ))$plot,
  (main_plots$dpd_60
    %>% filter(
      ltv_cutoff == 1,
      residual_income_type == "subsistence"
    ))$plot
)

map2(
  plots_for_paper, file_names_paper,
  ~ ggsave(file.path(out_path, .y), .x,
    width = 6, height = 4
  )
)

# width = 8, height = 4.5
##### Plot shares can pay ####
# Get the chase share positive statistics
# Load Chase data:

jpmci_est_stats_ltv_1_dpd60 <-
  read_excel(chase_stats,
    sheet = "stats_text"
  ) %>%
  filter(
    grepl("60dpd", desc),
    !grepl("p diff", desc)
  ) %>%
  pivot_wider(
    names_from = desc,
    values_from = expected
  ) %>%
  rename(
    abovewater = `positive resid inc change abovewater (60dpd)`,
    underwater = `positive resid inc change underwater (60dpd)`,
    all_defaulters = `positive resid inc change all defaulters (60dpd)`,
    se_diff = `se diff positive resid inc change (60dpd)`
  )

jpmci_est_stats_ltv_1_dpd90 <-
  read_excel(chase_stats,
    sheet = "stats_text"
  ) %>%
  filter(
    grepl("90dpd", desc),
    !grepl("p diff", desc)
  ) %>%
  pivot_wider(
    names_from = desc,
    values_from = expected
  ) %>%
  rename(
    abovewater = `positive resid inc change abovewater (90dpd)`,
    underwater = `positive resid inc change underwater (90dpd)`,
    all_defaulters = `positive resid inc change all defaulters (90dpd)`,
    se_diff = `se diff positive resid inc change (90dpd)`
  )

# duplicate dataset to allow for different DPD cutoffs
jpmci_est_stats_ltv_1 <- list(
  jpmci_est_stats_ltv_1_dpd60,
  jpmci_est_stats_ltv_1_dpd90
)
names(jpmci_est_stats_ltv_1) <- c("dpd_60", "dpd_90")

jpmci_est_stats_ltv_1 <- map(jpmci_est_stats_ltv_1, ~ .x %>%
  mutate(
    n = 85654 + 52331,
    se = sqrt(all_defaulters * (1 - all_defaulters) / n)
  ))
# where n is from section 3.1 of paper (e.g., Table 3)

# get the share of defaulters meeting each of the criteria in the PSID
proportions <-
  map(tidy_with_groups, ~ .x %>%
    select(
      group, weight, residual_income, residual_income_type, ltv_cutoff,
      pwts
    ) %>%
    group_by(group, ltv_cutoff, residual_income_type) %>%
    summarise(
      share_defaults = Hmisc::wtd.mean(residual_income > 0, weight),
      n = n(),
      sum_sq_pweight = sum((weight / sum(weight))^2)
    ) %>%
    mutate(var = (1 - share_defaults) * share_defaults * sum_sq_pweight) %>%
    group_by(ltv_cutoff, residual_income_type) %>%
    # The code below will add the variance of the underwater default group
    mutate(
      total_var = var + sum(var * (group == "Underwater default")),
      diff = share_defaults -
        sum(share_defaults * (group == "Underwater default"))
    ) %>%
    mutate(
      se_diff = if_else(group == "Underwater default",
        NA_real_,
        sqrt(total_var)
      ),
      p_val = 2 * pnorm(-abs(diff / se_diff))
    ) %>%
    select(-var, -total_var) %>%
    ungroup() %>%
    mutate(source = "PSID"))

for (dpd in c("dpd_60", "dpd_90")) {
  proportions[[dpd]] <- proportions[[dpd]] %>%
    add_row(
      source = "Chase", group = "Above water default",
      ltv_cutoff = 1, residual_income_type = "chase_consumption",
      share_defaults = jpmci_est_stats_ltv_1[[dpd]]$abovewater,
      se_diff = jpmci_est_stats_ltv_1$dpd_60$se_diff
    ) %>%
    add_row(
      source = "Chase", group = "Underwater default",
      ltv_cutoff = 1, residual_income_type = "chase_consumption",
      share_defaults = jpmci_est_stats_ltv_1[[dpd]]$underwater,
      se_diff = NA
    )
}

with_plots <-
  map(proportions, ~ .x %>%
    filter(group != "All underwater") %>% # don't need comparison group here
    mutate(
      residual_income_type = factor(residual_income_type,
        levels = c(
          "consumption",
          "chase_consumption",
          "subsistence"
        )
      ),
      group = factor(group,
        levels = c(
          "Underwater default",
          "Above water default"
        )
      )
    ) %>%
    filter(residual_income_type %in%
      c(
        "consumption",
        "chase_consumption",
        "subsistence"
      )) %>%
    group_by(ltv_cutoff) %>%
    nest() %>%
    ungroup() %>%
    arrange(ltv_cutoff) %>%
    mutate(color_labels = list(color_labels_90, color_labels_100)) %>%
    mutate(
      plot =
        map2(
          data, color_labels,
          ~ .x %>%
            ggplot() +
            aes(residual_income_type, share_defaults, fill = group) +
            geom_col(position = "dodge") +
            geom_errorbar(aes(
              ymin = share_defaults - 1.96 * se_diff,
              ymax = share_defaults + 1.96 * se_diff
            ),
            position = position_dodge(width = 0.8),
            width = 0.2
            ) +
            fte_theme() +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 14),
              plot.title.position = "plot",
              plot.subtitle = element_text(color = greys[6])
            ) +
            labs(
              x = "", y = "",
              subtitle = "Share defaults"
            ) +
            scale_fill_manual(
              values =
                c(
                  "Underwater default" = "#2B83BA",
                  "Above water default" = "#ABDDA4"
                ),
              labels = .y
            ) +
            scale_x_discrete(
              labels =
                c(
                  consumption = "Can pay PSID",
                  chase_consumption = "Can pay Chase",
                  subsistence = "Subsist and pay PSID"
                )
            ) +
            scale_y_continuous(
              labels =
                scales::percent_format(accuracy = 1)
            )
        )
    ))

file_names_raw <- c(
  "comp_strategic_est_raw_90ltv",
  "comp_strategic_est_raw"
)

file_names <-
  map(
    file_names_raw,
    ~ paste0(.x, "_", names(with_plots), ".png")
  ) %>%
  transpose() %>%
  flatten()

file_names_paper <- file_names[c(1, 2)]
plots_for_paper <- c(
  (with_plots$dpd_60 %>% filter(ltv_cutoff == 0.9))$plot,
  (with_plots$dpd_60 %>% filter(ltv_cutoff == 1))$plot
)

map2(
  plots_for_paper,
  file_names_paper,
  ~ ggsave(file.path(out_path, .y), .x,
    width = 8, height = 4.5
  )
)