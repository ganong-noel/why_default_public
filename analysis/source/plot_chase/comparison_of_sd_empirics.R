#INPUT
# "BDS_estimated_default_prob_ignoring_equity_effect.csv"
# "BDS_estimated_strat_default.csv"
# "BDS_Figure6.csv"
# "BDS_Figure3.csv"

#OUTPUT
# "bds_strategic.csv"

data_path <-
  make_path(config$data_path$data, "scraped_strat_default_from_papers")

bin_scores <-
  function(data, start, end, interval) {
    breaks <-
      data_frame(
        lower_bound = c(-Inf, seq(start, end, interval)),
        upper_bound = c(seq(start, end, interval), Inf)
      ) %>%
      suppressWarnings() %>%
      mutate(
        labels =
          case_when(
            lower_bound == -Inf ~ str_c("< ", upper_bound, "%"),
            upper_bound == Inf ~ str_c(lower_bound + 1, "% +"),
            TRUE ~ str_c(lower_bound + 1, "-", upper_bound, "%")
          )
      )

    upper_bound <- breaks$upper_bound
    lower_bound <- breaks$lower_bound
    labels <- breaks$labels

    cases <- lapply(seq_along(labels), function(i) {
      expr(between(ltv, lower_bound[!!i], upper_bound[!!i]) ~ labels[!!i])
    })

    data %>% mutate(binned_ltv = case_when(!!!cases))
  }


######################################
# Export data for plot on the inside #
######################################

bds_1 <-
  read_csv(
    file.path(
      data_path,
      "BDS_estimated_default_prob_ignoring_equity_effect.csv"
    ),
    col_names = c("ltv", "non_strat_defaults")
  ) %>%
  bin_scores(100, 220, 20) %>%
  group_by(binned_ltv) %>%
  summarise(non_strat_defaults = mean(non_strat_defaults))

bds_2 <-
  read_csv(
    file.path(data_path, "BDS_estimated_strat_default.csv"),
    col_names = c("ltv", "strat_defaults")
  ) %>%
  bin_scores(100, 220, 20) %>%
  group_by(binned_ltv) %>%
  summarise(strat_defaults = mean(strat_defaults))

conversion_factor <- pgamma(130, shape = 2.99, scale = 27.6) /
  pgamma(110, shape = 2.99, scale = 27.6)

bhutta_last <-
  bds_1 %>%
  left_join(bds_2) %>%
  summarise_all(last) %>%
  mutate(
    binned_ltv = "221%+",
    strat_defaults = strat_defaults * conversion_factor
  )

bhutta <-
  bds_1 %>%
  left_join(bds_2) %>%
  mutate(binned_ltv = str_replace(binned_ltv, "(\\d{3})-(\\d{3})%$", "\\1%-\\2%")) %>%
  bind_rows(bhutta_last) %>%
  mutate(
    strategic_default_rate =
      strat_defaults / (strat_defaults + non_strat_defaults),
    source = "Bhutta, Dokko and Shan (2017)",
    ref = "bds"
  ) %>%
  dplyr::select(-c(strat_defaults, non_strat_defaults))

write_csv(bhutta, file = str_c(out_path, "/bds_strategic.csv"))

###
# Construct the share of defaults that are strategic
###

# load data from BDS figure 3 and 6
strategic_cltv_6 <- read_csv(file.path(data_path, "BDS_Figure6.csv"),
  col_names = c(
    "CLTV", "strategic_default_rate",
    "CLTV_all", "default_rate"
  ),
  skip = 2
)

cltv_hist <- read_csv(file.path(data_path, "BDS_Figure3.csv"),
  col_names = c("CLTV", "weight")
)

cltv_hist <- cltv_hist %>%
  mutate(
    CLTV_accurate = seq(100, 205, length.out = 22),
    CLTV = CLTV_accurate
  ) %>%
  select(CLTV, weight)

strategic_cltv_6 <- strategic_cltv_6 %>%
  select(-CLTV_all) %>%
  mutate(
    CLTV = round(CLTV, digit = 0),
    CLTV_bin = floor(CLTV / 5) * 5,
    share_strategic = strategic_default_rate / default_rate
  )

# interpolate default values and calc share
strategic_default <- right_join(strategic_cltv_6, cltv_hist,
  by = c("CLTV_bin" = "CLTV")
) %>%
  group_by(CLTV_bin) %>%
  filter(row_number() == 1) %>%
  arrange(CLTV_bin) %>%
  ungroup() %>%
  mutate(share_strategic = approx(
    CLTV_bin, share_strategic,
    CLTV_bin
  )$y)

rate_strategic_underwater <- round(
  weighted.mean(strategic_default$share_strategic,
    w = strategic_default$weight
  ), 4
)

test_that(
  "Strategic default in BDS is 27%",
  expect_equal(rate_strategic_underwater, 0.27, tol = 0.01)
)
