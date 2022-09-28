library(tidyverse)
library(lubridate)
library(imputeTS)

setwd("~/repo/strategic/")
data_path <- "analysis/input/data/guren/"


re_run_sample <- FALSE
reduced_df <- read_csv(str_c(data_path, "guren_controls.csv"))


############
# for this to run df_month from 16_ph_regressions.R must be read in

if (re_run_sample) {
  year_cbsas <- df_month %>%
    select(cbsa, year) %>%
    distinct() %>%
    arrange(cbsa, year)
  
  write_csv(year_cbsas,
            str_c(issue_dir, "cbsa_year_sample.csv"))
  
  
  month_cbsas <- df_month %>%
    select(cbsa, year, month, year_month) %>%
    distinct() %>%
    arrange(cbsa, year, month)
  
  write_csv(month_cbsas,
            str_c(issue_dir, "cbsa_month_sample.csv"))
}

year_cbsas <- read_csv(str_c(data_path, "cbsa_year_sample.csv"))
month_cbsas <- read_csv(str_c(data_path, "cbsa_month_sample.csv"))

############

lin_inter <- function(x) {
  na_vals <- which(is.na(x))
  if (length(x) %in% na_vals) {
    stop("Cannot interpolate with missing maximum")
  }
  non_missing <- {
    1:length(x)
  }[-na_vals]
  
  for (i in na_vals) {
    next_non_missing <- min(non_missing[non_missing > i])
    x[i] <- x[i - 1]  +
      (x[next_non_missing] - x[i - 1]) / (next_non_missing - i + 1)
  }
  return(x)
}

# yearly data
guren_yearly <- reduced_df %>%
  select(-date) %>%
  group_by(cbsa, year) %>%
  summarise_all(mean)

write_csv(guren_yearly,
          str_c(data_path, "guren_yearly_crtl.csv"))


# monthly data
guren_monthly <- tibble(
  cbsa = select(month_cbsas, cbsa) %>% distinct %>% pull(cbsa)
) %>%
  crossing(year = 2007:2016,
           month = 1:12) %>%
  left_join(
    reduced_df %>%
      mutate(q = substr(date, 5, 6),
             month = case_when(q == "q1" ~ 3,
                               q == "q2" ~ 6,
                               q == "q3" ~ 9,
                               q == "q4" ~ 12)),
    by = c("cbsa", "year", "month")
  ) %>%
  mutate(date = ymd(str_c(year, str_pad(month, 2, "left", pad = "0"), "01"))) %>%
  filter(date >= ymd("20070301")) %>%
  arrange(cbsa, year, month) %>%
  group_by(cbsa) %>%
  mutate_at(vars(predict_control:share70_sic_ipolate),
            ~na_interpolation(.))

write_csv(guren_monthly,
          str_c(data_path, "guren_monthly_crtl.csv"))
