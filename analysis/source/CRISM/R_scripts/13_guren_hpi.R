
library(tidyverse)
library(haven)
library(readxl)
library(lubridate)
library(zoo)


in_path <- "analysis/input/data/guren_hpi"

# Guren et al. data
guren_df <- read_dta(str_c(
  in_path,
  "/gmns_gammas.dta"
))

write_csv(
  guren_df,
  str_c(
    in_path,
    "/guren_df.csv"
  )
)

# Regional HPI

freddie_all <- read_dta(str_c(
  in_path,
  "/freddie_foranalysis_longdiff_pc.dta"
))

# get dates from quaterly data
freddie_hpi <- zap_formats(freddie_all) %>%
  select(date, year, cbsa, lhpi_reg_a, region) %>%
  filter(
    year >= 2007,
    year <= 2016
  ) %>%
  arrange(cbsa, date) %>%
  group_by(cbsa, year) %>%
  mutate(
    quarter = row_number(),
    month_q = case_when(
      quarter == 1 ~ "0331",
      quarter == 2 ~ "0630",
      quarter == 3 ~ "0930",
      quarter == 4 ~ "1231"
    ),
    date_q = ymd(str_c(year, month_q)),
    month = month(date_q)
  ) %>%
  ungroup() %>%
  select(date_q, cbsa, region, lhpi_reg_a, year)

# expand to monthly data frame
monthly_hpi <- freddie_hpi %>%
  expand(cbsa, year, 1:12) %>%
  rename(month = `1:12`) %>%
  transmute(cbsa,
    date_q = ceiling_date(ymd(str_c(
      year,
      str_pad(month, 2, side = "left", "0"),
      "01"
    )),
    unit = "month"
    ) - 1
  ) %>%
  left_join(freddie_hpi, by = c("cbsa", "date_q")) %>%
  group_by(cbsa) %>%
  fill(region, .direction = "up") %>%
  fill(year, .direction = "up") %>%
  filter(!row_number() %in% c(1, 2)) %>%
  mutate(
    region_hpi = na.approx(lhpi_reg_a),
    month = month(date_q)
  ) %>%
  ungroup() %>%
  select(region, cbsa, year, month, region_hpi)

write_csv(monthly_hpi, str_c(in_path, "/regional_hpi.csv"))
