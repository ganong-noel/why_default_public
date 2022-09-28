#INPUT:
# "HPI_Bulk_Export_by_ZIP_201904.csv"
# "repeat_sales_clean_all1.csv"
# "repeat_sales_clean_all2.csv"
# "repeat_sales_clean_all3.csv"
# "repeat_sales_clean_all4.csv"
# "repeat_sales_clean_all5.csv"
# "repeat_sales_clean_all6.csv"
# "repeat_sales_clean_all7.csv"
# "repeat_sales_clean_foreclosure1.csv"
# "repeat_sales_clean_foreclosure2.csv"
# "repeat_sales_clean_foreclosure3.csv"
# "repeat_sales_clean_foreclosure4.csv"
# "repeat_sales_clean_foreclosure5.csv"
# "repeat_sales_clean_foreclosure6.csv"
# "repeat_sales_clean_foreclosure7.csv"

#OUTPUT:
# "with_rel_inc_all.csv"
# "with_rel_inc_foreclosure.csv"

library(tidyverse)
library(lubridate)

data_output <- "/project2/pnoel/probertson/repo/strategic/analysis/release/corelogic/"

zip_hpi <- read_csv("/project2/pnoel/data/HPI_Bulk_Export_by_ZIP_201904.csv") %>%
  filter(TIER_CODE == 0) %>%
  transmute(
    prop_zip = as.numeric(ZIP_CODE),
    hpi_dt = ymd(str_c(YYYYMM, "01")),
    hpi = HOME_PRICE_INDEX
  ) %>%
  mutate(prop_zip = str_pad(prop_zip,
    width = 5,
    pad = "0",
    side = "left"
  ))


for (sample in c("all", "foreclosure")) {
  repeat_sales_clean <- map_dfr(
    1:7,
    ~ read_csv(str_c(
      data_output,
      "repeat_sales_clean_",
      sample,
      .x,
      ".csv"
    ))
  ) %>%
    mutate(
      zip = str_pad(zip,
        width = 5,
        pad = "0",
        side = "left"
      ),
      fips = str_pad(fips,
        width = 5,
        pad = "0",
        side = "left"
      )
    ) %>%
    mutate(
      date = ymd(str_c(str_sub(date, 1, 7), "-01")),
      prev_sale = ymd(str_c(str_sub(prev_sale, 1, 7), "-01"))
    )


  distinct_dates <- repeat_sales_clean %>%
    arrange(date) %>%
    pull(date) %>%
    unique()


  zip_hpi <- zip_hpi %>%
    filter(year(hpi_dt) > 1988) %>%
    semi_join(repeat_sales_clean,
      by = c(prop_zip = "zip")
    )

  with_local_aggregate_zip <- repeat_sales_clean %>%
    left_join(zip_hpi, by = c(
      zip = "prop_zip",
      date = "hpi_dt"
    )) %>%
    rename(hpi_current = hpi) %>%
    left_join(zip_hpi, by = c(
      zip = "prop_zip",
      prev_sale = "hpi_dt"
    )) %>%
    # current over original HPI is the relative increase
    mutate(hpi_increase = hpi_current / hpi) %>%
    select(-hpi_current, -hpi) %>%
    filter(!is.na(hpi_increase))


  rm(repeat_sales_clean)

  with_local_aggregate_zip %>%
    write_csv(str_c(
      data_output,
      "with_rel_inc_",
      sample,
      ".csv"
    ))
}
