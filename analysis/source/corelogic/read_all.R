#INPUT:
# "University_of_Chicago_FR300000107671374_Deed_History_File1.txt"
# "University_of_Chicago_FR300000107671374_Deed_History_File2.txt"
# "University_of_Chicago_FR300000107671374_Deed_History_File3.txt"
# "University_of_Chicago_FR300000107671374_Deed_History_File4.txt"
# "University_of_Chicago_FR300000107671374_Deed_History_File5.txt"
# "University_of_Chicago_FR300000107671374_Deed_History_File6.txt"
# "University_of_Chicago_FR300000107671374_Deed_History_File7.txt"

#OUTPUT:
# "filtered_data_s_1.csv"
# "filtered_data_s_2.csv"
# "filtered_data_s_3.csv"
# "filtered_data_s_4.csv"
# "filtered_data_s_5.csv"
# "filtered_data_s_6.csv"
# "filtered_data_s_7.csv"
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

library(tidyverse)
library(lubridate)

raw_location <- "/project2/databases/bfi/bfi-tax_deed/old_version/"
data_output <- "/project2/pnoel/probertson/repo/strategic/analysis/release/corelogic/"

equal_nas <- function(x, y) {
  result <- x == y
  result[is.na(result)] <- FALSE
  result[is.na(x) & is.na(y)] <- TRUE
  return(result)
}

col_spec <- cols_only(
  `COMPOSITE PROPERTY LINKAGE KEY` = col_character(),
  `FORECLOSURE REO INDICATOR` = col_integer(),
  `SHORT SALE INDICATOR` = col_integer(),
  `PARTIAL INTEREST INDICATOR` = col_character(),
  `TRANSACTION BATCH DATE` = col_integer(),
  `BUYER 1 FULL NAME` = col_character(),
  `SELLER 1 FULL NAME` = col_character(),
  `MORTGAGE AMOUNT` = col_double(),
  `MORTGAGE SEQUENCE NUMBER` = col_number(),
  `SALE DOCUMENT TYPE` = col_character(),
  `SITUS ZIP CODE` = col_double(),
  `FIPS CODE` = col_integer(),
  `SALE AMOUNT` = col_double(),
  `SALE DATE` = col_integer(),
  `PROPERTY INDICATOR CODE` = col_number(),
  `PRIMARY CATEGORY CODE` = col_character(),
  `TRANSACTION TYPE` = col_integer()
)


filter_data <- function(x, pos) {
  x %>%
    select(
      prop_id = `COMPOSITE PROPERTY LINKAGE KEY`,
      zip = `SITUS ZIP CODE`,
      fips = `FIPS CODE`,
      curr_sale_amount = `SALE AMOUNT`,
      partial_ind = `PARTIAL INTEREST INDICATOR`,
      batch_date = `TRANSACTION BATCH DATE`,
      buyer_1_name = `BUYER 1 FULL NAME`,
      seller_1_name = `SELLER 1 FULL NAME`,
      mtg_amount = `MORTGAGE AMOUNT`,
      mtg_seq = `MORTGAGE SEQUENCE NUMBER`,
      doc_type = `SALE DOCUMENT TYPE`,
      date = `SALE DATE`,
      foreclosure = `FORECLOSURE REO INDICATOR`,
      short_sale = `SHORT SALE INDICATOR`,
      trans_type = `TRANSACTION TYPE`,
      property_ind = `PROPERTY INDICATOR CODE`,
      primary_cat = `PRIMARY CATEGORY CODE`
    ) %>%
    filter(
      !is.na(curr_sale_amount),
      !is.na(date),
      mtg_seq %in% c(0, 1)
    ) %>%
    mutate(date = ymd(str_replace(date, "00$", "01"))) %>%
    filter(date > ymd("19890101")) %>%
    select(-mtg_seq)
}

write_sales_clean <- function(df, name, i = i) {
  df %>%
    select(
      -c(
        short_sale,
        foreclosure,
        property_ind,
        lag_property_ind,
        prev_short_sale,
        prev_foreclosure,
        primary_cat,
        lag_primary_cat,
        trans_type,
        lag_trans_type,
        partial_ind,
        lag_partial_ind
      )
    ) %>%
    mutate(time_diff = (date - prev_sale) / 365) %>%
    filter(date >= ymd("19980101")) %>%
    filter(time_diff < 10) %>%
    group_by(prop_id) %>%
    filter(!any(time_diff < 1)) %>%
    ungroup() %>%
    select(-time_diff) %>%
    mutate(zip = if_else(str_length(zip) > 5,
      str_pad(zip,
        width = 9,
        pad = "0",
        side = "left"
      ) %>%
        str_sub(1, 5),
      str_pad(zip,
        width = 5,
        pad = "0", side = "left"
      )
    )) -> repeat_sales_clean

  write_csv(
    repeat_sales_clean,
    str_c(
      data_output,
      "repeat_sales_clean_",
      name,
      i,
      ".csv"
    )
  )

  rm(repeat_sales_clean)
}

for (i in seq(1, 7)) {
  filtered_data <- read_delim_chunked(
    str_c(
      raw_location,
      "/deed/University_of_Chicago_FR300000107671374_Deed_History_File",
      i,
      ".txt"
    ),
    delim = "|", quote = "",
    callback = DataFrameCallback$new(filter_data),
    chunk_size = 1000000, col_types = col_spec
  )

  write_csv(filtered_data, str_c(
    data_output,
    "filtered_data_s_",
    i,
    ".csv"
  ))

  filtered_data <- read_csv(str_c(
    data_output,
    "filtered_data_s_",
    i,
    ".csv"
  ))


  with_match <- filtered_data %>%
    arrange(
      buyer_1_name,
      seller_1_name,
      doc_type,
      curr_sale_amount,
      mtg_amount,
      date
    ) %>%
    mutate(
      match_lag = equal_nas(buyer_1_name, lag(buyer_1_name)) &
        equal_nas(seller_1_name, lag(seller_1_name)) &
        equal_nas(doc_type, lag(doc_type)) &
        equal_nas(curr_sale_amount, lag(curr_sale_amount)) &
        equal_nas(mtg_amount, lag(mtg_amount)),
      match_lead = lead(match_lag),
      same_time_lag = abs(date - lag(date)) <= 5
    ) %>%
    filter((match_lag & same_time_lag) | (match_lead & lead(same_time_lag))) %>%
    mutate(
      match_lag = equal_nas(buyer_1_name, lag(buyer_1_name)) &
        equal_nas(seller_1_name, lag(seller_1_name)) &
        equal_nas(doc_type, lag(doc_type)) &
        equal_nas(curr_sale_amount, lag(curr_sale_amount)) &
        equal_nas(mtg_amount, lag(mtg_amount)),
      match_lead = lead(match_lag),
      same_time_lag = abs(date - lag(date)) <= 5
    ) %>%
    mutate(sale_group = cumsum(!match_lag | !same_time_lag)) %>%
    select(prop_id, fips, date, sale_group)


  # We can now split the data frame into two types:

  # Group sales, which involve multiple properties
  # In a single transation

  # And the same property being recorded multiple times
  # (i.e. duplicate observations)

  group_sales <- with_match %>%
    group_by(sale_group) %>%
    filter(n_distinct(prop_id) > 1) %>%
    ungroup() %>%
    transmute(prop_id, fips, date, group = TRUE) %>%
    distinct()

  duplicate_obs <- with_match %>%
    group_by(sale_group) %>%
    filter(n_distinct(prop_id) == 1) %>%
    ungroup() %>%
    distinct()

  with_group_distinct <- filtered_data %>%
    left_join(group_sales, by = c("prop_id", "fips", "date")) %>%
    filter(
      !is.na(prop_id),
      !is.na(zip),
      !is.na(fips)
    ) %>%
    select(
      -mtg_amount, -doc_type,
      -batch_date, -buyer_1_name, -seller_1_name
    ) %>%
    left_join(duplicate_obs) %>%
    distinct() %>%
    group_by(sale_group) %>%
    # This is entirely designed to remove duplicates
    # Sale group is coming from the duplicate obs df
    # so it won't be there for group sales, only duplicate observations
    # So we can take either that the sale group is na
    # Or the observation is the first of its group
    filter(is.na(sale_group) | row_number() == 1) %>%
    ungroup() %>%
    select(-sale_group)

  rm(filtered_data, group_sales)


  repeat_sales <- with_group_distinct %>%
    group_by(prop_id) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    filter(count > 1) %>%
    select(-count) %>%
    arrange(prop_id, date) %>%
    group_by(prop_id) %>%
    mutate(
      prev_sale_amount = lag(curr_sale_amount),
      prev_sale = lag(date),
      prev_foreclosure = lag(foreclosure),
      prev_short_sale = lag(short_sale),
      lag_property_ind = lag(property_ind),
      lag_primary_cat = lag(primary_cat),
      lag_trans_type = lag(trans_type),
      lag_partial_ind = lag(partial_ind),
      lag_group = lag(group)
    ) %>%
    ungroup() %>%
    filter(
      !is.na(prev_sale_amount),
      prev_foreclosure != 1,
      prev_short_sale != 1,
      property_ind == 10,
      lag_property_ind == 10,
      primary_cat == "A",
      lag_primary_cat == "A",
      trans_type %in% c(1, 3),
      lag_trans_type %in% c(1, 3),
      is.na(partial_ind),
      is.na(lag_partial_ind),
      is.na(group),
      is.na(lag_group)
    )

  all_sales <- repeat_sales

  foreclosure_sales <- repeat_sales %>%
    filter(foreclosure == 1)

  rm(repeat_sales)
  rm(with_group_distinct)

  write_sales_clean(all_sales, "all", i = i)
  write_sales_clean(foreclosure_sales, "foreclosure", i = i)

  print(str_c("saved file ", i))
}
