source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")

corelogic_files <- list.files(corelogic_dir, full.names = TRUE)

zip_file <- str_subset(corelogic_files, "ZIP")
cbsa_file <- str_subset(corelogic_files, "CBSA")

zip_hpi <- read_csv(zip_file) %>%
  filter(TIER_CODE == 11) %>%
  transmute(
    prop_zip = as.numeric(ZIP_CODE),
    hpi_dt = YYYYMM,
    hpi = HOME_PRICE_INDEX
  ) %>%
  efx_dates_as_mcdash_months()

write_csv(zip_hpi, str_c(scratch_dir, "corelogic/hpi_zip.csv"))

cbsa_hpi <- read_csv(cbsa_file) %>%
  filter(TIER_CODE == 11) %>%
  transmute(
    prop_cbsa = as.numeric(CBSA_CODE),
    hpi_dt = YYYYMM,
    hpi = HOME_PRICE_INDEX,
    is_msa = str_detect(CBSA_NAME, "Metropolitan")
  ) %>%
  efx_dates_as_mcdash_months()

write_csv(cbsa_hpi, str_c(scratch_dir, "corelogic/hpi_cbsa.csv"))

rm(zip_hpi, cbsa_hpi)
