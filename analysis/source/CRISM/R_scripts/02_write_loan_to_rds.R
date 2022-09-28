source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")

saveRDS(
  import_mcdash_loan(str_c(mcdash_data, "mcdash/Loan.dta")),
  str_c(temp_directory, "overall/all_loan.rds")
)
