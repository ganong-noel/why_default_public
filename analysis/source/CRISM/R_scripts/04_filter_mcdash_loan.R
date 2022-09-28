source("analysis/source/CRISM/R_scripts/00_prelim.R")

loan_level <- readRDS(str_c(temp_directory, "overall/all_loan.rds"))

loan_corresp <- readRDS(str_c(temp_directory, "overall/loan_cid.rds"))

loan_level %>%
  semi_join(loan_corresp, by = "loan_id") %>%
  saveRDS(str_c(temp_directory, "overall/loan.rds"), compress = FALSE)
