source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")

print(temp_directory)


walk(
  c(
    "first_default",
    "foreclosure_start",
    "foreclosure_end",
    "cured_loans",
    "time_30",
    "straight_LPS"
  ),
  collate_files
)
