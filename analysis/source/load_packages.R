if (!exists("reinstall_packages")) {
  reinstall_packages <- FALSE
}

# version packages
packages <- c(
  "yaml",
  "lubridate",
  "tidyverse",
  "testthat",
  "knitr",
  "latex2exp",
  "haven",
  "rprojroot",
  "Hmisc",
  "quantreg",
  "binsreg",
  "readxl",
  "Matching",
  "purrr",
  "broom",
  "dplyr",
  "binsreg",
  "RColorBrewer",
  "scales",
  "gt",
  "corrr"
)

if (reinstall_packages == TRUE) {
  require(versions)
  install.dates(
    packages,
    "2022-05-12"
  )
}

lapply(packages, function(x) require(x, character.only = TRUE))
select <- dplyr::select
matches <- dplyr::matches
