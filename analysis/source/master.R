# clear memory
rm(list = ls())

# set working directory
setwd("../../") # nolint

remove_large_objects_from_memory <- function() {
  to_remove <- keep(ls(envir = globalenv()), ~ object.size(get(.)) > 200e+7) # 200 MB
  rm(list = to_remove, envir = globalenv())
}

# load packages
reinstall_packages <- FALSE
source("analysis/source/load_packages.R")

# set up directories
data_depenencies <- c("data_cc_simulations")
test_that(
  "Data dependencies are on local machine",
  expect_equal(
    data_depenencies %in% list.files(".."),
    rep(TRUE, length(data_depenencies))
  )
)

make_path <- is_git_root$make_fix_file()
config <- yaml.load_file(make_path("analysis/config.yml"))
source("prelim.R")


##### scripts that don't use chase data ####
# Measurement error simulations
code_path <- config$source_path$sim
out_path <- make_path(config$release_path$sim)
source(file.path(code_path, "relax_assumption_3.R"))
source(file.path(code_path, "sim_bias_var_tradeoff.R"))
remove_large_objects_from_memory()



##### scripts that do use chase data ####
chase_stats <- "analysis/input/disclose/latest/gn_strategic_latest.xls"


# Analysis from PSID
out_path <- make_path(config$release_path$psid)
code_path <- config$source_path$psid
source(file.path(code_path, "ability_to_pay_by_ltv.R"))
source(file.path(code_path, "psid_benchmarks.R"))



# Plot model output
out_path <- make_path("analysis/release/structural_model_fortran/")
code_path <-
  make_path("analysis/source/structural_model_fortran/analysis/")
data_path <-
  str_c(config$data_path$cc_simulations)
source(file.path(code_path, "chase_vs_cc_simulations.R"))
remove_large_objects_from_memory()



# Comparison to prior lit
out_path <- make_path("analysis/release/plot_chase")
code_path <- "analysis/source/plot_chase"
source(file.path(code_path, "comparison_of_sd_empirics.R"))
source(file.path(code_path, "plot_chase.R"))
remove_large_objects_from_memory()
