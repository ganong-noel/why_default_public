# runs all analysis scripts in `analysis/source/structural_model_fortran`

# Setup
rm(list = ls())
library(tidyverse)
library(yaml)
library(rprojroot)
library(RColorBrewer)

make_path <- is_git_root$make_fix_file()
config <- yaml.load_file(make_path("analysis/config.yml"))
source("prelim.R")

data_path <- config$data_path$cc_simulation
out_path <- make_path("analysis/release/structural_model_fortran/")
code_path <-
  make_path("analysis/source/structural_model_fortran/analysis/")

source(file.path(code_path, "cocco_data.R"))
source(file.path(code_path, "sarmi_processing_functions.R"))

# Produce output
source(file.path(code_path, "cc_table_2_dot_plots.R"))
source(file.path(code_path, "optimal_stigma.R"))
source(file.path(code_path, "chase_vs_cc_simulations.R"))
source(file.path(code_path, "stigma_cost_value_functions.R"))
