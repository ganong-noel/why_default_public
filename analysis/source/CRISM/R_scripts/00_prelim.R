library(yaml)
library(rprojroot)
library(tidyverse)
library(haven)

matches <- dplyr::matches # resolve conflict with testthat

source("analysis/source/CRISM/R_scripts/00_get_bash_args.R")

stata_sample <- str_c(scratch_dir, "stata_sample/")
temp_directory <- str_c(scratch_dir, "r_output/")


# working directory should be "within" the repo
make_path <- is_git_root$make_fix_file()

config <- yaml.load_file(make_path("analysis/config.yml"))
source("prelim.R")

# These variables determine whether the script will load the dataframes from the
# temporary directory, or use what is in memory (if the files are being run
# consecutively)

load_for_match <- TRUE
load_for_second_lien <- TRUE
load_for_plot <- TRUE
