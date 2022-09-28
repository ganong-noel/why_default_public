args <- commandArgs()

print(args)

scratch_dir <- str_subset(args, "scratch_dir:") %>%
  str_remove("scratch_dir:")

mcdash_data <- str_subset(args, "crism_data:") %>%
  str_remove("crism_data:")

corelogic_dir <- str_subset(args, "corelogic:") %>%
  str_remove("corelogic:")

output_dir <- str_subset(args, "output_dir:") %>%
  str_remove("output_dir:")

iteration <- str_subset(args, "iteration:") %>%
  str_remove("iteration:") %>%
  as.numeric()

months_in_data <- seq(306, 455)
print(iteration)
if (length(iteration) > 0) {
  required_months <- split(months_in_data, 1:5)[[iteration]]
} else {
  required_months <- months_in_data
}

print(required_months)
