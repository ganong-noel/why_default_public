library(tidyverse)
library(survival)
library(lfe)
library(stargazer)
library(broom)
library(testthat)
library(fastDummies)

if (FALSE) {
  setwd("/home/tcejka/repo/strategic/")
}

source("analysis/source/CRISM/R_scripts/00_prelim.R")
source("analysis/source/CRISM/R_scripts/00_cleaning_functions.R")

if (FALSE) {
  scratch_dir <- "/scratch/midway2/tcejka/CRISM_temp_files/"
}

scratch_loans <- str_c(scratch_dir, "r_output/month/loan_lv")
scratch_overall <- str_c(scratch_dir, "r_output/overall")

record_missings <- function(df) {
  df %>%
    mutate(
      missing_is_primary = as.numeric(is.na(is_primary)),
      is_primary = if_else(is.na(is_primary), 9, is_primary),
      missing_fico_orig = as.numeric(is.na(fico_orig)),
      fico_orig = if_else(is.na(fico_orig), 9, fico_orig),
      missing_subprime = as.numeric(is.na(subprime)),
      subprime = if_else(is.na(subprime), 9, subprime),
      missing_dti_ratio = as.numeric(is.na(dti_ratio)),
      dti_ratio = if_else(is.na(dti_ratio), 9, dti_ratio),
      missing_cltv_orig = as.numeric(is.na(cltv_orig)),
      cltv_orig = if_else(is.na(cltv_orig), 9, cltv_orig),
      missing_full_documentation = as.numeric(is.na(full_documentation)),
      full_documentation = if_else(is.na(full_documentation), 9, full_documentation),
      missing_is_balloon = as.numeric(is.na(is_balloon)),
      is_balloon = if_else(is.na(is_balloon), 9, is_balloon),
      missing_init_rate = as.numeric(is.na(init_rate)),
      init_rate = if_else(is.na(init_rate), 9, init_rate),
      missing_predict_control = as.numeric(is.na(predict_control)),
      predict_control = if_else(is.na(predict_control), 9, predict_control),
      missing_share10 = as.numeric(is.na(share10_sic_ipolate)),
      share10_sic_ipolate = if_else(is.na(share10_sic_ipolate), 9, share10_sic_ipolate),
      missing_share15 = as.numeric(is.na(share15_sic_ipolate)),
      share15_sic_ipolate = if_else(is.na(share15_sic_ipolate), 9, share15_sic_ipolate),
      missing_share20 = as.numeric(is.na(share20_sic_ipolate)),
      share20_sic_ipolate = if_else(is.na(share20_sic_ipolate), 9, share20_sic_ipolate),
      missing_share40 = as.numeric(is.na(share40_sic_ipolate)),
      share40_sic_ipolate = if_else(is.na(share40_sic_ipolate), 9, share40_sic_ipolate),
      missing_share50 = as.numeric(is.na(share50_sic_ipolate)),
      share50_sic_ipolate = if_else(is.na(share50_sic_ipolate), 9, share50_sic_ipolate),
      missing_share52 = as.numeric(is.na(share52_sic_ipolate)),
      share52_sic_ipolate = if_else(is.na(share52_sic_ipolate), 9, share52_sic_ipolate),
      missing_share60 = as.numeric(is.na(share60_sic_ipolate)),
      share60_sic_ipolate = if_else(is.na(share60_sic_ipolate), 9, share60_sic_ipolate),
      missing_share70 = as.numeric(is.na(share70_sic_ipolate)),
      share70_sic_ipolate = if_else(is.na(share70_sic_ipolate), 9, share70_sic_ipolate)
    )
}

df_year <- read_csv(str_c(scratch_overall, "/loan_year_large.csv")) %>%
  filter(
    start_year != stop_year,
    !is.na(gam)
  ) %>%
  mutate(
    cbsa_factor = as.factor(cbsa),
    year_factor = as.factor(year),
    orig_year_factor = as.factor(orig_year),
    region = as.factor(region),
    instrument_region = gam * region_hpi
  ) %>%
  record_missings()


df_month <- read_csv(str_c(scratch_overall, "/loan_month_large.csv")) %>%
  filter(!is.na(gam)) %>%
  mutate(
    cbsa_factor = as.factor(cbsa),
    year_month = as.factor(str_c(year, "_", month)),
    year_factor = as.factor(year),
    orig_year_factor = as.factor(orig_year),
    region = as.factor(region),
    instrument_region = gam * region_hpi
  ) %>%
  record_missings()

# check sample sizes
test_that(
  "First stage obs size is 14.9 M.",
  {
    expect_equal(
      round(df_month %>%
        pull(LoanId) %>%
        length() / 10^6, digits = 1),
      14.9
    )
  }
)

test_that(
  "Number of loans is 388k",
  {
    expect_equal(
      round(df_month %>%
        pull(LoanId) %>%
        unique() %>%
        length() / 1000),
      388
    )
  }
)

controls <- c(
  "is_balloon",
  "init_rate",
  "is_io",
  "cltv_orig",
  "is_primary",
  "subprime",
  "is_arm",
  "predict_control",
  "share10_sic_ipolate",
  "share15_sic_ipolate",
  "share20_sic_ipolate",
  "share40_sic_ipolate",
  "share50_sic_ipolate",
  "share52_sic_ipolate",
  "share60_sic_ipolate",
  "share70_sic_ipolate",
  "missing_is_balloon",
  "missing_init_rate",
  "missing_cltv_orig",
  "missing_is_primary",
  "missing_subprime",
  "missing_predict_control"
)
# only including missing_predict_control because to capture the effect of all missing additional Guren controls

formula_ph <- function(y, x, ctrl = controls, fe) {
  as.formula(paste(
    y, "~",
    x, "+",
    paste(controls, collapse = "+"), "+",
    fe,
    "+ cluster(cbsa_factor)"
  ))
}

formula_fstg <- function(x, z, ctrl = controls, fe) {
  as.formula(paste(
    x, "~",
    z, "+",
    paste(controls, collapse = "+"),
    "|", fe,
    "| 0 | cbsa_factor"
  ))
}

# First column: OLS
ols <-
  coxph(Surv(start_year, stop_year, default) ~ underwater + region:year_factor +
    cbsa_factor + cluster(cbsa_factor),
  data = df_year
  )
summary(ols)


# Second column: OLS with controls
ols_ctrl <-
  coxph(formula_ph(
    y = "Surv(start_year, stop_year, default)",
    x = "underwater",
    ctrl = controls,
    fe = "region:year_factor + cbsa_factor"
  ),
  data = df_year
  )
summary(ols_ctrl)

# Third column: IV-Guren
fstg_guren <- felm(formula_fstg(
  x = "LTV",
  z = "instrument_region",
  ctrl = controls,
  fe = "region:year_month + cbsa_factor"
),
data = df_month
)

F_guren <- round(summary(fstg_guren)$P.fstat[5], digits = 2)
df_month$res_orig_year_LTV <- residuals(fstg_guren)

df_res <- df_month %>%
  select(LoanId, year, res_orig_year_LTV) %>%
  group_by(LoanId, year) %>%
  summarise(res_orig_year_LTV_guren = mean(res_orig_year_LTV)) %>%
  ungroup()

df_year <- df_year %>%
  left_join(df_res, by = c("LoanId", "year"))

sum(is.na(df_year$res_orig_year_LTV_guren)) == 0 # full merge

iv_guren <-
  coxph(formula_ph(
    y = "Surv(start_year, stop_year, default)",
    x = "underwater + res_orig_year_LTV",
    ctrl = controls,
    fe = "region:year_factor + cbsa_factor"
  ),
  data = rename(df_year, res_orig_year_LTV = res_orig_year_LTV_guren)
  )
summary(iv_guren)

# Fourth column: IV-Palmer
fstg_palmer <- felm(
  formula_fstg(
    x = "LTV",
    z = "gam:year_month",
    ctrl = controls,
    fe = "orig_year_factor + cbsa_factor"
  ),
  df_month
)

F_palmer <- round(summary(fstg_palmer)$P.fstat[5], digits = 2)
df_month$res_orig_year_LTV <- residuals(fstg_palmer)

df_res <- df_month %>%
  select(LoanId, year, res_orig_year_LTV) %>%
  group_by(LoanId, year) %>%
  summarise(res_orig_year_LTV_palmer = mean(res_orig_year_LTV)) %>%
  ungroup()

df_year <- df_year %>%
  left_join(df_res, by = c("LoanId", "year"))

sum(is.na(df_year$res_orig_year_LTV_palmer)) == 0 # full merge

iv_palmer <-
  coxph(formula_ph(
    y = "Surv(start_year, stop_year, default)",
    x = "underwater + res_orig_year_LTV",
    ctrl = controls,
    fe = "orig_year_factor + cbsa_factor"
  ),
  data = rename(df_year, res_orig_year_LTV = res_orig_year_LTV_palmer)
  )
summary(iv_palmer)

results <- list(
  ols = ols,
  ols_ctrl = ols_ctrl,
  iv_guren = iv_guren,
  iv_palmer = iv_palmer
)

results

alpha_ne <- function(reg_output) {
  a <- 1 - 1 / exp(reg_output$coefficients[1])
  format(as.numeric(round(a, 3)), nsmall = 3)
}

se_alpha_ne <- function(reg_output, se_input) {
  se <- sqrt(se_input[1]^2 * exp(-2 * reg_output$coefficients[1]))
  paste0("(", format(as.numeric(round(se, 3)), nsmall = 3), ")")
}

N <- map(results, ~ .$n) %>% unlist()
logli <- map(results, ~ scales::comma(as.numeric(logLik(.)))) %>% unlist()
se_robust <- map(results, ~ tidy(.)$robust.se)
hat_alpha_ne <- map(results, ~ alpha_ne(.)) %>% unlist()
hat_se_alpha_ne <- map2(
  results, se_robust,
  ~ se_alpha_ne(.x, .y)
) %>% unlist()

stargazer(ols, ols_ctrl,
  iv_guren, iv_palmer,
  covariate.labels = c("Underwater", "LTV fitted residuals"),
  se = se_robust,
  omit = c(
    "year_factor", "region", "missing_", "cbsa",
    "is_balloon", "init_rate", "is_io", "cltv_orig", "is_primary",
    "subprime", "is_arm", "predict_control", "share"
  ),
  omit.stat = c("rsq", "adj.rsq", "f", "ser"),
  model.names = FALSE,
  star.cutoffs = NA,
  omit.table.layout = "ld",
  add.lines = list(
    c("$\\hat{\\alpha}_{\\text{negative equity}}$", hat_alpha_ne),
    c("", hat_se_alpha_ne),
    c("Region-Year FEs", c(
      "Y", "Y",
      "Y", "N"
    )),
    c("CBSA FEs", c(
      "Y", "Y",
      "Y", "Y"
    )),
    c("Borrower and loan characteristics", c(
      "N", "Y",
      "Y", "Y"
    )),
    c("CBSA controls", c(
      "N", "Y",
      "Y", "Y"
    )),
    c("Origination year FEs", c(
      "N", "N",
      "N", "Y"
    )),
    c("Instrument", c(
      "-", "-",
      "Cyclicality-HPI", "Cyclicality-Month"
    )),
    c("First stage partial F-Stat", c(
      "-", "-",
      F_guren, F_palmer
    )),
    c("Log Likelihood", logli),
    c("Observations", N)
  ),
  table.layout = "=ldc#-t-ad-",
  type = "latex",
  float = FALSE
) %>%
  write_lines("analysis/release/CRISM/iv_results.tex")

rm(results)

# 16_binned_reg.R ----
ltv_bin <- c(-Inf, 0.85, 0.95, Inf)

bin_ctrls <-
  str_c(
    "`ltv_bins_(",
    c(
      "-Inf,0.85]`",
      "0.95, Inf]`"
    )
  )

ltv_bin_text <- paste(bin_ctrls, collapse = "+")

df_year <-
  df_year %>%
  mutate(ltv_bins = cut(LTV, ltv_bin)) %>%
  dummy_cols("ltv_bins")


iv_guren_ltv_bins <-
  coxph(formula_ph(
    y = "Surv(start_year, stop_year, default)",
    x = str_c(ltv_bin_text, " + res_orig_year_LTV"),
    ctrl = controls,
    fe = "region:year_factor + cbsa_factor"
  ),
  data = rename(df_year, res_orig_year_LTV = res_orig_year_LTV_guren)
  )


iv_palmer_ltv_bins <-
  coxph(formula_ph(
    y = "Surv(start_year, stop_year, default)",
    x = str_c(ltv_bin_text, " + res_orig_year_LTV"),
    ctrl = controls,
    fe = "orig_year_factor + cbsa_factor"
  ),
  data = rename(df_year, res_orig_year_LTV = res_orig_year_LTV_palmer)
  )


results <- list(
  iv_guren_ltv_bins = iv_guren_ltv_bins,
  iv_palmer_ltv_bins = iv_palmer_ltv_bins
)

results

alpha_ne_binned <- function(reg_output) {
  a <- 1 - 1 / exp(coef(reg_output)["`ltv_bins_(0.95, Inf]`"])
  format(as.numeric(round(a, 3)), nsmall = 3)
}

se_alpha_ne_binned <- function(reg_output, se_input) {
  se <- sqrt(se_input[2]^2 * exp(-2 * coef(reg_output)["`ltv_bins_(0.95, Inf]`"]))
  paste0("(", format(as.numeric(round(se, 3)), nsmall = 3), ")")
}

N <- map(results, ~ .$n) %>% unlist()
logli <- map(results, ~ scales::comma(as.numeric(logLik(.)))) %>% unlist()
se_robust <- map(results, ~ tidy(.)$robust.se)
hat_alpha_ne <- map(results, ~ alpha_ne_binned(.)) %>% unlist()
hat_se_alpha_ne <- map2(
  results, se_robust,
  ~ se_alpha_ne_binned(.x, .y)
) %>% unlist()

stargazer(iv_guren_ltv_bins, iv_palmer_ltv_bins,
  covariate.labels = c("LTV $\\leq$ 85", "LTV $>$ 95", "LTV fitted residuals"),
  se = se_robust,
  omit = c(
    "year_factor", "region", "missing_", "cbsa",
    "is_balloon", "init_rate", "is_io", "cltv_orig", "is_primary",
    "subprime", "is_arm", "predict_control", "share"
  ),
  omit.stat = c("rsq", "adj.rsq", "f", "ser"),
  model.names = FALSE,
  star.cutoffs = NA,
  omit.table.layout = "ld",
  add.lines = list(
    c("$\\hat{\\alpha}_{\\text{negative equity}}$", hat_alpha_ne),
    c("", hat_se_alpha_ne),
    c("Region-Year FEs", c("Y", "N")),
    c("CBSA FEs", c("Y", "Y")),
    c("Borrower and loan characteristics", c("Y", "Y")),
    c("CBSA controls", c("Y", "Y")),
    c("Origination year FEs", c("N", "Y")),
    c("Instrument", c("Cyclicality-HPI", "Cyclicality-Month")),
    c("First stage partial F-Stat", c(F_guren, F_palmer)),
    c("Log Likelihood", logli),
    c("Observations", N)
  ),
  table.layout = "=ldc#-t-ad-",
  type = "latex",
  float = FALSE
) %>%
  write_lines("analysis/release/CRISM/iv_results_binned.tex")
