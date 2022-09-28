# Hardcodes data from Campbell and Cocco (2015) to be used for comparison to our
# model runs in `cc_table_2_dot_plots.R`

require(tidyverse)

cocco_hh_table_2 <-
  tribble(
    ~variable, ~default, ~underwater_no_d, ~cash_out, ~no_action,
    "Current LTV", 1.33, 1.15, .46, .55,
    "Price level", 1.2, 1.17, 1.32, 1.31,
    "Real house price", 0.51, .59, 1.29, 1.07,
    "Real income", 43.3, 50.3, 46.8, 54.7,
    "Real cons t - 1", 12.6, 14.4, 12.8, 15.2,
    "Mort/Inc", .4, .34, .38, .31,
    "(Mort - Rent)/Inc", .3, .25, .08, .12,
    "Nom int rate", .04, .038, .047, .044,
    "Age", 35.5, 34.5, 38.2, 38.2,
    "Probability", .048, NA, .625, NA
  )
cocco_hl_table_2 <-
  tribble(
    ~variable, ~default, ~underwater_no_d, ~cash_out, ~no_action,
    "Current LTV", 1.41, 1.15, .43, .54,
    "Price level", 1.23, 1.17, 1.35, 1.32,
    "Real house price", 0.45, .59, 1.32, 1.07,
    "Real income", 46.3, 48, 48.5, 54.7,
    "Real cons t - 1", 13.4, 14.2, 12.9, 14.5,
    "Mort/Inc", .33, .33, .33, .30,
    "(Mort - Rent)/Inc", .26, .24, .05, .11,
    "Nom int rate", .039, .038, .047, .044,
    "Age", 36.4, 34.6, 39, 38.3,
    "Probability", .037, NA, .595, NA
  )

cocco_ll_table_2 <-
  tribble(
    ~variable, ~default, ~underwater_no_d, ~cash_out, ~no_action,
    "Current LTV", 1.43, 1.11, .42, .55,
    "Price level", 1.17, 1.08, 1.29, 1.26,
    "Real house price", 0.45, .69, 1.34, 1.08,
    "Real income", 46.6, 47.7, 48.8, 52.5,
    "Real cons t - 1", 13.9, 15.1, 13.4, 14.9,
    "Mort/Inc", .33, .29, .31, .28,
    "(Mort - Rent)/Inc", .26, .22, .05, .12,
    "Nom int rate", .037, .022, .041, .039,
    "Age", 36.4, 33.2, 39.3, 38.3,
    "Probability", .044, NA, .583, NA
  )

cocco_lh_table_2 <-
  tribble(
    ~variable, ~default, ~underwater_no_d, ~cash_out, ~no_action,
    "Current LTV", 1.41, 1.11, .44, .56,
    "Price level", 1.16, 1.08, 1.28, 1.25,
    "Real house price", 0.46, .69, 1.32, 1.07,
    "Real income", 47.3, 49.5, 48.2, 54.4,
    "Real cons t - 1", 14.1, 15.4, 13.5, 15.5,
    "Mort/Inc", .35, .30, .35, .29,
    "(Mort - Rent)/Inc", .28, .23, .07, .12,
    "Nom int rate", .037, .022, .041, .036,
    "Age", 36.4, 33.2, 39.0, 38.2,
    "Probability", .046, NA, .602, NA
  )

cocco_fig_2 <-
  tribble(
    ~ltv, ~mti_difference, ~proportion, ~source,
    1, 0.16, .12, "hh",
    1.1, 0.055, .135, "hh",
    1.2, 0.039, .15, "hh",
    1.3, 0.040, .25, "hh",
    1.4, 0.049, .41, "hh",
    1.5, 0, .83, "hh",
    1, -0.012, 048, "hl",
    1.1, .025, .08, "hl",
    1.2, .008, .096, "hl",
    1.3, .0349, .21, "hl",
    1.4, .0307, .41, "hl",
    1.5, -.004, .81, "hl",
    1, .002, .03, "lh",
    1.1, .0407, .076, "lh",
    1.2, .035, .09, "lh",
    1.3, .0149, .13, "lh",
    1.4, .003, .48, "lh",
    1.5, .043, .827, "lh",
    1, -.012, .037, "ll",
    1.1, .018, .064, "ll",
    1.2, .01, .093, "ll",
    1.3, -.005, .1, "ll",
    1.4, .001, .46, "ll",
    1.5, .033, .82, "ll"
  )

order <-
  tribble(
    ~variable, ~order,
    "Current LTV", 1,
    "Price level", 2,
    "Real house price", 3,
    "Real income", 4,
    "Real cons t - 1", 5,
    "Mort/Inc", 6,
    "(Mort - Rent)/Inc", 7,
    "Nom int rate", 8,
    "Age", 9,
    "Probability", 10,
    "n", 11
  )
