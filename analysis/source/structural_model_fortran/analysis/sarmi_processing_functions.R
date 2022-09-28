# Creates functions for cleaning and analyzing the model output from `sarmi.m`

# Setup
require(tidyverse)
require(yaml)
require(rprojroot)

#####################
#  Helper functions #
#####################
bin_scores <-
  function(col, start, end, interval) {
    dat <- tibble(col)
    names(dat) <- c("binned")
    breaks <-
      tibble(
        lower_bound = c(-Inf, seq(start, end, interval)),
        upper_bound = c(seq(start, end, interval), Inf)
      ) %>%
      mutate(
        labels =
          case_when(
            lower_bound == -Inf ~ str_c("< ", upper_bound, "%"),
            upper_bound == Inf ~ str_c(lower_bound, "%+"),
            TRUE ~ str_c(lower_bound, "-", upper_bound - 1, "%")
          )
      )

    upper_bound <- breaks$upper_bound
    lower_bound <- breaks$lower_bound
    labels <- breaks$labels

    cases <- lapply(seq_along(labels), function(i) {
      expr(between(binned, lower_bound[!!i], upper_bound[!!i]) ~ labels[!!i])
    })

    dat %>%
      mutate(binned_ltv = case_when(!!!cases) %>%
        factor(levels = labels, ordered = TRUE)) %>%
      pull(binned_ltv)
  }

se <- function(col, na.rm = NULL) {
  if (!missing(na.rm)) {
    if (na.rm) {
      col <- col[!is.na(col)]
    }
  }
  sd(col) / sqrt(length(col))
}


fit_current_pattern <-
  function(run_number = 3, pattern) {
    file.path(
      data_path,
      glue::glue("simulation_{pattern}_{run_number}.raw")
    )
  }


#################################
#  Read and Process simulations #
#################################
col_names <- c(
  "id",
  "age",
  "simy",
  "simcons",
  "sav",
  "simcoh",
  "simdefc",
  "exp_gr",
  "simexpgi",
  "simph",
  "simpl",
  "simltv",
  "remdebt",
  "mpreal",
  "rentpay",
  "pvsimpi",
  "simmove",
  "perm_inc_shock_counts",
  "temp_inc_shock",
  "simw",
  "simvrent",
  "simvstay",
  "simconsstay",
  "simconsrent",
  "simwrent",
  "simwstay",
  "simcohrent",
  "simcohstay",
  "savrent",
  "savstay",
  "simtermwrent",
  "simtermwstay"
)


read_sim <- function(run_number, pattern, .col_names = col_names) {
  read_table(fit_current_pattern(run_number, pattern),
    col_names = .col_names
  ) %>%
    mutate(
      # from table II
      action = case_when(
        simdefc == 1 & lag(simdefc) == 0 ~ "default",
        simdefc == 0 & simltv >= 1 ~ "underwater_no_d",
        simdefc == 2 & lag(simdefc) == 0 ~ "cash_out",
        simdefc == 0 ~ "no_action",
        TRUE ~ "no_longer_mortgagor"
      ),
      mti = (mpreal / simy),
      mti_rent_adj = (mpreal - rentpay) / simy
    ) %>%
    group_by(id) %>%
    mutate_at(vars(simcons, simy, mpreal), .funs = list(lag = lag)) %>%
    ungroup()
}

read_multiple_sims <-
  function(pattern,
           n,
           actions = c("default", "cash_out")) {
    cocco <- tibble()
    for (k in 1:n) {
      temp <-
        read_sim(k, pattern) %>%
        group_by(id) %>%
        dplyr::mutate(
          delta_inc = simy - simy_lag,
          delta_cons = simcons - simcons_lag,
          run = k
        ) %>%
        filter(action %in% actions)
      cocco <- bind_rows(temp, cocco)
    }
    cocco
  }

bin_sims <- function(data) {
  data %>%
    ungroup() %>%
    dplyr::mutate(
      underwater_ltv_bins = bin_scores(simltv * 100, 100, 150, 10),
      wide_ltv_bins = bin_scores(simltv * 100, 34, 134, 20),
      ltv_bins_unadjusted = bin_scores(simltv * 100, 41, 141, 20),
      ltv_centiles = floor(simltv * 100),
      ltv_ventiles = floor(simltv * 20) * 5,
      simmove = factor(simmove,
        labels = c(
          "no move shock",
          "move shock"
        )
      )
    )
}

process_income_change_data <- function(full_sims, n_years = 5) {
  ever_mortgage_leaver <-
    # pull information from mortgage leavers
    full_sims %>%
    filter(action %in% c("default", "cash_out")) %>%
    transmute(
      id,
      run,
      final_ltv = simltv,
      mortgage_length = age,
      ltv_bins_unadjusted,
      move_shock = fct_relevel(simmove, "move shock", "no move shock")
    ) %>%
    # add back years before mortgage exit
    left_join(full_sims %>%
      dplyr::select(-ltv_bins_unadjusted),
    by = c("id", "run")
    ) %>%
    group_by(id, run) %>%
    arrange(desc(age)) %>%
    mutate(time_to_exit = row_number() - 1)


  aggregated_mortgage_leavers <-
    ever_mortgage_leaver %>%
    filter(mortgage_length > n_years, time_to_exit < n_years) %>%
    # relies on arrange above
    mutate(mpreal = last(mpreal)) %>%
    group_by(ltv_bins_unadjusted, move_shock, time_to_exit) %>%
    summarise(
      income = mean(simy),
      mti = mean(mti),
      se = se(simy),
      se_mti = se(mti),
      savings = mean(sav),
      se_savings = se(sav),
      n = n(),
      final_ltv = mean(final_ltv),
      mpreal = mean(mpreal),
      normalized_savings = savings / mpreal,
    ) %>%
    mutate(underwater = ifelse(final_ltv > 1.00, "underwater", "above water"))
}

process_simulation <-
  function(run_number = 1,
           pattern = "",
           .col_names = col_names,
           nsim = 40000,
           sim_data = NULL) {
    # For dot charts

    if (missing(sim_data)) {
      sim_data <- read_sim(run_number, pattern, .col_names)
    }

    sim_data %>%
      filter(action != "no_longer_mortgagor") %>%
      group_by(action) %>%
      transmute(
        "Current LTV" = simltv,
        "Price level" = simpl,
        "Real house price" = simph,
        "Real income" = simy,
        "Real cons t - 1" = simcons_lag,
        "Mort/Inc" = mti,
        "(Mort - Rent)/Inc" = mti_rent_adj,
        # simexpgi is real rate and exp_gr is
        "Nom int rate" = simexpgi * exp_gr - 1,
        "Age" = age + 30,
        n = n(),
        "Probability" = ifelse(action %in% c("cash_out", "default"),
          n / nsim,
          NA
        )
      ) %>%
      summarize_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE))) %>%
      gather(key = variable, value = value, 2:ncol(.)) %>%
      spread(key = action, value = value) %>%
      mutate(run = run_number)
  }

compute_event_study <-
  function(data, n_years = 5, y = simy) {
    aggregated_mortgage_leavers <-
      data %>%
      filter(mortgage_length > n_years, time_to_exit < n_years) %>%
      # relies on arrange above
      mutate(mpreal = last(mpreal)) %>%
      group_by(wide_ltv_bins, move_shock, time_to_exit) %>%
      summarize(
        y_ = mean(!!ensym(y)),
        se_ = se(!!ensym(y)),
        n = n(),
        final_ltv = mean(final_ltv),
      ) %>%
      mutate(underwater = ifelse(final_ltv > .94,
        "underwater",
        "above water"
      )) %>%
      ungroup()
  }


##########################
#  Make CC figures       #
##########################
source(file.path(code_path, "cocco_data.R"))

# event study of various variables around mortgage exit [pg new code]
greys <- brewer.pal("Greys", n = 9)

plot_event_study <- function(data, y = y_, se = se_, ylabel) {
  ylim_ <-
    data %>%
    summarise(min(!!ensym(y)), max(!!ensym(y))) %>%
    as.numeric()
  data %>%
    ggplot(aes(
      y = !!ensym(y),
      x = time_to_exit,
      color = wide_ltv_bins
    )) +
    geom_line() +
    geom_pointrange(
      aes(
        ymin = !!ensym(y) - !!ensym(se) * 1.96,
        ymax = !!ensym(y) + !!ensym(se) * 1.96
      ),
      position = position_dodge(width = .3)
    ) +
    facet_wrap(underwater ~ move_shock) +
    theme_minimal() +
    theme(
      plot.title.position = "plot",
      plot.subtitle = element_text(color = greys[6])
    ) +
    scale_x_reverse() +
    coord_cartesian(ylim = ylim_) +
    labs(
      x = "Time to mortgage exit (years)", y = "",
      subtitle = ylabel, color = "LTV at exit"
    )
}

make_dot_chart <-
  function(pattern,
           title,
           included_runs = c("hh", "hl", "lh", "ll"),
           subtitle = "Diamonds: HH: pink, LH: orange, LH: blue, LL: green
           Circles: Multiple simulations with given parameterization",
           n = 1,
           save_table_2_output = FALSE,
           sims_data = NULL) {
    if (missing(sims_data)) {
      sims_data <- map_dfr(1:n, process_simulation, pattern)
    }

    all_runs <- c("hh", "hl", "lh", "ll")
    excluded_runs <- setdiff(all_runs, included_runs)

    data_with_prob <-
      sims_data %>%
      filter(str_detect(variable, "_mean")) %>%
      mutate(variable = str_replace(variable, "_mean", ""), run = "sim") %>%
      bind_rows(
        cocco_hh_table_2 %>% mutate(run = "hh"),
        cocco_hl_table_2 %>% mutate(run = "hl"),
        cocco_lh_table_2 %>% mutate(run = "lh"),
        cocco_ll_table_2 %>% mutate(run = "ll")
      ) %>%
      gather(-c("variable", "run"), key = "type", value = "value") %>%
      filter(
        !run %in% excluded_runs,
        !variable %in% c("n", "Age", "Final income")
      ) %>%
      filter(!(variable == "Probability" &
        !type %in% c("default", "cash_out"))) %>%
      mutate(
        type = case_when(
          type == "default" ~ "Default",
          type == "cash_out" ~ "Cash out",
          type == "underwater_no_d" ~ "Underwater",
          type == "no_action" ~ "Above water"
        ),
        type = factor(
          type,
          levels = c(
            "Above water",
            "Underwater",
            "Cash out",
            "Default"
          )
        )
      )

    run_names <- our_names[our_names %in% data_with_prob$run]

    data_with_prob %>%
      filter(variable != "Probability") %>%
      ggplot(aes(
        x = value,
        y = type,
        color = run,
        shape = run
      ),
      alpha = .01
      ) +
      geom_point() +
      facet_wrap(~variable, scales = "free_x") +
      scale_color_manual(
        values = colors,
        breaks = run_names,
        labels = legend_labels[run_names]
      ) +
      scale_shape_manual(
        values = shapes,
        breaks = run_names,
        labels = legend_labels[run_names]
      ) +
      fte_theme() +
      theme(
        axis.text.x = element_text(angle = 50, hjust = 1),
        strip.background = element_rect(fill = "white"),
        legend.position = c(1, 0.05),
        legend.justification = c(1, 0.05),
        legend.text = element_text(size = 12)
      ) +
      labs(
        y = "",
        x = ""
      )



    if (save_table_2_output) {
      ggsave(file.path(
        out_path,
        glue::glue("table_2_rep_{included_runs}.png")
      ),
      width = 7,
      height = 5
      )
    }
  }


make_income_drop_bar_two_panel <- function(cc_data, gn_data, title = "",
                                           width = 0.9, reverse = FALSE) {
  comparison_data <-
    cc_data %>%
    filter(
      move_shock == "no move shock",
      time_to_exit %in% c(0, 4)
    ) %>%
    filter(!ltv_bins_unadjusted %in% c("< 41%", "41-60%", "61-80%")) %>%
    mutate(
      ltv_bins = fct_recode(
        ltv_bins_unadjusted,
        "< 100%" = "81-100%"
      ),
      ltv_bins = fct_drop(ltv_bins, only = c("< 41%", "41-60%", "61-80%"))
    ) %>%
    group_by(ltv_bins) %>%
    transmute(
      time_to_exit = ifelse(time_to_exit != 0, -12, 0),
      normalized_delta_inc = (income - last(income)) / mpreal,
      source = ifelse(stigma == 0,
        "Model: Baseline",
        "Model: Utility Cost of Default"
      )
    ) %>%
    bind_rows(gn_data) %>%
    ungroup() %>%
    mutate(ltv_bins = fct_rev(ltv_bins))

  stigma_level <- cc_data %>%
    pull(title) %>%
    first() %>%
    str_split("_", simplify = TRUE) %>%
    .[[2]]

  # bar_chart
  if (reverse) {
    base <- comparison_data %>%
      ggplot(aes(
        y = normalized_delta_inc,
        x = fct_rev(ltv_bins),
        fill = fct_rev(source)
      ))
  } else {
    base <- comparison_data %>%
      ggplot(aes(
        y = normalized_delta_inc,
        x = fct_rev(ltv_bins),
        fill = source
      ))
  }
  base <- base +
    geom_col(
      position = "dodge2",
      width = width,
      data = comparison_data %>% filter(time_to_exit < 0)
    ) +
    coord_cartesian(ylim = c(-1.15, 0)) +
    scale_y_continuous(labels = scales::percent) +
    fte_theme() +
    scale_fill_manual(values = c("#2B83BA", "#ABDDA4")) +
    theme(
      legend.position = c(0, 0),
      legend.justification = c(0, 0),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 17, color = greys[6]),
      axis.title = element_text(size = 17),
      axis.text = element_text(size = 17)
    ) +
    labs(
      x = "Loan-to-value ratio",
      y = "",
      subtitle = "Income change from pre-period (as share of payment due)"
    )


  (plot_1 <-
    base +
    geom_col(
      position = "dodge2",
      width = width,
      data = comparison_data %>%
        filter(ltv_bins %in% c("< 100%"), time_to_exit == 0)
    ))

  (plot_2 <-
    plot_1 +
    geom_col(
      position = "dodge2",
      width = width,
      data = comparison_data %>%
        filter(ltv_bins %in% c("101-120%"), time_to_exit == 0)
    ))

  (plot_3 <-
    plot_2 +
    geom_col(
      position = "dodge2",
      width = width,
      data = comparison_data %>%
        filter(time_to_exit == 0)
    ))


  list(plot_1, plot_2, plot_3)
}

make_income_drop_bar_one_panel <- function(cc_data, gn_data, title = "", width = 0.9) {
  comparison_data <-
    cc_data %>%
    filter(
      move_shock == "no move shock",
      time_to_exit %in% c(0, 4)
    ) %>%
    filter(!ltv_bins_unadjusted %in% c("< 41%", "41-60%", "61-80%")) %>%
    mutate(
      ltv_bins = fct_recode(
        ltv_bins_unadjusted,
        "< 100%" = "81-100%"
      ),
      ltv_bins = fct_drop(ltv_bins, only = c("< 41%", "41-60%", "61-80%"))
    ) %>%
    group_by(ltv_bins, stigma) %>%
    transmute(
      time_to_exit = ifelse(time_to_exit != 0, -12, 0),
      normalized_delta_inc = (income - last(income)) / mpreal,
      source = ifelse(stigma == 0,
        "Model: Baseline",
        "Model: Utility Cost of Default"
      )
    ) %>%
    bind_rows(gn_data) %>%
    ungroup() %>%
    mutate(ltv_bins = fct_rev(ltv_bins))

  stigma_level <- cc_data %>%
    pull(title) %>%
    first() %>%
    str_split("_", simplify = TRUE) %>%
    .[[2]]

  # bar_chart
  base <-
    comparison_data %>%
    ggplot(aes(
      y = normalized_delta_inc,
      x = fct_rev(ltv_bins),
      fill = source
    )) +
    geom_col(
      position = "dodge2",
      width = width,
      data = comparison_data %>% filter(time_to_exit < 0)
    ) +
    coord_cartesian(ylim = c(-1.3, 0)) +
    scale_y_continuous(labels = scales::percent) +
    fte_theme() +
    # scale_fill_manual(values = c("#2B83BA", "#ABDDA4", "#31a354")) +
    theme(
      legend.position = c(0, 0),
      legend.justification = c(0, 0)
    ) +
    labs(
      x = "Loan-to-value ratio",
      y = "Income change from pre-period\n(as share of payment due)"
    )

  (plot_1 <-
    base +
    geom_col(
      position = "dodge2",
      width = width,
      data = comparison_data %>%
        filter(ltv_bins %in% c("< 100%", "101-120%"), time_to_exit == 0) %>%
        mutate(
          normalized_delta_inc =
            ifelse(source == "Model: Utility Cost of Default",
              NA,
              normalized_delta_inc
            )
        )
    ) +
    scale_fill_manual(
      values = c("#2B83BA", "#ABDDA4", "#FFFFFF"),
      labels = c("Data", "Model: Baseline", "")
    ))


  (plot_2 <-
    plot_1 +
    geom_col(
      position = "dodge2",
      width = width,
      data = comparison_data %>%
        filter(time_to_exit == 0) %>%
        mutate(
          normalized_delta_inc =
            ifelse(source == "Model: Utility Cost of Default",
              NA,
              normalized_delta_inc
            )
        )
    ) +
    scale_fill_manual(
      values = c("#2B83BA", "#ABDDA4", "#FFFFFF"),
      labels = c("Data", "Model: Baseline", " ")
    ))


  (plot_3 <-
    plot_2 +
    geom_col(
      position = "dodge2",
      width = width,
      data = comparison_data %>%
        filter(time_to_exit == 0)
    ) +
    scale_fill_manual(
      values = c("#2B83BA", "#ABDDA4", "#31a354"),
      labels = c(
        "Data",
        "Model: Baseline",
        "Model: Utility Cost of Default"
      )
    ))

  list(plot_1, plot_2, plot_3)
}

make_savings_bar <- function(cc_data, gn_data, title = "") {
  comparison_data <-
    cc_data %>%
    filter(
      move_shock == "no move shock",
      time_to_exit == 0
    ) %>%
    filter(!ltv_bins_unadjusted %in% c("< 41%", "41-60%", "61-80%")) %>%
    mutate(
      ltv_bins = fct_recode(ltv_bins_unadjusted,
        "< 100%" = "81-100%"
      ),
      ltv_bins = fct_drop(ltv_bins, only = c("< 41%", "41-60%", "61-80%"))
    ) %>%
    group_by(ltv_bins) %>%
    transmute(
      normalized_savings,
      source = "Model"
    ) %>%
    bind_rows(gn_data) %>%
    ungroup() %>%
    mutate(ltv_bins = fct_rev(ltv_bins))

  stigma_level <- cc_data %>%
    pull(title) %>%
    first() %>%
    str_split("_", simplify = TRUE) %>%
    .[[2]]


  # bar_chart
  palette <- brewer.pal("Greys", n = 9)
  base <-
    comparison_data %>%
    ggplot(aes(
      y = normalized_savings,
      x = fct_rev(ltv_bins),
      fill = source
    )) +
    geom_col(
      position = "dodge2",
      data = comparison_data
    ) +
    geom_text(
      aes(label = paste0(
        100 * round(normalized_savings, digits = 2), "%"
      )),
      color = palette[6],
      vjust = -0.5,
      position = position_dodge(.9),
      size = 3.0
    ) +
    # coord_cartesian(ylim = c(-1.15, 0)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 9)) +
    fte_theme() +
    scale_fill_manual(values = c("#2B83BA", "#ABDDA4")) +
    theme(
      legend.position = c(0, 1),
      legend.justification = c(0, 1)
    ) +
    labs(
      x = "Loan-to-value ratio",
      y = "Assets at time of default\n(as share of payment due)"
    )

  # base +
  #   geom_col(position = "dodge2",
  #            data = comparison_data %>% filter(time_to_exit == 0))
}
