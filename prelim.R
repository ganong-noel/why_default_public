blues <- brewer.pal("Blues", n = 3)[c(3, 2)]

fte_theme <- function(font_family = "serif",
                      legend_title_on = FALSE,
                      dark_text = FALSE) {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n = 9)
  color.background <- "white"
  color.grid.major <- palette[3]
  color.axis.text <- if_else(dark_text, palette[7], palette[6])
  color.axis.title <- palette[7]
  color.title <- palette[9]

  if (legend_title_on) {
    legend_title <- element_text(colour = color.axis.title)
    legend_text <- element_text(colour = color.axis.text)
  } else {
    legend_title <- element_blank()
    legend_text <- element_text(
      colour = color.axis.title,
      size = rel(1.5)
    )
  }

  # Begin construction of chart
  theme_bw(base_size = 12, base_family = font_family) +

    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = color.background, color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(color = color.background)) +

    # Format the grid
    theme(panel.grid.major = element_line(color = color.grid.major, size = .25)) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +

    # Format the legend, but hide by default
    theme(legend.position = "none") +
    theme(legend.background = element_rect(fill = color.background)) +
    theme(legend.title = legend_title) +
    theme(legend.text = legend_text) +

    # Set title and axis labels, and format these and tick marks
    theme(axis.text = element_text(size = rel(1), color = color.axis.text)) +
    theme(axis.title.x = element_text(color = color.axis.title, vjust = 0)) +
    theme(axis.title.y = element_text(color = color.axis.title, vjust = 1.25)) +

    # colour and size caption

    theme(plot.caption = element_text(color = color.axis.title)) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

coef_label <- function(text, loc) {
  if (loc == "ur") {
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1.07, family = "serif", color = brewer.pal("Greys", n = 9)[7], label = text)
  } else if (loc == "lr") {
    annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -0.03, family = "serif", color = brewer.pal("Greys", n = 9)[7], label = text)
  } else if (loc == "ul") {
    annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1.07, family = "serif", color = brewer.pal("Greys", n = 9)[7], label = text)
  } else if (loc == "ll") {
    annotate("text", x = -Inf, y = -Inf, hjust = 0, vjust = -0.03, family = "serif", color = brewer.pal("Greys", n = 9)[7], label = text)
  }
}

# Caution -- when working with grouped data, use
# 1) df <- df %>% ungroup() %>% mutate(x = winsor(x, 0.01, 0.99))
# or
# 2) df$x   = winsor(df$x, 0.01, 0.99)

winsor <- function(x, fraction_low = 0.05, fraction_high = .95,
                   value_low = NA, value_high = NA,
                   verbose = TRUE, varname = "var",
                   positive_only = FALSE,
                   fractionLow = NA, fractionHigh = NA,
                   pos_only = NA) {
  # check inputs
  if (!is.na(value_low)) {
    fraction_low <- ecdf(x)(value_low)
  }

  if (!is.na(value_high)) {
    fraction_high <- ecdf(x)(value_high)
  }

  if (!is.na(fractionLow)) {
    print("Warning: fractionLow is deprecated please use fraction_low")
    fraction_low <- fractionLow
  }

  if (!is.na(fractionHigh)) {
    print("Warning: fractionHigh is deprecated please use fraction_low")
    fraction_high <- fractionHigh
  }

  if (!is.na(pos_only)) {
    print("Warning: pos_only is deprecated please use positive_only")
    positive_only <- pos_only
  }


  if (length(fraction_low) != 1 || fraction_low < 0 ||
    fraction_low > 0.5) {
    stop("bad value for 'fraction_low'")
  }
  if (length(fraction_high) != 1 || fraction_high < 0 ||
    fraction_high < 0.5) {
    stop("bad value for 'fraction_high'")
  }

  # compute limits
  if (positive_only == TRUE) {
    x_quantile_sample <- x[x > 0]
  } else {
    x_quantile_sample <- x
  }
  lim <- quantile(x_quantile_sample,
    probs = c(0, fraction_low, fraction_high, 1),
    na.rm = TRUE
  )
  min_str <- min(x)
  max_str <- max(x)

  # winsorize
  lim[2] <- ifelse(fraction_low == 0, min_str, lim[2])
  lim[3] <- ifelse(fraction_high == 1, max_str, lim[3])
  x[x < lim[2]] <- lim[2]
  x[x > lim[3]] <- lim[3]
  if (verbose == TRUE) {
    if (fraction_low != 0) {
      print(paste0(
        varname, " min was ", lim[1], ", now winsorized to ",
        fraction_low, " percentile of ", min(x, na.rm = TRUE)
      ))
    }
    if (fraction_high != 1) {
      print(paste0(
        varname, " max was ", lim[4], ", now winsorized to ",
        fraction_high, " percentile of ", max(x, na.rm = TRUE)
      ))
    }
  }

  return(x)
}

save_animation <- function(out_series,
                           out_path,
                           out_name,
                           width = 6,
                           height = 4,
                           verbose = FALSE) {
  stopifnot(is_list(out_series))
  stopifnot((map_lgl(out_series, is.ggplot) %>% all()))


  n <- length(out_series)

  for (i in 1:n) {
    file_name <- file.path(out_path, str_c(out_name, "_", i, "_of_", n, ".png"))
    if (verbose) print(file_name)
    ggsave(
      file = file_name,
      plot = out_series[[i]],
      width = width,
      height = height
    )
  }
}


test_equal_stat <- function(desc, object, expected) {
  # this function tests if an object is equal to an expected value
  # it then either creates or adds to a data frame `stats_text` the test description and value
  # sample call: stats_text <- test_equal_stat("rows in mpg ", nrow(mpg), 234)

  test_that("", expect_equal(object, expected))

  if (!exists("stats_text")) {
    return(tibble(desc, expected))
  }

  stopifnot(nrow(stats_text) == n_distinct(stats_text[["desc"]]))
  if (stats_text[["desc"]] %>% str_detect(desc) %>% any()) {
    str_c(
      "test: ", desc,
      ", recording ", expected,
      ", replaces old value of ",
      stats_text[str_detect(stats_text[["desc"]], desc), "expected"] %>% as.numeric()
    ) %>%
      print()
    stats_text[str_detect(stats_text[["desc"]], desc), ] <- tibble(desc, expected)
  } else {
    stats_text %>% bind_rows(tibble(desc, expected))
  }
}


label_cuts <- function(type) {
  alternate_dollar <- function(x) {
    dollar_ouput <- rep("", length(x))

    dollar_ouput[x >= 0] <- scales::dollar(x[x >= 0])

    dollar_ouput[x < 0] <- str_c("-", scales::dollar(abs(x[x < 0])))

    return(dollar_ouput)
  }

  function(x) {
    vars <- str_split_fixed(x, ",", 2)
    end_points <- as.numeric(str_remove(vars[, 2], "\\)|\\]"))
    end_type <- str_detect(vars[, 2], "\\)")

    start_points <- as.numeric(str_remove(vars[, 1], "\\(|\\["))
    start_type <- str_detect(vars[, 1], "\\(")


    if (type == "dollar") {
      output <- str_c(
        alternate_dollar(start_points + start_type), "-",
        alternate_dollar(end_points - end_type)
      )

      output %>%
        str_replace("-Inf-", "<") %>%
        str_replace("-Inf", "+")
    } else if (type == "percent") {
      output <- str_c(
        scales::percent(start_points + 0.01 * start_type,
          accuracy = 1,
          suffix = ""
        ), "-",
        scales::percent(end_points - 0.01 * end_type,
          accuracy = 1
        )
      )

      output %>%
        str_replace("-Inf-", "<") %>%
        str_replace("-Inf", "%+")
    }
  }
}


animate_factor <- function(data, # A dataframe with the data for plotting
                           plotting, # A function that takes the dataframe as an argument
                           # and returns the final frame, excluding the animated aesthetics
                           y_aesthetic, # the name of the variable getting mapped to the y-aesthetic
                           factor_variable, # the name of the variable getting animated. It should be a
                           # factor with the levels corresponding to the order
                           labels, # a named vector with the labels of the factor variable as they should
                           # appear on the legend
                           aesthetics # a list of lists. Each list should contain a scale_*_manual function
                           # as the first element and the values that should be supplied to the
                           # value argument of the function as the second
) {
  construct_plot <- function(plot_number) {
    names_vec <- names(labels)[seq(1, plot_number)]

    plot <- data %>%
      mutate(!!ensym(y_aesthetic) := if_else(!!ensym(factor_variable) %in% names_vec,
        !!ensym(y_aesthetic),
        NA_real_
      )) %>%
      plotting() +
      theme(legend.justification = c(0, 1))


    for (i in 1:length(aesthetics)) {
      plot <- plot +
        aesthetics[[i]][[1]](labels = labels[seq(1, plot_number)],
          values = aesthetics[[i]][[2]],
          breaks = names(labels)[seq(1, plot_number)])
    }
    return(plot)
  }



  map(1:length(labels), construct_plot)
}
