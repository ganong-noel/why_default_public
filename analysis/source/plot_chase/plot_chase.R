# INPUT
# "gn_strategic_latest.xls"

# OUTPUT
# "histogram_defaulter_LTV_chase.png"

data_path <- "analysis/input/disclose/"

ggsave_png <- function(plot_name) {
  ggsave(
    str_c(out_path, "/", plot_name, ".png"),
    height = 4.5,
    width = 8
  )
}

# LTV histogram
chase_ltv <- file.path(data_path, "latest/gn_strategic_latest.xls") %>%
  read_excel(sheet = "tbl_ltv_hist")

ltv_labels <- c(
  "0 - 20%",
  "21 - 40%",
  "41 - 60%",
  "61 - 80%",
  "81 - 100%",
  "101 - 120%",
  "121 - 140%",
  "141 - 160%",
  "161 - 180%",
  "181 - 200%",
  "201 - 220%",
  "> 220%"
)

chase_labels <- chase_ltv %>%
  pull(ltv_bin)

chase_ltv_labels <- tibble(
  ltv_bin = chase_labels,
  LTV_binned = ltv_labels
)

n_chase <- chase_ltv %>%
  pull(count) %>%
  sum()

chase_ltv <- chase_ltv %>%
  left_join(chase_ltv_labels,
    by = "ltv_bin"
  ) %>%
  mutate(
    share = count / n_chase,
    LTV_binned = factor(LTV_binned,
      levels = ltv_labels
    )
  )

palette <- brewer.pal("Greys", n = 9)

chase_ltv %>%
  ggplot(aes(
    x = LTV_binned,
    y = share
  )) +
  geom_col(fill = "#2b83ba") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    subtitle = "Share of mortgages",
    x = "Loan-to-value ratio at the time of default",
    y = ""
  ) +
  fte_theme() +
  theme(
    axis.text.x = element_text(
      size = 10,
      angle = 30
    ),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = palette[7])
  )

ggsave_png("histogram_defaulter_LTV_chase")