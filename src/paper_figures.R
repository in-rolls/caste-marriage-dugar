# Reproduce the three grouped bar charts on page 36 of the December 2010 draft.
source("src/functions.R")
suppressPackageStartupMessages(library(ggplot2))

paper_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    legend.position = "bottom", legend.title = element_blank(),
    plot.title.position = "plot", plot.caption = element_text(hjust = 0)
  )
read_figure <- function(n) read.csv(file.path(paths$output, paste0("paper_figure", n, ".csv")))
income_labels <- c("HI\nRs 35,000", "MI\nRs 15,000", "LI\nRs 7,000")
save_paper_figure <- function(plot, n) {
  ggsave(file.path(paths$figs, paste0("paper_figure", n, ".png")), plot,
    width = 9, height = 4.8, dpi = 180, bg = "white"
  )
}

for (n in 1:2) {
  data <- read_figure(n) |>
    mutate(
      income_level = factor(income_level, levels = c("HI", "MI", "LI")),
      groom_caste = factor(groom_caste, levels = c("HC", "MC", "LC"))
    )
  colours <- if (n == 1) {
    c(HC = "#6096c6", MC = "#ffd58f", LC = "#909090")
  } else {
    c(MC = "#6096c6", LC = "#ffd58f")
  }
  title <- if (n == 1) {
    "Original Figure 1. Brahmin responses by groom caste and income"
  } else {
    "Original Figure 2. Kayastha responses to own-caste and lower-caste grooms"
  }
  totals <- data |>
    distinct(income_level, denominator) |>
    arrange(income_level)
  denominators <- paste(paste0(totals$income_level, ": ", totals$denominator), collapse = "; ")
  note <- paste0(
    "Denominator: all ", if (n == 1) "Brahmin" else "Kayastha",
    " letters at each income (", denominators, ")."
  )
  source_note <- paste0(
    if (n == 2) "Includes letters to HCG. " else "",
    "Source: Table 4; December 2010 draft, Figure ", n, "."
  )
  plot <- ggplot(data, aes(income_level, proportion, fill = groom_caste)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7, colour = "grey40", linewidth = 0.2) +
    scale_fill_manual(values = colours, labels = c(HC = "HCG", MC = "MCG", LC = "LCG")) +
    scale_x_discrete(labels = income_labels) +
    scale_y_continuous(
      limits = c(0, if (n == 1) 0.8 else 0.6), breaks = (0:8) / 10,
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = title, x = "Groom monthly income", y = "Proportion of responses",
      caption = paste(note, source_note, sep = "\n")
    ) +
    paper_theme
  save_paper_figure(plot, n)
}

data <- read_figure(3) |>
  mutate(
    pair = factor(paste0(responder_caste, "R to ", groom_caste, "G"),
      levels = c("HCR to MCG", "HCR to LCG", "MCR to LCG")
    ),
    income_level = factor(income_level, levels = c("LI", "MI", "HI"))
  )
totals <- data |>
  distinct(pair, denominator) |>
  arrange(pair)
pair_denominators <- paste(totals$denominator, collapse = "; ")
plot <- ggplot(data, aes(pair, proportion, fill = income_level)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, colour = "grey40", linewidth = 0.2) +
  scale_fill_manual(
    values = c(LI = "#6096c6", MI = "#ffd58f", HI = "#696969"),
    labels = c(LI = "LI: Rs 7,000", MI = "MI: Rs 15,000", HI = "HI: Rs 35,000")
  ) +
  scale_y_continuous(limits = c(0, 0.7), breaks = (0:7) / 10, expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Original Figure 3. Cross-caste responses by groom income", x = NULL, y = "Proportion of responses",
    caption = paste(
      paste0(
        "Denominator: letters within each pair (", pair_denominators,
        "), across all three incomes."
      ),
      "Source: Table 4; December 2010 draft, Figure 3.",
      sep = "\n"
    )
  ) +
  paper_theme
save_paper_figure(plot, 3)
