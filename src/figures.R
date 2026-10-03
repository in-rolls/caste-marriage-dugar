# Audit figures: letter shares with a common responder-caste denominator, Table 6 compensation
# figures with their uncertainty and functional-form range, and letter shares
# by newspaper edition.

source("src/functions.R")
suppressPackageStartupMessages(library(ggplot2))

caste_colors <- c(HC = "#2a78d6", MC = "#eb6834", LC = "#1baf7a")
caste_labels <- c(HC = "Brahmin (HC)", MC = "Kayastha (MC)", LC = "Namasudra (LC)")
theme_set(theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    legend.position = "top", plot.title.position = "plot",
    strip.text = element_text(face = "bold")
  ))

letters <- readRDS(paths$letters)
sample <- analysis_sample(letters)

# Figure 1: share of each responder caste's letters going to each groom.
shares <- sample |>
  count(responder_caste, groom_caste, groom_income) |>
  group_by(responder_caste) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  mutate(
    responder_caste = factor(responder_caste,
      levels = c("HC", "MC", "LC"),
      labels = paste("Letters from", caste_labels, "families")
    ),
    groom_caste = factor(groom_caste, levels = c("HC", "MC", "LC"))
  )
fig_shares <- ggplot(shares, aes(groom_income / 1000, share, colour = groom_caste, group = groom_caste)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.6) +
  geom_text(
    data = filter(shares, groom_income == 35000),
    aes(label = caste_labels[as.character(groom_caste)]), hjust = -0.1, size = 3.2, show.legend = FALSE
  ) +
  facet_wrap(~responder_caste) +
  scale_colour_manual(values = caste_colors, guide = "none") +
  scale_x_continuous(breaks = c(7, 15, 35), limits = c(5, 50)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(
    x = "Advertised monthly income of the groom (thousand rupees)",
    y = "Share of the caste's letters",
    title = "Where each caste's 1,123 letters went",
    subtitle = "Nine fictitious grooms; each letter answered one. Line = groom caste."
  )
ggsave(file.path(paths$figs, "letter_shares.png"), fig_shares, width = 10, height = 4, dpi = 150, bg = "white")

# Figure 2: Table 6 with bootstrap intervals and the functional-form range.
unc <- read.csv(file.path(paths$output, "audit_table6_uncertainty.csv"))
seg <- read.csv(file.path(paths$output, "audit_table6_segment_slopes.csv"), check.names = FALSE)
logit <- read.csv(file.path(paths$output, "audit_table6_logit.csv"))
comp <- unc |>
  left_join(seg, by = c("responder_caste", "groom_caste", "income_level")) |>
  left_join(logit, by = c("responder_caste", "groom_caste", "income_level")) |>
  mutate(
    cell = paste0(caste_labels[responder_caste], " responders\n", caste_labels[groom_caste], " groom"),
    income_level = factor(income_level,
      levels = c("LI", "MI", "HI"),
      labels = c("Rs 7,000", "Rs 15,000", "Rs 35,000")
    )
  )
alt <- comp |>
  select(cell, income_level, `LI-MI slope` = `LI-MI`, `MI-HI slope` = `MI-HI`, `Logit` = compensation_logit_k) |>
  pivot_longer(-c(cell, income_level), names_to = "alternative", values_to = "value")
fig_comp <- ggplot(comp, aes(y = income_level)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_errorbar(aes(xmin = lower_boot, xmax = upper_boot), width = 0.25, colour = "#52514e", orientation = "y") +
  geom_point(aes(x = estimate), size = 3, colour = "#2a78d6") +
  geom_point(data = alt, aes(x = value, shape = alternative), size = 2.2, colour = "#eb6834") +
  scale_shape_manual(values = c(`LI-MI slope` = 1, `MI-HI slope` = 2, Logit = 5), name = "Alternative slope / model") +
  facet_wrap(~cell, ncol = 1) +
  labs(
    x = "Extra monthly income needed to match the own-caste groom's letter share (thousand rupees)",
    y = "Groom's advertised income",
    title = "Table 6 compensation figures with 95% letter-bootstrap intervals",
    subtitle = "Blue: the paper's figure. Orange: the same gap closed with a different income slope or a logit."
  )
ggsave(file.path(paths$figs, "compensation_uncertainty.png"), fig_comp, width = 9, height = 8, dpi = 150, bg = "white")

# Figure 3: shares by edition, HC and MC responders.
ed <- read.csv(file.path(paths$output, "audit_edition_shares.csv")) |>
  filter(responder_caste %in% c("HC", "MC")) |>
  mutate(
    groom = factor(groom, levels = groom_levels),
    edition = factor(edition, labels = c("2 Sept 2007", "16 Sept 2007")),
    responder_caste = factor(responder_caste,
      levels = c("HC", "MC"),
      labels = paste("Letters from", caste_labels[1:2], "families")
    )
  )
fig_ed <- ggplot(ed, aes(groom, share, fill = edition)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  facet_wrap(~responder_caste, ncol = 1) +
  scale_fill_manual(values = c("#2a78d6", "#1baf7a"), name = "Edition") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = NULL, y = "Share of the caste's letters in that edition",
    title = "Letter shares are similar across the two placements of the same nine ads"
  )
ggsave(file.path(paths$figs, "shares_by_edition.png"), fig_ed, width = 9, height = 6, dpi = 150, bg = "white")
