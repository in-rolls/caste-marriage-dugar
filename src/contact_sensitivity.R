# Brahmin letters to LC versus Brahmin grooms at the same advertised income.
# The consideration ratio is a sensitivity parameter, never an estimated exposure.
source("src/functions.R")
suppressPackageStartupMessages(library(ggplot2))

letters <- readRDS(paths$letters)
samples <- list(retained = analysis_sample(letters), all_recorded = letters)
results <- bind_rows(lapply(names(samples), function(sample_name) {
  bind_rows(lapply(c("pooled", "edition_1", "edition_2"), function(period) {
    data <- samples[[sample_name]] |>
      filter(responder_caste == "HC", groom_caste %in% c("HC", "LC"))
    if (period != "pooled") {
      data <- filter(data, edition == as.integer(sub("edition_", "", period)))
    }
    bind_rows(lapply(sort(unique(groom_design$groom_income)), function(income) {
      cell <- filter(data, groom_income == income)
      own <- sum(cell$groom_caste == "HC")
      other <- sum(cell$groom_caste == "LC")
      inference <- contact_count_ratio(other, own)
      tibble(
        sample = sample_name, period = period, groom_income = income,
        n_own = own, n_lc = other, count_ratio = inference[["ratio"]],
        lower = inference[["lower"]], upper = inference[["upper"]]
      )
    }))
  }))
}))
write_table(results, "contact_consideration_thresholds")

# Fixed relative consideration and an income-invariant relative letter rate
# jointly imply the same LC/HC count ratio at all incomes.
income_tests <- results |>
  group_by(sample, period) |>
  group_modify(function(data, keys) {
    data <- arrange(data, groom_income)
    masters <- filter(data, groom_income %in% c(15000, 35000)) |>
      arrange(desc(groom_income))
    all_test <- fisher.test(as.matrix(select(data, n_lc, n_own)))
    masters_test <- fisher.test(as.matrix(select(masters, n_lc, n_own)))
    tibble(
      p_all_incomes = all_test$p.value, p_masters_only = masters_test$p.value,
      high_vs_mid_ratio = masters$count_ratio[1] / masters$count_ratio[2],
      conditional_odds_ratio = unname(masters_test$estimate),
      lower = masters_test$conf.int[1], upper = masters_test$conf.int[2]
    )
  }) |>
  ungroup()
write_table(income_tests, "contact_income_tests")

# For lambda_j = exposure_j * rate_j, rate_LC/rate_HC = count_ratio / e.
grid <- tidyr::crossing(
  filter(results, sample == "retained", period == "pooled"),
  consideration_ratio = exp(seq(log(0.025), log(2), length.out = 201))
) |>
  mutate(
    rate_ratio = count_ratio / consideration_ratio,
    rate_lower = lower / consideration_ratio, rate_upper = upper / consideration_ratio
  )
write_table(grid, "contact_consideration_curves")

main <- filter(results, sample == "retained", period == "pooled")
plot <- ggplot(grid, aes(consideration_ratio, rate_ratio)) +
  geom_ribbon(aes(ymin = rate_lower, ymax = rate_upper), fill = "#6096c6", alpha = 0.2) +
  geom_hline(yintercept = 1, colour = "grey35", linewidth = 0.4) +
  geom_vline(xintercept = 1, colour = "grey45", linetype = "dotted", linewidth = 0.4) +
  geom_line(colour = "#346b9c", linewidth = 0.7) +
  geom_point(data = main, aes(x = count_ratio, y = 1), colour = "#346b9c", size = 2) +
  facet_wrap(~groom_income,
    nrow = 1,
    labeller = as_labeller(c(`7000` = "Rs 7,000", `15000` = "Rs 15,000", `35000` = "Rs 35,000"))
  ) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.5, 1, 2)) +
  scale_y_log10(breaks = c(0.05, 0.2, 1, 5, 20)) +
  labs(
    x = "Assumed consideration ratio: LC ad / Brahmin ad (log scale)",
    y = "Implied letter rate ratio\nLC / Brahmin (log scale)"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey95"))
ggsave(file.path(paths$figs, "contact_consideration.png"), plot,
  width = 9, height = 3.8, dpi = 200, bg = "white"
)
