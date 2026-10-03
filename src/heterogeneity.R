source("src/functions.R")
source("src/heterogeneity_functions.R")
suppressPackageStartupMessages(library(ggplot2))

letters <- readRDS(paths$letters)
retained <- analysis_sample(letters)
model <- fit_attribute_index(retained)
no_education <- fit_attribute_index(retained, omit_education = TRUE)
scored <- assign_attribute_groups(letters, model)
write_table(
  select(scored, letter_id, responder_caste, repeat_letter, score, attribute_group),
  "heterogeneity_assignments"
)
rules <- bind_rows(lapply(names(model$cutoffs), function(caste) {
  tibble(
    responder_caste = caste, lower_cutoff = model$cutoffs[[caste]][1],
    upper_cutoff = model$cutoffs[[caste]][2]
  )
}))
write_table(rules, "heterogeneity_cutoffs")
write_table(tibble(
  attribute = names(model$medians), median = model$medians,
  loading = model$orientation * model$fit$rotation[, 1]
), "heterogeneity_index")

complete <- complete.cases(quality_attributes(retained))
variants <- list(
  retained = filter(scored, !repeat_letter),
  edition_1 = filter(scored, !repeat_letter, edition == 1),
  edition_2 = filter(scored, !repeat_letter, edition == 2),
  all_recorded = scored, excluded = filter(scored, repeat_letter),
  no_education = assign_attribute_groups(retained, no_education),
  complete_case = assign_attribute_groups(retained[complete, ], model),
  education = education_groups(retained)
)
counts <- bind_rows(lapply(names(variants), function(name) {
  mutate(subgroup_counts(variants[[name]]), specification = name)
}))
write_table(counts, "heterogeneity_counts")
sizes <- counts |>
  distinct(specification, responder_caste, attribute_group, group_letters)
write_table(sizes, "heterogeneity_sizes")
directions <- counts |>
  mutate(direction = case_when(
    responder_caste == groom_caste ~ "Own",
    match(groom_caste, c("HC", "MC", "LC")) < match(responder_caste, c("HC", "MC", "LC")) ~ "Up",
    TRUE ~ "Down"
  )) |>
  group_by(specification, responder_caste, attribute_group, direction) |>
  summarise(letters = sum(letters), group_letters = first(group_letters), .groups = "drop") |>
  mutate(share = letters / group_letters)
write_table(directions, "heterogeneity_directions")

missing <- bind_cols(select(retained, responder_caste, edition, groom_caste), quality_attributes(retained)) |>
  pivot_longer(-c(responder_caste, edition, groom_caste), names_to = "attribute", values_to = "value") |>
  group_by(responder_caste, edition, groom_caste, attribute) |>
  summarise(letters = n(), missing = sum(is.na(value)), fraction_missing = mean(is.na(value)), .groups = "drop")
write_table(missing, "heterogeneity_missing")

main <- filter(counts, specification == "retained")
gaps <- brahmin_gaps(main)
write_table(gaps, "heterogeneity_brahmin_gaps")
crossings <- bind_rows(lapply(c("T1", "T2", "T3"), function(g) {
  x <- filter(gaps, attribute_group == g)
  roots <- response_crossings(x$groom_income, x$difference)
  if (nrow(roots) == 0) roots <- data.frame(lower_income = NA_real_, upper_income = NA_real_)
  mutate(roots, attribute_group = g)
}))
write_table(crossings, "heterogeneity_crossings")

tests <- bind_rows(lapply(c(7000L, 15000L, 35000L), function(income) {
  x <- filter(gaps, groom_income == income)
  test <- fisher.test(as.matrix(select(x, HC, LC)))
  tibble(
    groom_income = income, letters = sum(x$HC + x$LC), p = test$p.value,
    bottom_lc_fraction = x$LC[1] / (x$LC[1] + x$HC[1]),
    top_lc_fraction = x$LC[3] / (x$LC[3] + x$HC[3])
  )
})) |>
  mutate(bottom_minus_top = bottom_lc_fraction - top_lc_fraction, p_holm = p.adjust(p, "holm"))
write_table(tests, "heterogeneity_tests")

boot <- bootstrap_heterogeneity(retained)
write.csv(boot$gaps, file.path(paths$output, "heterogeneity_bootstrap_gaps.csv"), row.names = FALSE)
write.csv(boot$crossings, file.path(paths$output, "heterogeneity_bootstrap_crossings.csv"), row.names = FALSE)
intervals <- boot$gaps |>
  group_by(attribute_group, groom_income) |>
  summarise(
    lower = quantile(difference, 0.025), upper = quantile(difference, 0.975),
    fraction_lc_ahead = mean(difference > 0), fraction_equal = mean(difference == 0),
    draws = n(), .groups = "drop"
  )
write_table(intervals, "heterogeneity_intervals")
crossing_frequency <- boot$crossings |>
  group_by(attribute_group, draw) |>
  summarise(
    crossings = sum(!is.na(lower_income)),
    equality_interval = any(upper_income > lower_income, na.rm = TRUE), .groups = "drop"
  ) |>
  group_by(attribute_group) |>
  summarise(
    draws = n(), no_crossing = sum(crossings == 0),
    one_crossing = sum(crossings == 1), multiple_crossings = sum(crossings > 1),
    equality_interval = sum(equality_interval), .groups = "drop"
  )
write_table(crossing_frequency, "heterogeneity_crossing_frequency")

plot_data <- left_join(gaps, intervals, by = c("attribute_group", "groom_income"))
stopifnot(nrow(plot_data) == nrow(gaps))
plot <- ggplot(plot_data, aes(groom_income / 1000, 100 * difference)) +
  geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed") +
  geom_errorbar(aes(ymin = 100 * lower, ymax = 100 * upper), width = 1, colour = "#346b9c") +
  geom_line(colour = "#346b9c", linewidth = 0.6) +
  geom_point(colour = "#346b9c", size = 2) +
  facet_wrap(~attribute_group,
    nrow = 1,
    labeller = as_labeller(c(T1 = "Lower third", T2 = "Middle third", T3 = "Upper third"))
  ) +
  scale_x_continuous(breaks = c(7, 15, 35)) +
  labs(
    x = "Advertised monthly income (thousand rupees)",
    y = "LC minus Brahmin share of letters\n(percentage points within attribute group)"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey95"))
ggsave(file.path(paths$figs, "heterogeneity_brahmin.png"), plot,
  width = 9, height = 3.8, dpi = 200, bg = "white"
)
