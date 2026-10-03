# Side-by-side comparison of every reproduced number with its published value.

source("src/functions.R")
source("src/published_values.R")

read_out <- function(name) read.csv(file.path(paths$output, paste0(name, ".csv")), check.names = FALSE)

long_compare <- function(published, reproduced, keys, label, tol) {
  p <- pivot_longer(published, -all_of(keys), names_to = "quantity", values_to = "published")
  r <- pivot_longer(reproduced, -all_of(keys), names_to = "quantity", values_to = "reproduced")
  inner_join(p, r, by = c(keys, "quantity")) |>
    mutate(
      table = label, difference = reproduced - published,
      match = !is.na(published) & abs(difference) <= tol, .before = 1
    )
}

t4 <- long_compare(published_table4, read_out("table4_counts") |> select(groom, HC, MC, LC), "groom", "Table 4", 0)

t5_rep <- read_out("table5_lpm") |>
  select(responder_caste, term, estimate, se_hc1, p_hc1) |>
  pivot_wider(names_from = responder_caste, values_from = c(estimate, se_hc1, p_hc1)) |>
  transmute(term,
    HC_est = estimate_HC, HC_se = se_hc1_HC, MC_est = estimate_MC, MC_se = se_hc1_MC,
    LC_est = estimate_LC, LC_p = p_hc1_LC
  )
t5 <- long_compare(published_table5, t5_rep, "term", "Table 5", 0.0001) |>
  mutate(match = ifelse(quantity == "LC_p", round(reproduced, 3) == published, round(reproduced, 4) == published))

t5_fit <- tibble(
  responder_caste = names(published_table5_n),
  quantity = "n", published = published_table5_n
) |>
  bind_rows(tibble(responder_caste = names(published_table5_r2), quantity = "r2", published = published_table5_r2)) |>
  left_join(pivot_longer(read_out("table5_lpm_fit"), -responder_caste, names_to = "quantity", values_to = "reproduced"),
    by = c("responder_caste", "quantity")
  ) |>
  mutate(
    table = "Table 5 fit", difference = reproduced - published,
    match = ifelse(quantity == "n", difference == 0,
      round(reproduced, ifelse(responder_caste == "LC", 3, 2)) == published
    ), .before = 1
  )

fn <- published_footnote_tests |>
  left_join(read_out("table5_footnote_tests") |> select(responder_caste, hypothesis, f_rep = f, p_rep = p),
    by = c("responder_caste", "hypothesis")
  ) |>
  transmute(
    table = "Footnote F-tests", responder_caste, hypothesis, verdict,
    published = f, reproduced = f_rep, p_reproduced = p_rep, difference = f_rep - f,
    verdict_reproduced = case_when(
      p_rep < 0.01 ~ "reject_1pct", p_rep < 0.05 ~ "reject_5pct",
      p_rep < 0.10 ~ "reject_10pct", TRUE ~ "not_rejected"
    ),
    match = ifelse(is.na(f), verdict == verdict_reproduced, abs(difference) < 0.01)
  )
write_table(fn, "comparison_footnote_tests")

# The worksheet rounds coefficients before calculating and zeroes two MC terms.
t6 <- long_compare(published_table6, read_out("table6_worksheet"), "pair", "Table 6", 0.005)
t6_formula <- long_compare(
  published_table6, read_out("table6_compensation"),
  "pair", "Table 6 unrestricted", 0.05
)
worksheet_check <- long_compare(
  read_out("table6_worksheet_source"), read_out("table6_worksheet"),
  "pair", "Worksheet", 1e-10
)
write_table(worksheet_check, "comparison_table6_worksheet")
stopifnot(nrow(worksheet_check) == 15L, all(worksheet_check$match))

# Height means differ by up to 0.09 in because the paper reads 4.11 as 4 ft 1 in.
t3 <- long_compare(
  published_table3, read_out("table3_responders") |> select(all_of(names(published_table3))),
  "responder_caste", "Table 3", 0.1
)

t2 <- long_compare(
  published_table2, read_out("table2_ads") |> select(all_of(names(published_table2))),
  "groom_caste", "Table 2", 0.6
)

comparison <- bind_rows(
  t4 |> rename(row = groom),
  t5 |> rename(row = term),
  t5_fit |> rename(row = responder_caste),
  transmute(fn, table,
    row = paste(responder_caste, hypothesis), quantity = "F", published, reproduced, difference,
    match
  ),
  t6 |> rename(row = pair),
  t6_formula |> rename(row = pair),
  t3 |> rename(row = responder_caste),
  t2 |> rename(row = groom_caste)
) |>
  select(table, row, quantity, published, reproduced, difference, match)
write_table(comparison, "comparison_published")

summary_tbl <- comparison |>
  group_by(table) |>
  summarise(cells = n(), matched = sum(match), mismatched = sum(!match))
write_table(summary_tbl, "comparison_summary")
print(summary_tbl)
print(filter(comparison, !match), n = 50, width = 120)
