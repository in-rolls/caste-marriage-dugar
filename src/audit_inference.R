# Inference audit. Three questions:
#  1. Do the standard errors and F-tests survive treating the letter, not the
#     stacked row, as the unit? (cluster by letter; exact multinomial variance)
#  2. Are the letter shares stable across the two newspaper editions, the only
#     replication of the nine ads the design provides?
#  3. How uncertain are the Table 6 compensation figures, which the paper
#     reports without intervals? (delta method and letter-level bootstrap)
#  4. Does dropping the 243 repeat letters matter?

source("src/functions.R")
set.seed(20260914)

letters <- readRDS(paths$letters)
sample <- analysis_sample(letters)
stacked <- stack_letters(sample)
fits <- lapply(c(HC = "HC", MC = "MC", LC = "LC"), function(c) fit_lpm(stacked, c))

# 1a. Standard errors three ways. With only groom dummies, each LPM coefficient
# is the share difference p_j - p_ref among that caste's letters, so the exact
# multinomial variance is available without any regression.
share_vector <- function(data) {
  n <- nrow(data)
  p <- vapply(groom_levels, function(g) mean(data$groom == g), numeric(1))
  list(n = n, p = p)
}
multinomial_se_diff <- function(p, n, a, b) {
  pa <- if (a == "0") 0 else p[[a]]
  pb <- if (b == "0") 0 else p[[b]]
  sqrt((pa * (1 - pa) + pb * (1 - pb) + 2 * pa * pb) / n)
}
se_table <- bind_rows(lapply(names(fits), function(caste) {
  f <- fits[[caste]]
  s <- share_vector(filter(sample, responder_caste == caste))
  terms <- setdiff(groom_levels, groom_reference)
  tibble(
    responder_caste = caste,
    term = terms,
    estimate = unname(coef(f$fit)[coef_name(terms)]),
    se_hc1 = unname(sqrt(diag(f$vcov_hc1))[coef_name(terms)]),
    se_cluster_letter = unname(sqrt(diag(f$vcov_cluster))[coef_name(terms)]),
    se_multinomial = vapply(terms, function(t) multinomial_se_diff(s$p, s$n, t, groom_reference), numeric(1))
  )
})) |>
  mutate(ratio_cluster_to_hc1 = se_cluster_letter / se_hc1)
write_table(se_table, "audit_se_three_ways")

# 1b. Every footnote test with the three covariance matrices. The multinomial
# route tests the share difference directly (chi-square with 1 df).
tests <- read.csv(file.path(paths$output, "table5_footnote_tests.csv")) |>
  separate(hypothesis, into = c("a", "b"), sep = " = ", remove = FALSE)
test_all <- bind_rows(lapply(seq_len(nrow(tests)), function(i) {
  caste <- tests$responder_caste[i]
  f <- fits[[caste]]
  s <- share_vector(filter(sample, responder_caste == caste))
  a <- tests$a[i]
  b <- tests$b[i]
  hc1 <- test_equal(f$fit, f$vcov_hc1, a, b)
  cl <- test_equal(f$fit, f$vcov_cluster, a, b)
  diff <- s$p[[a]] - s$p[[b]]
  se <- multinomial_se_diff(s$p, s$n, a, b)
  tibble(
    responder_caste = caste, hypothesis = tests$hypothesis[i],
    f_hc1 = hc1$f, p_hc1 = hc1$p, f_cluster = cl$f, p_cluster = cl$p,
    z_multinomial = diff / se, p_multinomial = 2 * pnorm(-abs(diff / se))
  )
}))
write_table(test_all, "audit_footnote_tests_three_ways")

# 2. Edition stability. For each responder caste, a 9 x 2 table of groom by
# edition; chi-square test of independence, plus the shares themselves.
edition_shares <- sample |>
  count(responder_caste, edition, groom) |>
  group_by(responder_caste, edition) |>
  mutate(share = n / sum(n), letters = sum(n)) |>
  ungroup() |>
  mutate(groom = factor(groom, levels = groom_levels)) |>
  arrange(responder_caste, groom, edition)
write_table(edition_shares, "audit_edition_shares")
edition_tests <- bind_rows(lapply(c("HC", "MC", "LC"), function(caste) {
  tab <- with(filter(sample, responder_caste == caste), table(groom, edition))
  ct <- suppressWarnings(chisq.test(tab))
  ft <- fisher.test(tab, simulate.p.value = TRUE, B = 20000)
  tibble(
    responder_caste = caste, letters_edition1 = sum(tab[, 1]), letters_edition2 = sum(tab[, 2]),
    chisq = unname(ct$statistic), df = unname(ct$parameter), p_chisq = ct$p.value, p_fisher_mc = ft$p.value
  )
}))
write_table(edition_tests, "audit_edition_tests")
# Table 6 by edition: the same compensation computed within each placement.
fits_by_edition <- lapply(1:2, function(e) {
  st <- stack_letters(filter(sample, edition == e))
  lapply(c(HC = "HC", MC = "MC"), function(c) fit_lpm(st, c))
})
table6_by_edition <- bind_rows(lapply(1:2, function(e) {
  compensation_table(fits_by_edition[[e]]) |> mutate(edition = e)
})) |>
  pivot_wider(names_from = edition, values_from = compensation_k, names_prefix = "edition_")
write_table(table6_by_edition, "audit_table6_by_edition")

# 3. Uncertainty for Table 6. Delta method on the multinomial shares, and a
# bootstrap that resamples letters within responder caste (the sampling unit).
delta_ci <- function(p, n, responder_caste, groom_caste, income_level) {
  own <- paste0(responder_caste, "G-", income_level)
  other <- paste0(groom_caste, "G-", income_level)
  hi <- paste0(groom_caste, "G-HI")
  lo <- paste0(groom_caste, "G-LI")
  sigma <- (diag(p) - tcrossprod(p)) / n
  g <- function(q) (q[[own]] - q[[other]]) / ((q[[hi]] - q[[lo]]) / 28)
  grad <- central_gradient(g, p)
  est <- g(p)
  se <- sqrt(as.numeric(t(grad) %*% sigma %*% grad))
  c(estimate = est, se = se, lower = est - 1.96 * se, upper = est + 1.96 * se)
}
central_gradient <- function(f, x, h = 1e-6) {
  vapply(seq_along(x), function(i) {
    xp <- x
    xm <- x
    xp[i] <- xp[i] + h
    xm[i] <- xm[i] - h
    (f(xp) - f(xm)) / (2 * h)
  }, numeric(1))
}
boot_b <- 2000
boot_compensation <- function(data, responder_caste, groom_caste, income_level, b = boot_b) {
  n <- nrow(data)
  replicate(b, {
    draw <- data$groom[sample.int(n, n, replace = TRUE)]
    p <- vapply(groom_levels, function(g) mean(draw == g), numeric(1))
    compensation_from_shares(p, responder_caste, groom_caste, income_level)
  })
}
table6_uncertainty <- bind_rows(lapply(seq_len(nrow(table6_cells)), function(i) {
  rc <- table6_cells$responder_caste[i]
  gc <- table6_cells$groom_caste[i]
  il <- table6_cells$income_level[i]
  data <- filter(sample, responder_caste == rc)
  s <- share_vector(data)
  d <- delta_ci(s$p, s$n, rc, gc, il)
  bs <- boot_compensation(data, rc, gc, il)
  tibble(
    responder_caste = rc, groom_caste = gc, income_level = il,
    estimate = d[["estimate"]], se_delta = d[["se"]], lower_delta = d[["lower"]], upper_delta = d[["upper"]],
    boot_median = median(bs), lower_boot = quantile(bs, 0.025), upper_boot = quantile(bs, 0.975),
    boot_share_above_100k = mean(bs > 100)
  )
}))
write_table(table6_uncertainty, "audit_table6_uncertainty")

# Contrasts the text draws from Table 6: LC grooms need more than MC grooms at
# every income (HC responders), and compensation falls as income rises.
boot_contrasts <- {
  data <- filter(sample, responder_caste == "HC")
  n <- nrow(data)
  draws <- replicate(boot_b, {
    draw <- data$groom[sample.int(n, n, replace = TRUE)]
    p <- vapply(groom_levels, function(g) mean(draw == g), numeric(1))
    c(
      lc_minus_mc_hi = compensation_from_shares(p, "HC", "LC", "HI") - compensation_from_shares(p, "HC", "MC", "HI"),
      lc_minus_mc_mi = compensation_from_shares(p, "HC", "LC", "MI") - compensation_from_shares(p, "HC", "MC", "MI"),
      lc_minus_mc_li = compensation_from_shares(p, "HC", "LC", "LI") - compensation_from_shares(p, "HC", "MC", "LI"),
      lc_li_minus_hi = compensation_from_shares(p, "HC", "LC", "LI") - compensation_from_shares(p, "HC", "LC", "HI"),
      mc_li_minus_hi = compensation_from_shares(p, "HC", "MC", "LI") - compensation_from_shares(p, "HC", "MC", "HI")
    )
  })
  tibble(
    contrast = rownames(draws),
    estimate = apply(draws, 1, function(x) x[1] * 0 + NA_real_),
    boot_median = apply(draws, 1, median),
    lower_boot = apply(draws, 1, quantile, 0.025),
    upper_boot = apply(draws, 1, quantile, 0.975),
    share_positive = apply(draws, 1, function(x) mean(x > 0))
  )
}
p_hc <- share_vector(filter(sample, responder_caste == "HC"))$p
boot_contrasts$estimate <- c(
  compensation_from_shares(p_hc, "HC", "LC", "HI") - compensation_from_shares(p_hc, "HC", "MC", "HI"),
  compensation_from_shares(p_hc, "HC", "LC", "MI") - compensation_from_shares(p_hc, "HC", "MC", "MI"),
  compensation_from_shares(p_hc, "HC", "LC", "LI") - compensation_from_shares(p_hc, "HC", "MC", "LI"),
  compensation_from_shares(p_hc, "HC", "LC", "LI") - compensation_from_shares(p_hc, "HC", "LC", "HI"),
  compensation_from_shares(p_hc, "HC", "MC", "LI") - compensation_from_shares(p_hc, "HC", "MC", "HI")
)
write_table(boot_contrasts, "audit_table6_contrasts")

# 4. Repeat letters. The file flags 243 letters; the text says 47 non-unique
# letters from 22 responders were dropped. Recompute Tables 5 and 6 with all 1,366.
stacked_all <- stack_letters(letters)
fits_all <- lapply(c(HC = "HC", MC = "MC"), function(c) fit_lpm(stacked_all, c))
table6_all <- compensation_table(fits_all) |>
  rename(compensation_all_letters = compensation_k) |>
  left_join(compensation_table(fits) |> rename(compensation_paper_sample = compensation_k),
    by = c("responder_caste", "groom_caste", "income_level")
  )
write_table(table6_all, "audit_table6_with_repeat_letters")
repeat_flow <- letters |>
  count(responder_caste, repeat_letter) |>
  pivot_wider(names_from = repeat_letter, values_from = n, names_prefix = "repeat_")
write_table(repeat_flow, "audit_sample_flow")

print(se_table, n = 30)
print(test_all, n = 30, width = 150)
print(edition_tests)
print(table6_by_edition)
print(table6_uncertainty, width = 150)
print(boot_contrasts)
print(table6_all)
print(repeat_flow)
