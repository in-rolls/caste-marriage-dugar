# Functional-form audit of Table 6. The paper divides a share gap by one linear
# slope (LI to HI, per thousand rupees) and extrapolates. Here the same gap is
# closed under other assumptions about how shares respond to income, and the
# implied incomes are placed against the real advertisements.

source("src/functions.R")
suppressPackageStartupMessages(library(nnet))
set.seed(20260914)

letters <- readRDS(paths$letters)
ads <- readRDS(paths$ads_derived)
sample <- analysis_sample(letters)
stacked <- stack_letters(sample)
fits <- lapply(c(HC = "HC", MC = "MC"), function(c) fit_lpm(stacked, c))

# 1. Segment slopes. The share response to income is convex (HC letters to the
# LC groom: 5, 25, 51), so the per-thousand slope depends on the segment.
segments <- bind_rows(lapply(c("LI-HI", "LI-MI", "MI-HI"), function(s) {
  compensation_table(fits, slope = s) |> mutate(slope = s)
})) |>
  pivot_wider(names_from = slope, values_from = compensation_k)
write_table(segments, "audit_table6_segment_slopes")

# 2. Log-income and multinomial logit. Fit share ~ f(income) within the lower
# caste's three grooms and solve for the income at which the lower-caste share
# equals the own-caste share at the same income level.
shares <- sample |>
  count(responder_caste, groom, groom_caste, groom_income, income_level) |>
  group_by(responder_caste) |>
  mutate(share = n / sum(n)) |>
  ungroup()
solve_income <- function(target_share, groom_caste, rc, form) {
  s <- filter(shares, responder_caste == rc, groom_caste == !!groom_caste)
  # Linear in income, or linear in log income, through the three points (least squares).
  x <- if (form == "log") log(s$groom_income) else s$groom_income
  fit <- lm(s$share ~ x)
  b <- coef(fit)
  x_star <- (target_share - b[1]) / b[2]
  unname(if (form == "log") exp(x_star) else x_star)
}
alt_forms <- table6_cells |>
  rowwise() |>
  mutate(
    own_share = shares$share[shares$responder_caste == responder_caste &
                               shares$groom == paste0(responder_caste, "G-", income_level)],
    groom_income = groom_design$groom_income[groom_design$income_level == income_level][1],
    income_linear_ls = solve_income(own_share, groom_caste, responder_caste, "linear"),
    income_log = solve_income(own_share, groom_caste, responder_caste, "log"),
    compensation_linear_ls_k = (income_linear_ls - groom_income) / 1000,
    compensation_log_k = (income_log - groom_income) / 1000
  ) |>
  ungroup() |>
  left_join(compensation_table(fits) |> rename(compensation_paper_k = compensation_k),
    by = c("responder_caste", "groom_caste", "income_level")
  )
write_table(alt_forms, "audit_table6_alternative_forms")

# Multinomial logit at the letter level: choice among nine ads with caste dummies,
# income (thousand rupees) interacted with caste. The compensation is the income
# change that equalises the two alternatives' utilities, which in a logit is the
# caste gap divided by the income coefficient; no extrapolation to the reference share.
mlogit_fit <- function(rc) {
  data <- filter(sample, responder_caste == rc) |>
    mutate(groom = factor(groom, levels = groom_levels))
  # nnet::multinom fits alternative-specific intercepts; with nine unordered
  # alternatives and no letter-level covariates it is the saturated share model.
  # A conditional-logit parameterisation with caste dummies and income slopes is
  # fitted directly by maximum likelihood.
  counts <- table(data$groom)
  y <- as.numeric(counts[groom_levels])
  caste <- groom_design$groom_caste
  x_caste <- cbind(MC = as.numeric(caste == "MC"), LC = as.numeric(caste == "LC"))
  inc <- groom_design$groom_income / 1000
  negll <- function(theta) {
    # theta: caste intercepts (MC, LC relative to HC), income slope per caste (HC, MC, LC)
    u <- x_caste %*% theta[1:2] + inc * theta[3:5][match(caste, c("HC", "MC", "LC"))]
    p <- exp(u) / sum(exp(u))
    -sum(y * log(p))
  }
  opt <- optim(rep(0, 5), negll, method = "BFGS", hessian = TRUE)
  theta <- opt$par
  names(theta) <- c("MC_vs_HC", "LC_vs_HC", "income_HC", "income_MC", "income_LC")
  vc <- solve(opt$hessian)
  u <- x_caste %*% theta[1:2] + inc * theta[3:5][match(caste, c("HC", "MC", "LC"))]
  fitted <- tibble(groom = groom_levels, observed_share = y / sum(y), fitted_share = as.numeric(exp(u) / sum(exp(u))))
  list(theta = theta, vcov = vc, negll = opt$value, saturated_negll = -sum(y * log(y / sum(y))), fitted = fitted)
}
logit_compensation <- function(m, groom_caste, rc, income_level) {
  th <- m$theta
  inc <- groom_design$groom_income[groom_design$income_level == income_level][1] / 1000
  own_intercept <- if (rc == "HC") 0 else th[["MC_vs_HC"]]
  other_intercept <- th[[paste0(groom_caste, "_vs_HC")]]
  own_utility <- own_intercept + th[[paste0("income_", rc)]] * inc
  other_utility <- other_intercept + th[[paste0("income_", groom_caste)]] * inc
  gap <- own_utility - other_utility
  gap / th[[paste0("income_", groom_caste)]]
}
mlogits <- lapply(c(HC = "HC", MC = "MC"), mlogit_fit)
logit_params <- bind_rows(lapply(names(mlogits), function(rc) {
  m <- mlogits[[rc]]
  tibble(
    responder_caste = rc, parameter = names(m$theta), estimate = m$theta, se = sqrt(diag(m$vcov)),
    deviance_vs_saturated = 2 * (m$negll - m$saturated_negll)
  )
}))
write_table(logit_params, "audit_logit_parameters")
logit_fitted <- bind_rows(lapply(names(mlogits), function(rc) {
  mutate(mlogits[[rc]]$fitted, responder_caste = rc, .before = 1)
}))
write_table(logit_fitted, "audit_logit_fitted_shares")
logit_table6 <- table6_cells |>
  rowwise() |>
  mutate(compensation_logit_k = logit_compensation(
    mlogits[[responder_caste]], groom_caste, responder_caste,
    income_level
  )) |>
  ungroup()
write_table(logit_table6, "audit_table6_logit")

# 3. Support. Where does the implied income sit in the real-ad distribution?
support <- compensation_table(fits) |>
  mutate(
    groom_income = groom_design$groom_income[match(income_level, groom_design$income_level)],
    implied_income = groom_income + 1000 * compensation_k,
    share_of_real_ads_below = vapply(implied_income, function(v) mean(ads$income <= v, na.rm = TRUE), numeric(1)),
    real_ads_at_or_above = vapply(implied_income, function(v) sum(ads$income >= v, na.rm = TRUE), numeric(1))
  )
income_quantiles <- tibble(
  quantile = c(0.5, 0.9, 0.95, 0.99, 1),
  real_ad_income = quantile(ads$income, c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE)
)
write_table(support, "audit_table6_support")
write_table(income_quantiles, "audit_real_ad_income_quantiles")

print(segments)
print(select(
  alt_forms, responder_caste, groom_caste, income_level, compensation_paper_k, compensation_linear_ls_k,
  compensation_log_k
), width = 150)
print(logit_fitted, n = 20)
print(logit_params, n = 20)
print(logit_table6)
print(support)
print(income_quantiles)
