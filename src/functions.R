# Shared helpers for the Dugar, Bhattacharya and Reiley (2012) reproduction.
# Every number that appears in both the replication and the audit is computed
# here once, so the two cannot drift apart.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(haven)
  library(readxl)
  library(sandwich)
  library(car)
})

paths <- list(
  lpm = "data/original/lpmdata.dta",
  ads = "data/original/Marriage Ads from newspaper issues.xls",
  table6_worksheet = "sources/table6_worksheet_values.csv",
  letters = "data/derived/letters.rds",
  letters_csv = "data/derived/letters.csv",
  ads_derived = "data/derived/ads.rds",
  output = "output",
  figs = "figs"
)

# Conditional on the pair total, independent letter destinations are binomial.
# Transform its Clopper-Pearson interval to a ratio of expected letter counts.
contact_count_ratio <- function(other, own, conf_level = 0.95) {
  counts <- c(other, own)
  stopifnot(
    length(other) == 1L, length(own) == 1L,
    all(is.finite(counts)), all(counts >= 0), all(counts == floor(counts))
  )
  if (sum(counts) == 0) {
    return(c(ratio = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  interval <- stats::binom.test(counts, conf.level = conf_level)$conf.int
  c(
    ratio = other / own, lower = interval[1] / (1 - interval[1]),
    upper = interval[2] / (1 - interval[2])
  )
}

# Groom design: nine fictitious ads, three castes by three monthly incomes.
groom_design <- tibble(
  groomid = 1:9,
  groom_caste = rep(c("HC", "MC", "LC"), each = 3),
  groom_income = rep(c(35000L, 15000L, 7000L), times = 3),
  income_level = rep(c("HI", "MI", "LI"), times = 3)
) |>
  mutate(groom = paste0(groom_caste, "G-", income_level))

groom_levels <- groom_design$groom
groom_reference <- "LCG-LI"

responder_caste_labels <- c(h = "HC", m = "MC", l = "LC")

# The responder covariates that jointly identify a letter. The Stata file has no
# letter id; nine rows share one letter iff they share all of these.
letter_keys <- c(
  "setno", "respondercaste", "height", "age", "educan", "educid", "sons",
  "daughters", "unmdaughters", "foccupn", "moccupn", "goccupation",
  "complexion", "looks", "bridesincomepm", "repeat", "familywealth",
  "ownhouse", "ownapt", "ownland", "owncar"
)

# Heights are stored as feet.inches (5.3 means 5 ft 3 in). 4.11 is 4 ft 11 in,
# so 5 ft 10 in and 5 ft 1 in would both print as 5.1; the paper's Table 3 mean
# (63.2 in) is only reproduced by reading the first decimal as inches.
height_to_inches <- function(x) {
  feet <- floor(x + 1e-6)
  hundredths <- round((x - feet) * 100)
  inches <- ifelse(hundredths %% 10 == 0, hundredths / 10, hundredths)
  feet * 12 + inches
}

read_lpm <- function(path = paths$lpm) {
  read_dta(path) |>
    mutate(across(where(is.character), ~ trimws(.x)))
}

# Collapse the stacked file to one row per letter and check the structure the
# paper describes: nine rows per letter, exactly one of them a response.
build_letters <- function(lpm) {
  stacked <- lpm |>
    mutate(row_id = row_number()) |>
    group_by(across(all_of(letter_keys))) |>
    mutate(letter_id = cur_group_id(), rows_in_letter = n(), responses_in_letter = sum(response01)) |>
    ungroup()
  bad <- stacked |>
    filter(rows_in_letter != 9 | responses_in_letter != 1) |>
    distinct(letter_id, rows_in_letter, responses_in_letter)
  if (nrow(bad) > 0) {
    message(nrow(bad), " covariate profiles are shared by more than one letter; splitting them by response row")
  }
  # Profiles shared by k letters have 9k rows and k responses. Split them: each
  # response row anchors one letter, and non-response rows for the other grooms
  # are identical across the k letters, so any assignment is equivalent.
  letters <- stacked |>
    filter(response01 == 1) |>
    mutate(letter_id = row_number()) |>
    left_join(groom_design, by = "groomid") |>
    transmute(
      letter_id,
      duplicate_profile = rows_in_letter > 9,
      edition = as.integer(setno),
      responder_caste = responder_caste_labels[respondercaste],
      repeat_letter = `repeat` == "y",
      groomid, groom, groom_caste, groom_income, income_level,
      height_in = height_to_inches(height),
      age,
      education = factor(educid,
        levels = 1:4,
        labels = c("below_bachelors", "bachelors", "masters", "phd")
      ),
      education_raw = educan,
      sons, daughters, unmdaughters,
      girl_working = gworking,
      father_occupation = foccupn, father_absent = fabsent,
      father_govt = fgovjob, father_private = fpvtjob, father_business = fbusines, father_retired = fretired,
      mother_occupation = moccupn, mother_absent = mabsent, mother_housewife = mhousewife,
      complexion_raw = complexion,
      complexion = factor(complexionid, levels = 1:3, labels = c("fair", "medium_fair", "very_fair")),
      looks_raw = looks,
      looks = factor(looksid, levels = 1:3, labels = c("fair", "good", "very_good")),
      bride_income_pm = bridesincomepm,
      family_wealth_raw = familywealth,
      own_house = ownhouse, own_apartment = ownapt, own_land = ownland, own_car = owncar
    )
  # Letters that share every reported covariate get one profile_id.
  letters <- letters |>
    group_by(across(c(
      edition, responder_caste, repeat_letter, height_in, age, education_raw, sons, daughters,
      unmdaughters, father_occupation, mother_occupation, complexion_raw, looks_raw,
      bride_income_pm, family_wealth_raw, own_house, own_apartment, own_land, own_car
    ))) |>
    mutate(profile_id = cur_group_id()) |>
    ungroup()
  stopifnot(nrow(letters) == nrow(lpm) / 9, sum(lpm$response01) == nrow(letters))
  letters
}

# Expand letters back to the nine-row-per-letter form the paper's regressions use.
stack_letters <- function(letters) {
  letters |>
    select(letter_id, edition, responder_caste, repeat_letter, chosen_groomid = groomid) |>
    crossing(groom_design) |>
    mutate(
      response = as.integer(groomid == chosen_groomid),
      groom = factor(groom, levels = c(groom_reference, setdiff(groom_levels, groom_reference)))
    ) |>
    arrange(letter_id, groomid)
}

analysis_sample <- function(letters) {
  filter(letters, !repeat_letter)
}

# Linear probability model of Table 5: response on eight groom dummies, LCG-LI omitted,
# one responder caste at a time. Returns the lm plus HC1 (Stata "robust") and
# letter-clustered covariance matrices.
fit_lpm <- function(stacked, caste) {
  data <- filter(stacked, responder_caste == caste)
  fit <- lm(response ~ groom, data = data)
  list(
    fit = fit,
    data = data,
    vcov_hc1 = vcovHC(fit, type = "HC1"),
    vcov_cluster = vcovCL(fit, cluster = data$letter_id, type = "HC1")
  )
}

coef_name <- function(groom) paste0("groom", groom)

# Response probability for a groom from the LPM: intercept plus its dummy.
lpm_probability <- function(fit, groom) {
  b <- coef(fit)
  if (groom == groom_reference) unname(b["(Intercept)"]) else unname(b["(Intercept)"] + b[coef_name(groom)])
}

# The paper's compensation (Table 6, footnote 21): the gap in response probability
# between the own-caste groom and the lower-caste groom at the same income, divided
# by the lower-caste groom's per-thousand-rupee slope. `slope` selects the income
# segment used for the denominator; the paper uses LI to HI.
compensation_from_shares <- function(p, responder_caste, groom_caste, income_level,
                                     slope = c("LI-HI", "LI-MI", "MI-HI")) {
  slope <- match.arg(slope)
  own <- paste0(responder_caste, "G-", income_level)
  other <- paste0(groom_caste, "G-", income_level)
  gap <- p[[own]] - p[[other]]
  seg <- strsplit(slope, "-")[[1]]
  p_seg <- vapply(seg, function(l) p[[paste0(groom_caste, "G-", l)]], numeric(1))
  inc <- vapply(seg, function(l) groom_design$groom_income[groom_design$income_level == l][1], numeric(1))
  per_thousand <- diff(p_seg) / (diff(inc) / 1000)
  unname(gap / per_thousand)
}

lpm_shares <- function(fit) {
  p <- vapply(groom_levels, function(g) lpm_probability(fit, g), numeric(1))
  names(p) <- groom_levels
  p
}

compensation <- function(fit, groom_caste, income_level, responder_caste, slope = "LI-HI") {
  compensation_from_shares(lpm_shares(fit), responder_caste, groom_caste, income_level, slope)
}

table6_cells <- tibble(
  responder_caste = c(rep("HC", 6), rep("MC", 3)),
  groom_caste = c(rep("MC", 3), rep("LC", 3), rep("LC", 3)),
  income_level = rep(c("HI", "MI", "LI"), 3)
)

compensation_table <- function(fits, slope = "LI-HI") {
  table6_cells |>
    rowwise() |>
    mutate(compensation_k = compensation(
      fits[[responder_caste]]$fit, groom_caste, income_level,
      responder_caste, slope
    )) |>
    ungroup()
}

# Reconstruct the worksheet from rounded fitted coefficients. Its two documented
# overrides are specific to MCR-LCG; they are not a general significance filter.
worksheet_compensation <- function(fits, apply_overrides = TRUE) {
  table6_cells |>
    rowwise() |>
    mutate(compensation_k = {
      b <- round(coef(fits[[responder_caste]]$fit), 4)
      p <- setNames(vapply(groom_levels, function(g) {
        unname(b["(Intercept)"] + if (g == groom_reference) 0 else b[coef_name(g)])
      }, numeric(1)), groom_levels)
      if (apply_overrides && responder_caste == "MC") {
        p["LCG-MI"] <- p["LCG-LI"]
        p["MCG-LI"] <- p["LCG-LI"]
      }
      compensation_from_shares(p, responder_caste, groom_caste, income_level)
    }) |>
    ungroup()
}

compensation_wide <- function(cells) {
  cells |>
    mutate(pair = paste0(responder_caste, "R-", groom_caste, "G")) |>
    select(pair, income_level, compensation_k) |>
    pivot_wider(names_from = income_level, values_from = compensation_k) |>
    rowwise() |>
    mutate(mean = mean(c(HI, MI, LI)), sd = sd(c(HI, MI, LI))) |>
    ungroup()
}

paper_figure_data <- function(sample) {
  counts <- sample |>
    count(responder_caste, groom_caste, income_level, name = "letters") |>
    complete(responder_caste, groom_caste, income_level, fill = list(letters = 0L))
  by_income <- counts |>
    group_by(responder_caste, income_level) |>
    mutate(denominator = sum(letters), proportion = letters / denominator) |>
    ungroup()
  pairs <- tibble(responder_caste = c("HC", "HC", "MC"), groom_caste = c("MC", "LC", "LC"))
  list(
    figure1 = filter(by_income, responder_caste == "HC"),
    figure2 = filter(by_income, responder_caste == "MC", groom_caste != "HC"),
    figure3 = counts |>
      inner_join(pairs, by = c("responder_caste", "groom_caste")) |>
      group_by(responder_caste, groom_caste) |>
      mutate(denominator = sum(letters), proportion = letters / denominator) |>
      ungroup()
  )
}

# Wald test of a linear restriction between two groom coefficients.
test_equal <- function(model, vcov, groom_a, groom_b) {
  restriction <- paste(coef_name(groom_a), "=", if (groom_b == groom_reference) "0" else coef_name(groom_b))
  res <- linearHypothesis(model, restriction, vcov. = vcov, test = "F")
  tibble(hypothesis = paste(groom_a, "=", groom_b), f = res$F[2], df2 = res$Res.Df[2], p = res$`Pr(>F)`[2])
}

write_table <- function(x, name) {
  readr_available <- requireNamespace("readr", quietly = TRUE)
  path_csv <- file.path(paths$output, paste0(name, ".csv"))
  if (readr_available) readr::write_csv(x, path_csv) else write.csv(x, path_csv, row.names = FALSE)
  invisible(x)
}
