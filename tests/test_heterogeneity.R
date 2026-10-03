setwd(Sys.getenv("PROJECT_ROOT", unset = ".."))
source("src/functions.R")
source("src/heterogeneity_functions.R")
letters <- readRDS(paths$letters)
retained <- analysis_sample(letters)
model <- fit_attribute_index(retained)
assigned <- assign_attribute_groups(retained, model)
counts <- read.csv("output/heterogeneity_counts.csv")

test_that("the extension preserves the original PCA and does not split ties", {
  original <- read.csv("output/quality_pca_loadings.csv")
  expect_equal(model$fit$rotation[, 1], setNames(original$pc1, original$attribute))
  expect_equal(length(unique(assigned$letter_id)), nrow(retained))
  expect_equal(nrow(assigned), nrow(retained))
  expect_false(anyNA(assigned$score))
  ties <- assigned |>
    group_by(responder_caste, score) |>
    summarise(groups = n_distinct(attribute_group))
  expect_true(all(ties$groups == 1))
  reverse <- assign_attribute_groups(retained[rev(seq_len(nrow(retained))), ], model)
  expect_equal(reverse$attribute_group[rev(seq_len(nrow(reverse)))], assigned$attribute_group)
  for (caste in names(model$cutoffs)) {
    expect_equal(
      model$cutoffs[[caste]],
      as.numeric(quantile(assigned$score[assigned$responder_caste == caste], c(1 / 3, 2 / 3)))
    )
  }
})

test_that("subgroup counts recover the original counts and every denominator", {
  main <- filter(counts, specification == "retained")
  combined <- main |>
    group_by(responder_caste, groom_caste, groom_income) |>
    summarise(letters = sum(letters), .groups = "drop")
  actual <- retained |> count(responder_caste, groom_caste, groom_income, name = "letters")
  expect_equal(as.data.frame(combined), as.data.frame(actual))
  expect_equal(sum(main$letters), nrow(retained))
  sums <- main |>
    group_by(responder_caste, attribute_group) |>
    summarise(share = sum(share))
  expect_equal(sums$share, rep(1, nrow(sums)))
  expect_equal(main$share * main$group_letters, as.numeric(main$letters))
  expect_equal(main$share_at_income * main$income_letters, as.numeric(main$letters))
  editions <- filter(counts, specification %in% c("edition_1", "edition_2")) |>
    group_by(responder_caste, attribute_group, groom_income, groom_caste) |>
    summarise(letters = sum(letters), .groups = "drop")
  expect_equal(editions$letters, main$letters)
  all <- filter(counts, specification == "all_recorded")
  excluded <- filter(counts, specification == "excluded")
  expect_equal(all$letters, main$letters + excluded$letters)
  expect_equal(
    sum(filter(counts, specification == "complete_case")$letters),
    sum(complete.cases(quality_attributes(retained)))
  )
})

test_that("education groups retain missing values and use the documented codes", {
  groups <- education_groups(retained)
  expect_equal(sum(groups$attribute_group == "Missing education"), sum(is.na(retained$education)))
  expect_equal(
    sum(groups$attribute_group == "Master's or doctorate"),
    sum(as.numeric(retained$education) >= 3, na.rm = TRUE)
  )
  expect_equal(sum(filter(counts, specification == "education")$letters), nrow(retained))
  x <- quality_attributes(retained)
  expect_equal(sum(is.na(x$own_residence)), 408L)
  expect_equal(sum(is.na(x$own_car)), 408L)
  expect_equal(names(fit_attribute_index(retained, TRUE)$medians), setdiff(names(x), "education"))
})

test_that("crossings distinguish none, endpoints, multiple roots and equality intervals", {
  y <- c(7000, 15000, 35000)
  expect_equal(nrow(response_crossings(y, c(-3, -2, -1))), 0L)
  expect_equal(response_crossings(y, c(-2, -1, 1))$lower_income, 25000)
  expect_equal(response_crossings(y, c(-1, 1, -1))$lower_income, c(11000, 25000))
  expect_equal(response_crossings(y, c(-1, 0, 1))$lower_income, 15000)
  flat <- response_crossings(y, c(0, 0, 1))
  expect_equal(flat$lower_income, 7000)
  expect_equal(flat$upper_income, 15000)
  flat_all <- response_crossings(y, c(0, 0, 0))
  expect_equal(flat_all$lower_income, 7000)
  expect_equal(flat_all$upper_income, 35000)
  expect_error(response_crossings(y, c(NA, 0, 1)))
  expect_error(response_crossings(rev(y), c(1, 2, 3)))
})

test_that("bootstrap re-estimation is reproducible and keeps no-crossing draws", {
  a <- bootstrap_heterogeneity(retained, draws = 3, seed = 17)
  b <- bootstrap_heterogeneity(retained, draws = 3, seed = 17)
  expect_equal(a, b)
  expect_equal(nrow(a$gaps), 27L)
  expect_equal(n_distinct(a$crossings$draw), 3L)
  freq <- read.csv("output/heterogeneity_crossing_frequency.csv")
  expect_equal(freq$draws, rep(2000L, 3))
  expect_equal(freq$no_crossing + freq$one_crossing + freq$multiple_crossings, freq$draws)
  draws <- read.csv("output/heterogeneity_bootstrap_gaps.csv")
  intervals <- read.csv("output/heterogeneity_intervals.csv")
  for (i in seq_len(nrow(intervals))) {
    row <- intervals[i, ]
    x <- subset(draws, attribute_group == row$attribute_group & groom_income == row$groom_income)$difference
    expect_equal(as.numeric(quantile(x, c(0.025, 0.975))), c(row$lower, row$upper))
  }
})

test_that("subgroup tests compare groups directly and correct the declared family", {
  tests <- read.csv("output/heterogeneity_tests.csv")
  gaps <- brahmin_gaps(filter(counts, specification == "retained"))
  for (i in seq_len(nrow(tests))) {
    x <- filter(gaps, groom_income == tests$groom_income[i])
    expected <- fisher.test(as.matrix(select(x, LC, HC)))$p.value
    expect_equal(tests$p[i], expected)
  }
  expect_equal(tests$p_holm, p.adjust(tests$p, "holm"))
})

test_that("the manuscript's ranking claims match every reported specification", {
  for (spec in c(
    "retained", "edition_1", "edition_2", "all_recorded", "excluded",
    "no_education", "complete_case"
  )) {
    gap <- brahmin_gaps(filter(counts, specification == spec))
    high <- filter(gap, groom_income == 35000)
    expect_gt(high$LC[high$attribute_group == "T1"], high$HC[high$attribute_group == "T1"])
    expect_lt(high$LC[high$attribute_group == "T3"], high$HC[high$attribute_group == "T3"])
  }
  main <- brahmin_gaps(filter(counts, specification == "retained"))
  expect_true(all(filter(main, attribute_group != "T1")$difference < 0))
  bottom <- filter(main, attribute_group == "T1")
  roots <- response_crossings(bottom$groom_income, bottom$difference)
  # Re-derive the interpolation directly from the raw count differences.
  delta <- bottom$LC - bottom$HC
  crossing <- 15000 - delta[2] * 20000 / (delta[3] - delta[2])
  expect_equal(roots$lower_income, crossing)
  expect_equal(read.csv("output/heterogeneity_crossings.csv")$lower_income[1], crossing)
})
