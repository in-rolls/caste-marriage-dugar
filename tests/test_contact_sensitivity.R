setwd(Sys.getenv("PROJECT_ROOT", unset = ".."))
source("src/functions.R")

test_that("count ratio intervals respect reversal and boundary cases", {
  forward <- contact_count_ratio(51, 110)
  reverse <- contact_count_ratio(110, 51)
  expect_equal(unname(forward), 1 / unname(reverse[c(1, 3, 2)]))
  probability_limits <- c(qbeta(0.025, 51, 111), qbeta(0.975, 52, 110))
  expect_equal(unname(forward[2:3]), probability_limits / (1 - probability_limits))
  expect_equal(contact_count_ratio(0, 10)[["lower"]], 0)
  expect_equal(contact_count_ratio(10, 0)[["upper"]], Inf)
  expect_true(all(is.na(contact_count_ratio(0, 0))))
  expect_error(contact_count_ratio(-1, 10))
  expect_error(contact_count_ratio(1.5, 10))
})

test_that("sensitivity results preserve counts and edition aggregation", {
  x <- read.csv("output/contact_consideration_thresholds.csv")
  original <- read.csv("output/table4_counts.csv")
  main <- subset(x, sample == "retained" & period == "pooled")
  for (i in seq_len(nrow(main))) {
    level <- c(`7000` = "LI", `15000` = "MI", `35000` = "HI")[[as.character(main$groom_income[i])]]
    expect_equal(main$n_own[i], original$HC[original$groom == paste0("HCG-", level)])
    expect_equal(main$n_lc[i], original$HC[original$groom == paste0("LCG-", level)])
  }
  for (s in unique(x$sample)) {
    for (income in unique(x$groom_income)) {
      group <- subset(x, sample == s & groom_income == income)
      expect_equal(group$n_own[group$period == "pooled"], sum(group$n_own[group$period != "pooled"]))
      expect_equal(group$n_lc[group$period == "pooled"], sum(group$n_lc[group$period != "pooled"]))
    }
  }
  curves <- read.csv("output/contact_consideration_curves.csv")
  expect_equal(curves$rate_ratio * curves$consideration_ratio, curves$count_ratio)
  expect_true(all(curves$rate_lower <= curves$rate_ratio & curves$rate_ratio <= curves$rate_upper))
})

test_that("the income contrast compares high with middle and excludes bachelor's profiles", {
  x <- read.csv("output/contact_income_tests.csv")
  main <- subset(x, sample == "retained" & period == "pooled")
  expect_equal(main$high_vs_mid_ratio, (51 / 110) / (25 / 85))
  expected <- fisher.test(matrix(c(51, 110, 25, 85), nrow = 2, byrow = TRUE))
  expect_equal(main$p_masters_only, expected$p.value)
  expect_equal(c(main$lower, main$upper), as.numeric(expected$conf.int))
  expect_equal(nrow(x), 6L)
})
