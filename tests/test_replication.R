setwd(Sys.getenv("PROJECT_ROOT", unset = ".."))
source("src/functions.R")
source("src/published_values.R")

letters <- readRDS(paths$letters)
sample <- analysis_sample(letters)

test_that("letters are conserved from the stacked file", {
  expect_equal(nrow(letters), 1366)
  expect_equal(nrow(stack_letters(letters)), 12294)
  expect_equal(nrow(sample), 1123)
  expect_equal(as.vector(table(sample$responder_caste)[c("HC", "MC", "LC")]), c(478, 374, 271))
})

test_that("Table 4 counts match the paper", {
  counts <- sample |>
    count(groom, responder_caste) |>
    pivot_wider(names_from = responder_caste, values_from = n) |>
    arrange(match(groom, groom_levels))
  expect_equal(counts$HC, published_table4$HC)
  expect_equal(counts$MC, published_table4$MC)
  expect_equal(counts$LC, published_table4$LC)
})

fits <- lapply(c(HC = "HC", MC = "MC", LC = "LC"), function(c) fit_lpm(stack_letters(sample), c))

test_that("Table 5 coefficients and HC1 standard errors match to four decimals", {
  for (caste in c("HC", "MC")) {
    b <- coef(fits[[caste]]$fit)
    se <- sqrt(diag(fits[[caste]]$vcov_hc1))
    terms <- c(paste0("groom", published_table5$term[1:8]), "(Intercept)")
    expect_equal(round(unname(b[terms]), 4), published_table5[[paste0(caste, "_est")]], tolerance = 1e-8)
    expect_equal(round(unname(se[terms]), 4), published_table5[[paste0(caste, "_se")]], tolerance = 1e-8)
    expect_equal(nobs(fits[[caste]]$fit), unname(published_table5_n[caste]))
  }
})

test_that("Table 6 HC panels match and the MC panel follows the stated formula", {
  t6 <- compensation_table(fits)
  expect_equal(t6$compensation_k[1:6], c(27.40, 34.87, 40.47, 35.95, 36.56, 49.33), tolerance = 0.002)
  expect_equal(t6$compensation_k[7:9], c(22.56, 13.54, 1.79), tolerance = 0.002)
})

test_that("height conversion reads feet.inches", {
  expect_equal(height_to_inches(c(5.3, 4.11, 5, 6)), c(63, 59, 60, 72))
})

test_that("out-of-caste percentages use letters received as their denominator", {
  received <- read.csv("output/headline_letters_received.csv")
  for (groom in c("HC", "MC", "LC", "All")) {
    data <- if (groom == "All") sample else filter(sample, groom_caste == .env$groom)
    row <- filter(received, groom_caste == .env$groom)
    expect_equal(row$total, nrow(data))
    expect_equal(row$out_of_caste, sum(data$responder_caste != data$groom_caste))
    expect_equal(row$out_of_caste_share, mean(data$responder_caste != data$groom_caste))
    expect_equal(row$own + row$out_of_caste, row$total)
  }
  summary <- read.csv("output/headline_cross_caste_shares.csv")
  expect_equal(summary$up_letters + summary$down_letters, summary$cross_caste_letters)
  expect_equal(summary$own_caste_letters + summary$cross_caste_letters, summary$letters)
  expect_equal(summary$cross_caste, summary$cross_caste_letters / summary$letters)
  # The total is weighted by letters, not an unweighted average across groom castes.
  expect_equal(
    received$out_of_caste_share[received$groom_caste == "All"],
    weighted.mean(received$out_of_caste_share[1:3], received$total[1:3])
  )
})

test_that("the reported contact elasticity is the finite log-change in letter counts", {
  elasticity <- read.csv("output/headline_income_elasticities.csv") |>
    filter(responder_caste == "HC", groom_caste == "LC")
  high <- sum(sample$responder_caste == "HC" & sample$groom_caste == "LC" & sample$groom_income == 35000)
  middle <- sum(sample$responder_caste == "HC" & sample$groom_caste == "LC" & sample$groom_income == 15000)
  expect_equal(elasticity$arc_15k_to_35k, log(high / middle) / log(35000 / 15000))
})

test_that("destination shares preserve the respondent and income denominators", {
  destinations <- read.csv("output/headline_letter_destinations.csv")
  expect_equal(nrow(destinations), 36L)
  pooled <- filter(destinations, is.na(groom_income))
  expect_equal(pooled$letters, c(281L, 116L, 81L, 100L, 158L, 116L, 31L, 81L, 159L))
  expect_equal(pooled$denominator, rep(c(478L, 374L, 271L), each = 3))
  by_income <- filter(destinations, !is.na(groom_income))
  for (i in seq_len(nrow(by_income))) {
    row <- by_income[i, ]
    eligible <- sample$responder_caste == row$responder_caste & sample$groom_income == row$groom_income
    expect_equal(row$denominator, sum(eligible))
    expect_equal(row$letters, sum(eligible & sample$groom_caste == row$groom_caste))
  }
  totals <- destinations |>
    group_by(responder_caste, groom_income) |>
    summarise(share = sum(share), .groups = "drop")
  expect_equal(totals$share, rep(1, 12))
  summed <- by_income |>
    group_by(responder_caste, groom_caste) |>
    summarise(income_sum = sum(letters), .groups = "drop") |>
    left_join(pooled, by = c("responder_caste", "groom_caste"))
  expect_equal(summed$income_sum, summed$letters)
})
