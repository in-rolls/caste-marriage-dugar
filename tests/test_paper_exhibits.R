setwd(Sys.getenv("PROJECT_ROOT", unset = ".."))
source("src/functions.R")
source("src/published_values.R")

sample <- analysis_sample(readRDS(paths$letters))
fits <- lapply(c(HC = "HC", MC = "MC", LC = "LC"), function(c) fit_lpm(stack_letters(sample), c))

test_that("Table 5 includes all three columns, sample sizes and printed fit statistics", {
  for (caste in names(fits)) {
    f <- fits[[caste]]
    terms <- c(paste0("groom", published_table5$term[1:8]), "(Intercept)")
    b <- coef(f$fit)[terms]
    se <- sqrt(diag(f$vcov_hc1))[terms]
    expect_equal(round(unname(b), 4), published_table5[[paste0(caste, "_est")]])
    if (caste == "LC") {
      p <- 2 * pt(-abs(b / se), df = df.residual(f$fit))
      expect_equal(round(unname(p), 3), published_table5$LC_p)
    } else {
      expect_equal(round(unname(se), 4), published_table5[[paste0(caste, "_se")]])
    }
    expect_equal(nobs(f$fit), unname(published_table5_n[caste]))
    expect_equal(
      round(summary(f$fit)$r.squared, if (caste == "LC") 3 else 2),
      unname(published_table5_r2[caste])
    )
  }
})

test_that("Table 6 reconstruction matches both the worksheet and printed table", {
  result <- compensation_wide(worksheet_compensation(fits))
  source <- read.csv(paths$table6_worksheet)
  expect_equal(result$pair, source$pair)
  expect_equal(unname(as.matrix(result[-1])), unname(as.matrix(source[-1])),
    tolerance = 1e-10
  )
  expect_equal(as.matrix(round(result[-1], 2)), as.matrix(published_table6[-1]))
  notes <- read.csv("sources/table6_worksheet.csv")
  expect_match(notes$G22, "treat LCG_MI coeff as 0 because it is insig", fixed = TRUE)
  expect_match(notes$H22, "as probability diff is insig", fixed = TRUE)
  unrestricted <- worksheet_compensation(fits, FALSE)$compensation_k
  adjusted <- worksheet_compensation(fits)$compensation_k
  expect_equal(which(abs(unrestricted - adjusted) > 1e-10), c(8L, 9L))
  expect_equal(unrestricted[8:9], c((0.0508 - 0.0107) / (0.0829 / 28), 0.0053 / (0.0829 / 28)))
})

test_that("Figures 1 and 2 use all letters at each income, including omitted bars", {
  figures <- paper_figure_data(sample)
  f1 <- figures$figure1 |> arrange(match(income_level, c("HI", "MI", "LI")), groom_caste)
  expect_equal(f1$denominator, rep(c(227L, 139L, 112L), each = 3))
  expect_equal(f1$proportion, c(110, 51, 66, 85, 25, 29, 86, 5, 21) / f1$denominator)
  f2 <- figures$figure2 |> arrange(match(income_level, c("HI", "MI", "LI")), groom_caste)
  expect_equal(f2$denominator, rep(c(168L, 111L, 95L), each = 2))
  expect_equal(f2$proportion, c(58, 83, 31, 46, 27, 29) / f2$denominator)
  sums <- f2 |>
    group_by(income_level) |>
    summarise(total = sum(proportion))
  expect_true(all(sums$total < 1))
})

test_that("Figure 3 uses the cross-caste pair as denominator", {
  f3 <- paper_figure_data(sample)$figure3 |>
    arrange(responder_caste, groom_caste, match(income_level, c("LI", "MI", "HI")))
  expect_equal(f3$denominator, rep(c(81L, 116L, 116L), each = 3))
  expect_equal(f3$proportion, c(5, 25, 51, 21, 29, 66, 27, 31, 58) / f3$denominator)
  sums <- f3 |>
    group_by(responder_caste, groom_caste) |>
    summarise(total = sum(proportion), .groups = "drop")
  expect_equal(sums$total, rep(1, 3))
})

test_that("an empty response cell remains a zero bar", {
  reduced <- filter(sample, !(responder_caste == "HC" & groom == "LCG-LI"))
  f1 <- paper_figure_data(reduced)$figure1
  zero <- filter(f1, groom_caste == "LC", income_level == "LI")
  expect_equal(nrow(f1), 9L)
  expect_equal(zero$letters, 0L)
  expect_equal(zero$denominator, 107L)
  expect_equal(zero$proportion, 0)
})
