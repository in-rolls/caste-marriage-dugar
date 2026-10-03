# Reproduce Tables 2 to 6 and the footnote hypothesis tests of the December 2010
# final draft from the two original files.

source("src/functions.R")

letters <- readRDS(paths$letters)
ads <- readRDS(paths$ads_derived)
sample <- analysis_sample(letters)
stacked <- stack_letters(sample)

# Table 2: real advertisements, by groom caste. Built twice: as the file stands,
# and excluding the non-integer (filled) ages and heights in the Brahmin sheet.
summarise_ads <- function(a) {
  a |>
    group_by(groom_caste) |>
    summarise(
      ads = n(),
      income_n = sum(!is.na(income)), income_mean = mean(income, na.rm = TRUE), income_sd = sd(income, na.rm = TRUE),
      age_n = sum(!is.na(age)), age_mean = mean(age, na.rm = TRUE), age_sd = sd(age, na.rm = TRUE),
      height_n = sum(!is.na(height_in)), height_mean = mean(height_in, na.rm = TRUE),
      height_sd = sd(height_in, na.rm = TRUE),
      govt_job = sum(govt_job), po_box = sum(po_box), caste_no_bar = sum(caste_no_bar_flag),
      income_mentioned = sum(!is.na(income)), looks_mentioned = sum(looks_mentioned %in% "Yes"),
      education_not_mentioned = sum(education_missing)
    )
}
table2 <- bind_rows(
  summarise_ads(mutate(ads, groom_caste = "Aggregate")),
  summarise_ads(ads)
) |>
  mutate(groom_caste = factor(groom_caste, levels = c("Aggregate", "HC", "MC", "LC"))) |>
  arrange(groom_caste)
table2_clean <- bind_rows(
  summarise_ads(mutate(ads, groom_caste = "Aggregate", age = ifelse(age_noninteger, NA, age))),
  summarise_ads(mutate(ads, age = ifelse(age_noninteger, NA, age)))
) |>
  mutate(groom_caste = factor(groom_caste, levels = c("Aggregate", "HC", "MC", "LC"))) |>
  arrange(groom_caste)
write_table(table2, "table2_ads")
write_table(table2_clean, "table2_ads_excluding_filled")

# Table 3: responder attributes.
table3 <- bind_rows(
  sample |> mutate(responder_caste = "Aggregate"),
  sample
) |>
  mutate(responder_caste = factor(responder_caste, levels = c("Aggregate", "HC", "MC", "LC"))) |>
  group_by(responder_caste) |>
  summarise(
    responses = n(),
    height_mean = mean(height_in, na.rm = TRUE), height_sd = sd(height_in, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE), age_sd = sd(age, na.rm = TRUE),
    girl_working = sum(girl_working == 1, na.rm = TRUE),
    education_below_bachelors = sum(education == "below_bachelors", na.rm = TRUE),
    education_bachelors = sum(education == "bachelors", na.rm = TRUE),
    education_above_bachelors = sum(education %in% c("masters", "phd")),
    fair = sum(complexion == "fair", na.rm = TRUE),
    medium_fair = sum(complexion == "medium_fair", na.rm = TRUE),
    very_fair = sum(complexion == "very_fair", na.rm = TRUE),
    fairly_good_looking = sum(looks == "fair", na.rm = TRUE),
    medium_good_looking = sum(looks == "good", na.rm = TRUE),
    very_good_looking = sum(looks == "very_good", na.rm = TRUE),
    siblings_mean = mean(sons + daughters, na.rm = TRUE), siblings_sd = sd(sons + daughters, na.rm = TRUE),
    unmarried_sisters_mean = mean(unmdaughters), unmarried_sisters_sd = sd(unmdaughters),
    father_absent = sum(father_absent == 1),
    own_house_or_apt = sum(own_house == 1 | own_apartment == 1, na.rm = TRUE)
  )
write_table(table3, "table3_responders")

# Table 4: letters by groom and responder caste.
table4 <- sample |>
  count(groom, groom_caste, income_level, responder_caste) |>
  pivot_wider(names_from = responder_caste, values_from = n, values_fill = 0) |>
  mutate(groom = factor(groom, levels = groom_levels)) |>
  arrange(groom) |>
  select(groom, groom_caste, income_level, HC = HC, MC = MC, LC = LC)
write_table(table4, "table4_counts")

# Table 5: LPM per responder caste with HC1 standard errors.
fits <- lapply(c(HC = "HC", MC = "MC", LC = "LC"), function(caste) fit_lpm(stacked, caste))
saveRDS(fits, file.path(paths$output, "lpm_fits.rds"))
tidy_fit <- function(f, caste) {
  b <- coef(f$fit)
  se <- sqrt(diag(f$vcov_hc1))
  tibble(
    responder_caste = caste,
    term = sub("^groom", "", names(b)),
    estimate = unname(b), se_hc1 = unname(se),
    p_hc1 = 2 * pt(-abs(b / se), df = df.residual(f$fit))
  )
}
table5 <- bind_rows(lapply(names(fits), function(c) tidy_fit(fits[[c]], c))) |>
  mutate(term = ifelse(term == "(Intercept)", "Constant", term))
table5_n <- tibble(
  responder_caste = names(fits),
  n = vapply(fits, function(f) nobs(f$fit), numeric(1)),
  r2 = vapply(fits, function(f) summary(f$fit)$r.squared, numeric(1))
)
write_table(table5, "table5_lpm")
write_table(table5_n, "table5_lpm_fit")

# Footnote tests (fn 15, 16, 19, 20) with the paper's HC1 covariance.
tests <- list(
  HC = list(
    c("HCG-HI", "MCG-HI"), c("HCG-HI", "LCG-HI"), c("MCG-HI", "LCG-HI"),
    c("HCG-MI", "MCG-MI"), c("HCG-MI", "LCG-MI"), c("MCG-MI", "LCG-MI"),
    c("HCG-LI", "MCG-LI"), c("HCG-LI", "LCG-LI"), c("MCG-LI", "LCG-LI"),
    c("MCG-HI", "MCG-MI"), c("MCG-MI", "MCG-LI"), c("MCG-HI", "MCG-LI"),
    c("LCG-HI", "LCG-MI"), c("LCG-HI", "LCG-LI"), c("LCG-MI", "LCG-LI")
  ),
  MC = list(
    c("MCG-HI", "LCG-HI"), c("MCG-MI", "LCG-MI"), c("MCG-LI", "LCG-LI"),
    c("LCG-HI", "LCG-MI"), c("LCG-HI", "LCG-LI"), c("LCG-MI", "LCG-LI")
  )
)
footnote_tests <- bind_rows(lapply(names(tests), function(caste) {
  bind_rows(lapply(tests[[caste]], function(h) {
    test_equal(fits[[caste]]$fit, fits[[caste]]$vcov_hc1, h[1], h[2])
  })) |> mutate(responder_caste = caste, .before = 1)
}))
write_table(footnote_tests, "table5_footnote_tests")

# Table 6: compensation in thousand rupees per month.
table6 <- compensation_wide(compensation_table(fits, slope = "LI-HI"))
write_table(table6, "table6_compensation")
write_table(compensation_wide(worksheet_compensation(fits)), "table6_worksheet")
write_table(compensation_wide(worksheet_compensation(fits, FALSE)), "table6_rounded_unrestricted")

worksheet <- read.csv(paths$table6_worksheet)
write_table(worksheet, "table6_worksheet_source")

figure_data <- paper_figure_data(sample)
for (name in names(figure_data)) write_table(figure_data[[name]], paste0("paper_", name))

print(table4)
print(table5, n = 30)
print(table5_n)
print(footnote_tests, n = 30)
print(table6)
