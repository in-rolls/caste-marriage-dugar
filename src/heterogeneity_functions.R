# Attribute construction matches the pre-existing responder-quality analysis.
quality_attributes <- function(data) {
  data |>
    transmute(
      height_in, age,
      education = as.numeric(education),
      complexion = as.numeric(complexion), looks = as.numeric(looks),
      girl_working = as.numeric(girl_working == 1), unmdaughters,
      father_absent = as.numeric(father_absent == 1),
      own_residence = as.numeric(own_house == 1 | own_apartment == 1),
      own_car = as.numeric(own_car == 1)
    )
}

fit_attribute_index <- function(data, omit_education = FALSE) {
  x <- quality_attributes(data)
  if (omit_education) x$education <- NULL
  medians <- vapply(x, median, numeric(1), na.rm = TRUE)
  for (j in seq_along(x)) x[[j]][is.na(x[[j]])] <- medians[j]
  stopifnot(all(is.finite(as.matrix(x))), all(vapply(x, sd, numeric(1)) > 0))
  fit <- prcomp(x, center = TRUE, scale. = TRUE)
  anchors <- intersect(c("education", "complexion", "looks", "own_residence"), names(x))
  orientation <- sign(sum(fit$rotation[anchors, 1]))
  stopifnot(orientation != 0)
  score <- orientation * fit$x[, 1]
  cutoffs <- lapply(split(score, data$responder_caste), quantile,
    probs = c(1 / 3, 2 / 3), type = 7, names = FALSE
  )
  list(fit = fit, medians = medians, orientation = orientation, cutoffs = cutoffs)
}

assign_attribute_groups <- function(data, model) {
  x <- quality_attributes(data)[names(model$medians)]
  for (j in seq_along(x)) x[[j]][is.na(x[[j]])] <- model$medians[j]
  score <- as.numeric(predict(model$fit, x)[, 1] * model$orientation)
  # Equality belongs to the lower group, so tied scores are never split.
  group <- vapply(seq_along(score), function(i) {
    paste0("T", 1L + sum(score[i] > model$cutoffs[[data$responder_caste[i]]]))
  }, character(1))
  mutate(data, score = score, attribute_group = group)
}

education_groups <- function(data) {
  mutate(data, attribute_group = case_when(
    is.na(education) ~ "Missing education",
    as.numeric(education) == 1 ~ "Below bachelor's",
    as.numeric(education) == 2 ~ "Bachelor's",
    TRUE ~ "Master's or doctorate"
  ))
}

subgroup_counts <- function(data) {
  data |>
    count(responder_caste, attribute_group, groom_income, groom_caste, name = "letters") |>
    complete(nesting(responder_caste, attribute_group),
      groom_income = c(7000L, 15000L, 35000L), groom_caste = c("HC", "MC", "LC"),
      fill = list(letters = 0L)
    ) |>
    group_by(responder_caste, attribute_group) |>
    mutate(group_letters = sum(letters), share = letters / group_letters) |>
    group_by(responder_caste, attribute_group, groom_income) |>
    mutate(income_letters = sum(letters), share_at_income = ifelse(income_letters > 0,
      letters / income_letters, NA_real_
    )) |>
    ungroup()
}

# All roots on the observed support; an identically zero segment is an interval.
response_crossings <- function(income, difference) {
  stopifnot(
    length(income) == length(difference), length(income) >= 2,
    all(is.finite(income)), all(is.finite(difference)), all(diff(income) > 0)
  )
  empty <- data.frame(lower_income = numeric(), upper_income = numeric())
  roots <- empty
  for (i in seq_len(length(income) - 1L)) {
    a <- difference[i]
    b <- difference[i + 1L]
    if (a == 0 && b == 0) {
      roots <- rbind(roots, data.frame(lower_income = income[i], upper_income = income[i + 1L]))
    } else if (a * b < 0) {
      root <- income[i] - a * (income[i + 1L] - income[i]) / (b - a)
      roots <- rbind(roots, data.frame(lower_income = root, upper_income = root))
    }
  }
  for (root in income[difference == 0]) {
    if (!any(roots$lower_income <= root & roots$upper_income >= root)) {
      roots <- rbind(roots, data.frame(lower_income = root, upper_income = root))
    }
  }
  roots <- roots[order(roots$lower_income), , drop = FALSE]
  merged <- empty
  for (i in seq_len(nrow(roots))) {
    last <- nrow(merged)
    if (last > 0 && roots$lower_income[i] <= merged$upper_income[last]) {
      merged$upper_income[last] <- max(merged$upper_income[last], roots$upper_income[i])
    } else {
      merged <- rbind(merged, roots[i, , drop = FALSE])
    }
  }
  merged
}

brahmin_gaps <- function(counts) {
  counts |>
    filter(responder_caste == "HC", groom_caste %in% c("HC", "LC")) |>
    select(attribute_group, groom_income, groom_caste, letters, group_letters) |>
    pivot_wider(names_from = groom_caste, values_from = letters, values_fill = 0) |>
    mutate(difference = (LC - HC) / group_letters) |>
    arrange(attribute_group, groom_income)
}

bootstrap_heterogeneity <- function(data, draws = 2000L, seed = 20261002L) {
  set.seed(seed)
  strata <- split(seq_len(nrow(data)), data$responder_caste)
  results <- vector("list", draws)
  roots <- vector("list", draws)
  for (b in seq_len(draws)) {
    index <- unlist(lapply(strata, function(i) sample(i, length(i), replace = TRUE)), use.names = FALSE)
    boot <- data[index, , drop = FALSE]
    model <- fit_attribute_index(boot)
    gap <- brahmin_gaps(subgroup_counts(assign_attribute_groups(boot, model)))
    results[[b]] <- mutate(gap, draw = b)
    roots[[b]] <- bind_rows(lapply(c("T1", "T2", "T3"), function(g) {
      x <- gap[gap$attribute_group == g, ]
      cross <- response_crossings(x$groom_income, x$difference)
      if (nrow(cross) == 0) cross <- data.frame(lower_income = NA_real_, upper_income = NA_real_)
      mutate(cross, draw = b, attribute_group = g)
    }))
  }
  list(gaps = bind_rows(results), crossings = bind_rows(roots))
}
