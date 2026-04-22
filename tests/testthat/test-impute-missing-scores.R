# ==============================================================================
# impute_missing_scores()
# ==============================================================================
test_that(
  "impute_missing_scores adds .imputed = FALSE when nothing missing",
  {
    # Use only models with complete targets
    scores <- scores_quantile[
      model %in% c(
        "EuroCOVIDhub-ensemble",
        "EuroCOVIDhub-baseline"
      )
    ]
    scores <- new_scores(
      scores, get_metrics.scores(scores_quantile)
    )
    result <- suppressMessages(impute_missing_scores(
      scores, strategy = impute_na_score()
    ))
    expect_true(".imputed" %in% names(result))
    expect_false(any(result$.imputed))
  }
)

test_that(
  "impute_missing_scores preserves scores class and metrics",
  {
    scores <- scores_quantile
    result <- suppressMessages(impute_missing_scores(
      scores, strategy = impute_na_score()
    ))
    expect_s3_class(result, "scores")
    expect_identical(
      get_metrics.scores(result),
      get_metrics.scores(scores)
    )
  }
)

test_that(".imputed is not in get_metrics.scores output", {
  scores <- scores_quantile
  result <- suppressMessages(impute_missing_scores(
    scores, strategy = impute_na_score()
  ))
  metrics <- get_metrics.scores(result)
  expect_false(".imputed" %in% metrics)
})

test_that(".imputed is not in get_forecast_unit output", {
  # Requires .imputed in get_protected_columns()
  # (added by parallel agent)
  skip_if_not(
    ".imputed" %in% get_protected_columns(),
    ".imputed not yet in get_protected_columns"
  )
  scores <- scores_quantile
  result <- suppressMessages(impute_missing_scores(
    scores, strategy = impute_na_score()
  ))
  fu <- get_forecast_unit(result)
  expect_false(".imputed" %in% fu)
})

# ==============================================================================
# Strategy factories with missing data
# ==============================================================================

# scores_quantile already has missing combinations:
# UMass-MechBayes has 128 of 256 targets,
# epiforecasts-EpiNow2 has 247 of 256.
# Use it directly for tests needing missing data.

test_that("impute_worst_score fills with max observed score", {
  skip_if_not(
    exists("build_missing_grid",
           where = asNamespace("scoringutils")),
    "build_missing_grid not yet available"
  )
  scores <- scores_quantile
  metrics <- get_metrics.scores(scores)
  result <- suppressMessages(impute_missing_scores(
    scores, strategy = impute_worst_score()
  ))
  # Imputed rows should exist

  imputed <- result[(.imputed)]
  if (nrow(imputed) > 0) {
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, "model")

    # Each imputed metric value should equal the max of
    # that metric within the same target combination
    for (m in metrics) {
      if (!(m %in% names(imputed)) ||
            !(m %in% names(scores))) next
      max_per_target <- scores[,
        .(..max = max(get(m), na.rm = TRUE)),
        by = target_cols
      ]
      merged <- merge(
        imputed, max_per_target,
        by = target_cols, all.x = TRUE
      )
      expect_true(
        all(
          merged[[m]] == merged[["..max"]] |
            is.na(merged[[m]])
        ),
        info = paste(
          "metric", m,
          "does not match per-target max"
        )
      )
    }
  }
})

test_that("impute_mean_score fills with mean observed score", {
  skip_if_not(
    exists("build_missing_grid",
           where = asNamespace("scoringutils")),
    "build_missing_grid not yet available"
  )
  scores <- scores_quantile
  metrics <- get_metrics.scores(scores)
  result <- suppressMessages(impute_missing_scores(
    scores, strategy = impute_mean_score()
  ))
  imputed <- result[(.imputed)]
  expect_gt(nrow(imputed), 0)

  fu <- get_forecast_unit(scores)
  target_cols <- setdiff(fu, "model")

  for (m in metrics) {
    if (!(m %in% names(imputed)) ||
          !(m %in% names(scores))) next
    mean_per_target <- scores[,
      .(..mean = mean(get(m), na.rm = TRUE)),
      by = target_cols
    ]
    merged <- merge(
      imputed, mean_per_target,
      by = target_cols, all.x = TRUE
    )
    expect_true(
      all(
        abs(merged[[m]] - merged[["..mean"]]) < 1e-10 |
          is.na(merged[[m]])
      ),
      info = paste(
        "metric", m,
        "does not match per-target mean"
      )
    )
  }
})

test_that("impute_na_score fills with NA_real_", {
  skip_if_not(
    exists("build_missing_grid",
           where = asNamespace("scoringutils")),
    "build_missing_grid not yet available"
  )
  scores <- scores_quantile
  metrics <- get_metrics.scores(scores)
  result <- suppressMessages(impute_missing_scores(
    scores, strategy = impute_na_score()
  ))
  imputed <- result[(.imputed)]
  if (nrow(imputed) > 0) {
    for (m in metrics) {
      if (m %in% names(imputed)) {
        expect_true(
          all(is.na(imputed[[m]])),
          info = paste("metric", m, "should be NA")
        )
      }
    }
  }
})

test_that(
  "impute_model_score fills with reference model scores",
  {
    skip_if_not(
      exists("build_missing_grid",
             where = asNamespace("scoringutils")),
      "build_missing_grid not yet available"
    )
    scores <- scores_quantile
    # EuroCOVIDhub-baseline has all 256 targets so can
    # serve as reference for all missing combinations
    result <- suppressMessages(impute_missing_scores(
      scores,
      strategy = impute_model_score(
        "EuroCOVIDhub-baseline"
      )
    ))
    expect_s3_class(result, "scores")
  }
)

test_that(
  "impute_model_score errors when ref model missing target",
  {
    skip_if_not(
      exists("build_missing_grid",
             where = asNamespace("scoringutils")),
      "build_missing_grid not yet available"
    )
    scores <- scores_quantile
    # UMass-MechBayes only has 128 of 256 targets, so
    # it cannot serve as reference for imputing all missing
    # target combinations
    expect_error(
      impute_missing_scores(
        scores,
        strategy = impute_model_score("UMass-MechBayes")
      )
    )
  }
)

test_that("custom strategy function works", {
  skip_if_not(
    exists("build_missing_grid",
           where = asNamespace("scoringutils")),
    "build_missing_grid not yet available"
  )
  custom_strategy <- function(scores, missing_rows, metrics,
                              compare) {
    for (m in metrics) {
      data.table::set(missing_rows, j = m, value = 999)
    }
    return(missing_rows)
  }
  scores <- scores_quantile
  metrics <- get_metrics.scores(scores)
  result <- suppressMessages(impute_missing_scores(
    scores, strategy = custom_strategy
  ))
  imputed <- result[(.imputed)]
  if (nrow(imputed) > 0) {
    for (m in metrics) {
      if (m %in% names(imputed)) {
        expect_true(all(imputed[[m]] == 999))
      }
    }
  }
})

# ==============================================================================
# Integration tests
# ==============================================================================
test_that(
  "impute_missing_scores |> summarise_scores works",
  {
    skip_if_not(
      exists("build_missing_grid",
             where = asNamespace("scoringutils")),
      "build_missing_grid not yet available"
    )
    scores <- scores_quantile
    imputed <- suppressMessages(impute_missing_scores(
      scores, strategy = impute_na_score()
    ))
    result <- summarise_scores(imputed, by = "model")
    expect_s3_class(result, "data.table")
  }
)

test_that(
  "filter then impute pipeline works",
  {
    skip_if_not(
      exists("build_missing_grid",
             where = asNamespace("scoringutils")),
      "build_missing_grid not yet available"
    )
    skip_if_not(
      exists("filter_scores",
             where = asNamespace("scoringutils")),
      "filter_scores not yet available"
    )
    skip_if_not(
      exists("filter_to_include",
             where = asNamespace("scoringutils")),
      "filter_to_include not yet available"
    )
    scores <- scores_quantile
    ref_model <- "EuroCOVIDhub-baseline"

    filtered <- suppressMessages(filter_scores(
      scores,
      strategy = filter_to_include(ref_model)
    ))
    result <- suppressMessages(impute_missing_scores(
      filtered,
      strategy = impute_model_score(ref_model)
    ))
    expect_s3_class(result, "scores")
  }
)

test_that(
  "impute_worst_score skips metrics not in columns",
  {
    scores <- data.table::data.table(
      model = c("A", "A", "B"),
      location = c("DE", "US", "DE"),
      wis = c(1, 2, 3)
    )
    # Attribute claims "wis" and "fake" but "fake" is
    # not a column
    scores <- new_scores(scores, c("wis", "fake"))
    result <- suppressWarnings(suppressMessages(
      impute_missing_scores(
        scores, strategy = impute_worst_score()
      )
    ))
    imputed <- result[result$.imputed]
    expect_equal(nrow(imputed), 1)
    expect_false("fake" %in% names(imputed))
  }
)

test_that(
  "impute_mean_score skips metrics not in columns",
  {
    scores <- data.table::data.table(
      model = c("A", "A", "B"),
      location = c("DE", "US", "DE"),
      wis = c(1, 2, 3)
    )
    scores <- new_scores(scores, c("wis", "fake"))
    result <- suppressWarnings(suppressMessages(
      impute_missing_scores(
        scores, strategy = impute_mean_score()
      )
    ))
    imputed <- result[result$.imputed]
    expect_equal(nrow(imputed), 1)
    expect_false("fake" %in% names(imputed))
  }
)

test_that(
  "impute_model_score errors for nonexistent model",
  {
    scores <- data.table::data.table(
      model = c("A", "A", "B"),
      location = c("DE", "US", "DE"),
      wis = c(1, 2, 3)
    )
    scores <- new_scores(scores, "wis")
    expect_error(
      impute_missing_scores(
        scores,
        strategy = impute_model_score("nonexistent")
      ),
      "not found"
    )
  }
)

test_that(
  "impute_worst_score returns NA when all scores for a target are NA",
  {
    scores <- data.table::data.table(
      model = c("A", "A", "B", "C"),
      location = c("DE", "US", "DE", "DE"),
      wis = c(NA_real_, 2, NA_real_, NA_real_)
    )
    scores <- new_scores(scores, "wis")
    result <- suppressMessages(impute_missing_scores(
      scores, strategy = impute_worst_score()
    ))
    # B and C are missing US; all DE wis are NA so the
    # imputed DE rows for any model should be NA, not -Inf.
    imputed <- result[(.imputed) & location == "DE"]
    expect_true(all(is.na(imputed$wis)))
    expect_false(any(is.infinite(imputed$wis)))
  }
)

test_that(
  "impute_mean_score returns NA when all scores for a target are NA",
  {
    scores <- data.table::data.table(
      model = c("A", "A", "B", "C"),
      location = c("DE", "US", "DE", "DE"),
      wis = c(NA_real_, 2, NA_real_, NA_real_)
    )
    scores <- new_scores(scores, "wis")
    result <- suppressMessages(impute_missing_scores(
      scores, strategy = impute_mean_score()
    ))
    imputed <- result[(.imputed) & location == "DE"]
    expect_true(all(is.na(imputed$wis)))
    expect_false(any(is.nan(imputed$wis)))
  }
)

test_that(
  "impute_missing_scores errors on strategy with wrong formals",
  {
    scores <- data.table::data.table(
      model = c("A", "A", "B"),
      location = c("DE", "US", "DE"),
      wis = c(1, 2, 3)
    )
    scores <- new_scores(scores, "wis")
    bad_strategy <- function(scores, missing_rows) missing_rows
    expect_error(
      impute_missing_scores(scores, strategy = bad_strategy),
      "missing required"
    )
  }
)

test_that(
  "impute_missing_scores works with non-default compare",
  {
    scores <- data.table::data.table(
      forecaster = c("A", "A", "B"),
      location = c("DE", "US", "DE"),
      wis = c(1, 2, 3)
    )
    scores <- new_scores(scores, "wis")
    result <- suppressMessages(impute_missing_scores(
      scores,
      strategy = impute_worst_score(),
      compare = "forecaster"
    ))
    expect_true(".imputed" %in% names(result))
    imputed <- result[result$.imputed]
    expect_equal(nrow(imputed), 1)
    expect_equal(imputed$forecaster, "B")
    expect_equal(imputed$location, "US")
  }
)


# ==============================================================================
# Additional integration tests
# ==============================================================================
test_that(
  "impute then summarise gives equal row counts per model",
  {
    scores <- scores_quantile
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, "model")

    result <- suppressMessages(impute_missing_scores(
      scores, strategy = impute_na_score()
    ))

    # After imputation, every model should have the same
    # number of rows (one per target combination)
    rows_per_model <- result[, .N, by = "model"]
    expect_equal(
      length(unique(rows_per_model$N)), 1,
      info = paste(
        "Expected equal rows per model, got:",
        paste(
          rows_per_model$model,
          rows_per_model$N,
          sep = "=", collapse = ", "
        )
      )
    )
  }
)

test_that(
  "impute_model_score values match reference model scores",
  {
    scores <- scores_quantile
    metrics <- attr(scores, "metrics")
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, "model")
    ref_name <- "EuroCOVIDhub-baseline"

    result <- suppressMessages(impute_missing_scores(
      scores,
      strategy = impute_model_score(ref_name)
    ))
    imputed <- result[(.imputed)]
    expect_gt(nrow(imputed), 0)

    # Get the reference model's actual scores
    ref_scores <- scores[model == ref_name]

    # For each imputed row, the metric values should equal
    # the reference model's scores for the same target
    merged <- merge(
      imputed,
      ref_scores,
      by = target_cols,
      suffixes = c(".imputed", ".ref")
    )
    expect_equal(nrow(merged), nrow(imputed))

    for (m in metrics) {
      col_imp <- paste0(m, ".imputed")
      col_ref <- paste0(m, ".ref")
      if (col_imp %in% names(merged) &&
            col_ref %in% names(merged)) {
        expect_equal(
          merged[[col_imp]],
          merged[[col_ref]],
          info = paste(
            "Imputed", m,
            "should match reference model"
          )
        )
      }
    }
  }
)

test_that(
  "imputation does not alter original score values",
  {
    scores <- scores_quantile
    metrics <- get_metrics.scores(scores)
    result <- suppressMessages(impute_missing_scores(
      scores, strategy = impute_worst_score()
    ))

    originals <- result[!(.imputed)]
    originals[, .imputed := NULL]

    # Original metric values should be unchanged
    for (m in metrics) {
      if (!(m %in% names(originals))) next
      data.table::setkeyv(originals, names(scores))
      data.table::setkeyv(scores, names(scores))
      expect_equal(
        as.numeric(originals[[m]]),
        as.numeric(scores[[m]]),
        info = paste("metric", m, "changed")
      )
    }
  }
)
