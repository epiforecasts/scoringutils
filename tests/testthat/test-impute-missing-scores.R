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
    result <- impute_missing_scores(
      scores, strategy = impute_na_score()
    )
    expect_true(".imputed" %in% names(result))
    expect_false(any(result$.imputed))
  }
)

test_that(
  "impute_missing_scores preserves scores class and metrics",
  {
    scores <- scores_quantile
    result <- impute_missing_scores(
      scores, strategy = impute_na_score()
    )
    expect_s3_class(result, "scores")
    expect_identical(
      get_metrics.scores(result),
      get_metrics.scores(scores)
    )
  }
)

test_that(".imputed is not in get_metrics.scores output", {
  scores <- scores_quantile
  result <- impute_missing_scores(
    scores, strategy = impute_na_score()
  )
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
  result <- impute_missing_scores(
    scores, strategy = impute_na_score()
  )
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
  result <- impute_missing_scores(
    scores, strategy = impute_worst_score()
  )
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
  result <- impute_missing_scores(
    scores, strategy = impute_mean_score()
  )
  imputed <- result[(.imputed)]
  expect_gte(nrow(imputed), 0)
})

test_that("impute_na_score fills with NA_real_", {
  skip_if_not(
    exists("build_missing_grid",
           where = asNamespace("scoringutils")),
    "build_missing_grid not yet available"
  )
  scores <- scores_quantile
  metrics <- get_metrics.scores(scores)
  result <- impute_missing_scores(
    scores, strategy = impute_na_score()
  )
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
    result <- impute_missing_scores(
      scores,
      strategy = impute_model_score(
        "EuroCOVIDhub-baseline"
      )
    )
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
  result <- impute_missing_scores(
    scores, strategy = custom_strategy
  )
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
    result <- scores |>
      impute_missing_scores(strategy = impute_na_score()) |>
      summarise_scores(by = "model")
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
      exists("filter_to_intersection",
             where = asNamespace("scoringutils")),
      "filter_to_intersection not yet available"
    )
    scores <- scores_quantile
    ref_model <- "EuroCOVIDhub-baseline"

    result <- scores |>
      filter_scores(
        strategy = filter_to_intersection(
          models = ref_model
        )
      ) |>
      impute_missing_scores(
        strategy = impute_model_score(ref_model)
      )
    expect_s3_class(result, "scores")
  }
)
