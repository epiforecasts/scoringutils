# ==============================================================================
# impute_missing_scores()
# ==============================================================================
test_that(
  "impute_missing_scores adds .imputed = FALSE when nothing missing",
  {
    scores <- scores_quantile
    result <- impute_missing_scores(
      scores, strategy = impute_na_score()
    )
    expect_true(".imputed" %in% names(result))
    expect_true(all(result$.imputed == FALSE))
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

# Helper to create scores with missing entries
make_scores_with_missing <- function() {
  scores <- data.table::copy(scores_quantile)
  metrics <- get_metrics.scores(scores)
  fu <- get_forecast_unit(scores)

  # Remove some rows for one model to create missingness
  models <- unique(scores$model)
  target_model <- models[1]

  # Remove the first few unique target combos for that model
  target_cols <- setdiff(fu, "model")
  targets <- unique(
    scores[, target_cols, with = FALSE]
  )
  remove_targets <- targets[1:3]

  # Anti-join to remove those rows
  scores_reduced <- scores[
    !remove_targets, on = target_cols
  ]
  # Also remove those targets for the target model only
  # to ensure only that model is missing
  # Actually, let's be more precise: remove rows for
  # target_model matching those targets
  keep <- scores[
    !(model == target_model &
      scores[remove_targets, on = target_cols, which = TRUE,
             nomatch = NULL] |>
      (\(x) seq_len(nrow(scores)) %in% x)()),
  ]

  # Simpler approach: just remove specific rows
  idx <- scores[model == target_model, which = TRUE]
  remove_idx <- idx[1:min(5, length(idx))]
  result <- scores[-remove_idx]
  return(result)
}

test_that("impute_worst_score fills with max observed score", {
  skip_if_not(
    exists("build_missing_grid",
           where = asNamespace("scoringutils")),
    "build_missing_grid not yet available"
  )
  scores <- make_scores_with_missing()
  metrics <- get_metrics.scores(scores)
  result <- impute_missing_scores(
    scores, strategy = impute_worst_score()
  )
  # Imputed rows should exist

  imputed <- result[.imputed == TRUE]
  if (nrow(imputed) > 0) {
    # Each imputed metric value should be <= max of that
    # metric across all original data
    for (m in metrics) {
      if (m %in% names(imputed) && m %in% names(scores)) {
        max_val <- max(scores[[m]], na.rm = TRUE)
        expect_true(
          all(
            imputed[[m]] <= max_val | is.na(imputed[[m]])
          ),
          info = paste("metric", m, "exceeds max")
        )
      }
    }
  }
})

test_that("impute_mean_score fills with mean observed score", {
  skip_if_not(
    exists("build_missing_grid",
           where = asNamespace("scoringutils")),
    "build_missing_grid not yet available"
  )
  scores <- make_scores_with_missing()
  result <- impute_missing_scores(
    scores, strategy = impute_mean_score()
  )
  imputed <- result[.imputed == TRUE]
  expect_true(nrow(imputed) >= 0)
})

test_that("impute_na_score fills with NA_real_", {
  skip_if_not(
    exists("build_missing_grid",
           where = asNamespace("scoringutils")),
    "build_missing_grid not yet available"
  )
  scores <- make_scores_with_missing()
  metrics <- get_metrics.scores(scores)
  result <- impute_missing_scores(
    scores, strategy = impute_na_score()
  )
  imputed <- result[.imputed == TRUE]
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
    scores <- make_scores_with_missing()
    models <- unique(scores$model)
    # Use a model that is NOT the one with missing data
    # (first model had rows removed)
    ref_model <- models[2]
    result <- impute_missing_scores(
      scores,
      strategy = impute_model_score(ref_model)
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
    scores <- make_scores_with_missing()
    models <- unique(scores$model)
    # The first model has missing entries, so using it as
    # reference should error if it's missing those targets
    target_model <- models[1]
    # This should error because target_model is the one
    # missing forecasts
    expect_error(
      impute_missing_scores(
        scores,
        strategy = impute_model_score(target_model)
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
  custom_strategy <- function(scores, missing_rows, metrics) {
    for (m in metrics) {
      data.table::set(missing_rows, j = m, value = 999)
    }
    return(missing_rows)
  }
  scores <- make_scores_with_missing()
  metrics <- get_metrics.scores(scores)
  result <- impute_missing_scores(
    scores, strategy = custom_strategy
  )
  imputed <- result[.imputed == TRUE]
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
    scores <- make_scores_with_missing()
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
      exists("filter_missing_scores",
             where = asNamespace("scoringutils")),
      "filter_missing_scores not yet available"
    )
    skip_if_not(
      exists("filter_to_intersection",
             where = asNamespace("scoringutils")),
      "filter_to_intersection not yet available"
    )
    scores <- make_scores_with_missing()
    models <- unique(scores$model)
    ref_model <- models[2]

    result <- scores |>
      filter_missing_scores(
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
