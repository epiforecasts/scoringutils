# ==============================================================================
# build_missing_grid()
# ==============================================================================
test_that("build_missing_grid() returns correct missing rows", {
  scores <- data.table::data.table(
    model = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    wis = c(1, 2, 3)
  )
  scores <- new_scores(scores, "wis")
  missing <- build_missing_grid(scores, compare = "model")
  expect_s3_class(missing, "data.table")
  expect_equal(nrow(missing), 1)
  expect_equal(missing$model, "B")
  expect_equal(missing$location, "US")
})

test_that("build_missing_grid() returns zero rows when nothing missing", {
  scores <- data.table::data.table(
    model = c("A", "A", "B", "B"),
    location = c("DE", "US", "DE", "US"),
    wis = c(1, 2, 3, 4)
  )
  scores <- new_scores(scores, "wis")
  missing <- build_missing_grid(scores, compare = "model")
  expect_equal(nrow(missing), 0)
})

test_that("build_missing_grid() uses observed target combos", {
  # If we had locations DE, US and dates Mon, Tue but only
  # (DE, Mon) and (US, Tue) are observed, the grid should NOT
  # include (DE, Tue) or (US, Mon)
  scores <- data.table::data.table(
    model = c("A", "A", "B", "B"),
    location = c("DE", "US", "DE", "US"),
    date = c("Mon", "Tue", "Mon", "Tue"),
    wis = c(1, 2, 3, 4)
  )
  scores <- new_scores(scores, "wis")
  missing <- build_missing_grid(scores, compare = "model")
  expect_equal(nrow(missing), 0)

  # Now remove one combo - B should be missing (US, Tue)
  scores2 <- data.table::data.table(
    model = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    date = c("Mon", "Tue", "Mon"),
    wis = c(1, 2, 3)
  )
  scores2 <- new_scores(scores2, "wis")
  missing2 <- build_missing_grid(scores2, compare = "model")
  expect_equal(nrow(missing2), 1)
  expect_equal(missing2$model, "B")
  expect_equal(missing2$location, "US")
  expect_equal(missing2$date, "Tue")
})


# ==============================================================================
# filter_scores()
# ==============================================================================
test_that("filter_scores() drops incomplete targets", {
  scores <- data.table::data.table(
    model = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    wis = c(1, 2, 3)
  )
  scores <- new_scores(scores, "wis")
  result <- suppressMessages(filter_scores(scores))
  # Only DE should remain (both include have it)
  expect_equal(nrow(result), 2)
  expect_true(all(result$location == "DE"))
})

test_that("filter_scores() preserves class and metrics", {
  scores <- data.table::data.table(
    model = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    wis = c(1, 2, 3)
  )
  scores <- new_scores(scores, "wis")
  result <- suppressMessages(filter_scores(scores))
  expect_s3_class(result, "scores")
  expect_equal(attr(result, "metrics"), "wis")
})

test_that("filter_scores() unchanged when nothing missing", {
  scores <- data.table::data.table(
    model = c("A", "A", "B", "B"),
    location = c("DE", "US", "DE", "US"),
    wis = c(1, 2, 3, 4)
  )
  scores <- new_scores(scores, "wis")
  expect_message(
    result <- filter_scores(scores),
    "No rows filtered"
  )
  expect_equal(nrow(result), 4)
})

test_that("filter_scores() reports rows dropped", {
  scores <- data.table::data.table(
    model = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    wis = c(1, 2, 3)
  )
  scores <- new_scores(scores, "wis")
  expect_message(
    filter_scores(scores),
    "Filtered out 1 row"
  )
})

test_that("filter_scores() errors on strategy with wrong formals", {
  scores <- data.table::data.table(
    model = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    wis = c(1, 2, 3)
  )
  scores <- new_scores(scores, "wis")
  bad_strategy <- function(scores) scores
  expect_error(
    filter_scores(scores, strategy = bad_strategy),
    "missing required"
  )
})

test_that("filter_scores() errors on invalid compare column", {
  scores <- data.table::data.table(
    model = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    wis = c(1, 2, 3)
  )
  scores <- new_scores(scores, "wis")
  expect_error(
    filter_scores(scores, compare = "nonexistent"),
    "nonexistent"
  )
})


# ==============================================================================
# filter_to_intersection()
# ==============================================================================
test_that("filter_to_intersection(min_coverage=0.5) works", {
  scores <- data.table::data.table(
    model = c("A", "A", "A", "B", "C"),
    location = c("DE", "US", "FR", "DE", "DE"),
    wis = c(1, 2, 3, 4, 5)
  )
  scores <- new_scores(scores, "wis")
  strategy <- filter_to_intersection(min_coverage = 0.5)
  result <- strategy(scores, compare = "model")
  # DE covered by 3/3, US by 1/3, FR by 1/3
  # At min_coverage = 0.5, only DE qualifies
  expect_true(all(result$location == "DE"))

  strategy2 <- filter_to_intersection(min_coverage = 1 / 3)
  result2 <- strategy2(scores, compare = "model")
  # All locations have coverage >= 1/3
  expect_equal(nrow(result2), 5)
})

test_that("filter_to_include() keeps targets of single model", {
  scores <- data.table::data.table(
    model = c("m1", "m1", "m2", "m2", "m3"),
    location = c("DE", "US", "DE", "FR", "DE"),
    wis = c(1, 2, 3, 4, 5)
  )
  scores <- new_scores(scores, "wis")
  strategy <- filter_to_include("m1")
  result <- strategy(scores, compare = "model")
  # m1 covers DE and US, so keep all rows with DE or US
  expect_true(all(result$location %in% c("DE", "US")))
  # FR should be dropped
  expect_false("FR" %in% result$location)
})

test_that("filter_to_include() intersects multiple models", {
  scores <- data.table::data.table(
    model = c("m1", "m1", "m2", "m2", "m3"),
    location = c("DE", "US", "DE", "FR", "DE"),
    wis = c(1, 2, 3, 4, 5)
  )
  scores <- new_scores(scores, "wis")
  strategy <- filter_to_include(c("m1", "m2"))
  result <- strategy(scores, compare = "model")
  # m1 covers DE, US; m2 covers DE, FR; intersection = DE
  expect_true(all(result$location == "DE"))
})

test_that("filter_to_include() errors on unknown compare value", {
  scores <- data.table::data.table(
    model = c("A", "B"),
    location = c("DE", "DE"),
    wis = c(1, 2)
  )
  scores <- new_scores(scores, "wis")
  expect_error(
    filter_scores(
      scores,
      strategy = filter_to_include("Z")
    ),
    "not found"
  )
})

test_that("filter_scores() works with non-default compare", {
  scores <- data.table::data.table(
    forecaster = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    wis = c(1, 2, 3)
  )
  scores <- new_scores(scores, "wis")
  result <- suppressMessages(filter_scores(
    scores, compare = "forecaster"
  ))
  expect_true(all(result$location == "DE"))
  expect_equal(nrow(result), 2)
})


# ==============================================================================
# Integration tests with scores_quantile
# ==============================================================================
test_that(
  "filter_to_include() with real data filters
   to model's targets",
  {
    scores <- scores_quantile
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, "model")

    # EpiNow2 is missing 9 death targets out of 256
    epinow2_targets <- unique(
      scores[
        model == "epiforecasts-EpiNow2",
        target_cols,
        with = FALSE
      ]
    )
    n_epinow2 <- nrow(epinow2_targets)

    result <- suppressMessages(filter_scores(
      scores,
      strategy = filter_to_include(
        "epiforecasts-EpiNow2"
      )
    ))

    # All remaining targets should be EpiNow2's targets
    result_targets <- unique(
      result[, target_cols, with = FALSE]
    )
    expect_equal(nrow(result_targets), n_epinow2)

    # The 9 death targets EpiNow2 doesn't cover
    # should have been dropped
    all_targets <- unique(
      scores[, target_cols, with = FALSE]
    )
    n_dropped_targets <- nrow(all_targets) - n_epinow2
    expect_equal(n_dropped_targets, 9)

    # Every model in the result should only have
    # targets that EpiNow2 covers
    data.table::setkeyv(result_targets, target_cols)
    data.table::setkeyv(epinow2_targets, target_cols)
    expect_equal(result_targets, epinow2_targets)
  }
)

test_that(
  "filter_to_intersection(min_coverage) boundary with
   real data",
  {
    scores <- scores_quantile
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, "model")

    # 4 models total. UMass-MechBayes has no case targets
    # (128/256), so case targets are covered by 3/4 = 0.75.
    # At min_coverage = 0.75 case targets should be kept.
    result_relaxed <- suppressMessages(filter_scores(
      scores,
      strategy = filter_to_intersection(
        min_coverage = 0.75
      )
    ))
    relaxed_types <- unique(result_relaxed$target_type)
    expect_true("Cases" %in% relaxed_types)
    expect_true("Deaths" %in% relaxed_types)

    # At min_coverage = 1.0 (default), case targets
    # should be dropped because UMass-MechBayes lacks them.
    result_strict <- suppressMessages(filter_scores(scores))
    strict_types <- unique(result_strict$target_type)
    expect_false("Cases" %in% strict_types)
    expect_true("Deaths" %in% strict_types)

    # Relaxed should have strictly more rows
    expect_gt(nrow(result_relaxed), nrow(result_strict))
  }
)

test_that(
  "filter_scores then summarise_scores gives equal target
   counts per model",
  {
    scores <- scores_quantile
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, "model")

    filtered <- suppressMessages(filter_scores(scores))

    # Count distinct targets per model
    targets_per_model <- filtered[,
      .(n_targets = data.table::uniqueN(
        .SD[, target_cols, with = FALSE]
      )),
      by = "model"
    ]
    # All models should have the same number of targets
    expect_length(unique(targets_per_model$n_targets), 1)

    # Summarise should work and give a row per model
    summary <- summarise_scores(filtered, by = "model")
    expect_equal(
      nrow(summary),
      length(unique(filtered$model))
    )
  }
)
