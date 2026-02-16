# ==============================================================================
# as_forecast_multivariate_point() # nolint: commented_code_linter
# ==============================================================================
test_that("as_forecast_multivariate_point() works as expected", {
  test <- na.omit(data.table::copy(example_point))
  data.table::setnames(test,
    old = c("observed", "predicted"),
    new = c("obs", "pred")
  )
  expect_no_condition(
    as_forecast_multivariate_point(test,
      observed = "obs", predicted = "pred",
      forecast_unit = c(
        "location", "model", "target_type",
        "target_end_date", "horizon"
      ),
      joint_across = "location"
    )
  )
})

test_that(
  "as_forecast_multivariate_point() creates expected structure",
  {
    test <- na.omit(data.table::copy(example_point))

    result <- as_forecast_multivariate_point(test,
      forecast_unit = c(
        "location", "model", "target_type",
        "target_end_date", "horizon"
      ),
      joint_across = "location"
    )

    expect_snapshot({
      cat("Class:", class(result), "\n")
      cat(
        "Forecast type:",
        scoringutils:::get_forecast_type(result), "\n"
      )
      cat(
        "Forecast unit:",
        toString(get_forecast_unit(result)), "\n"
      )
      cat("Number of rows:", nrow(result), "\n")
      cat("Number of columns:", ncol(result), "\n")
      cat(
        "Column names:",
        toString(names(result)), "\n"
      )
      cat(
        "Number of unique groups:",
        length(unique(result$.mv_group_id)), "\n"
      )
    })
  }
)


# ==============================================================================
# is_forecast_multivariate_point()
# ==============================================================================
test_that(
  "is_forecast_multivariate_point() works as expected",
  {
    data <- na.omit(data.table::copy(example_point))
    mv_point <- as_forecast_multivariate_point(
      data,
      forecast_unit = c(
        "location", "model", "target_type",
        "target_end_date", "horizon"
      ),
      joint_across = "location"
    )
    expect_true(is_forecast_multivariate_point(mv_point))
    expect_false(is_forecast_multivariate_point(example_binary))
    expect_false(is_forecast_multivariate_point(example_point))
    expect_false(is_forecast_point(mv_point))
  }
)


# ==============================================================================
# get_metrics.forecast_multivariate_point()
# ==============================================================================
test_that(
  "get_metrics.forecast_multivariate_point() works as expected",
  {
    data <- na.omit(data.table::copy(example_point))
    mv_point <- as_forecast_multivariate_point(
      data,
      forecast_unit = c(
        "location", "model", "target_type",
        "target_end_date", "horizon"
      ),
      joint_across = "location"
    )
    result <- get_metrics(mv_point)
    expect_type(result, "list")
    expect_named(result, "variogram_score")
  }
)


# ==============================================================================
# score.forecast_multivariate_point()
# ==============================================================================
test_that(
  "score.forecast_multivariate_point() works as expected",
  {
    data <- na.omit(data.table::copy(example_point))
    mv_point <- as_forecast_multivariate_point(
      data,
      forecast_unit = c(
        "location", "model", "target_type",
        "target_end_date", "horizon"
      ),
      joint_across = "location"
    )

    scores <- score(mv_point)
    expect_true(is.data.table(scores))
    expect_true("variogram_score" %in% names(scores))
    expect_type(scores$variogram_score, "double")
  }
)

test_that(
  "score.forecast_multivariate_point() creates expected output",
  {
    data <- na.omit(data.table::copy(example_point))
    mv_point <- as_forecast_multivariate_point(
      data,
      forecast_unit = c(
        "location", "model", "target_type",
        "target_end_date", "horizon"
      ),
      joint_across = "location"
    )

    scores <- score(mv_point)

    expect_snapshot({
      cat("Class:", class(scores), "\n")
      cat("Number of rows:", nrow(scores), "\n")
      cat("Number of columns:", ncol(scores), "\n")
      cat(
        "Column names:",
        toString(names(scores)), "\n"
      )
      cat(
        "Variogram score range:",
        paste(
          range(scores$variogram_score, na.rm = TRUE),
          collapse = " to "
        ), "\n"
      )
      cat(
        "Number of non-NA scores:",
        sum(!is.na(scores$variogram_score)), "\n"
      )
    })
  }
)


# ==============================================================================
# Error cases for as_forecast_multivariate_point()
# ==============================================================================
test_that(
  "as_forecast_multivariate_point() handles errors appropriately",
  {
    data <- na.omit(data.table::as.data.table(example_point))

    expect_error(
      as_forecast_multivariate_point(data),
      "joint_across.*must be provided"
    )

    expect_error(
      as_forecast_multivariate_point(
        data,
        joint_across = c("nonexistent_column")
      ),
      "Must be a subset of"
    )
  }
)
