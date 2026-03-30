# nolint start: object_usage_linter
make_mv_point <- function() {
  data <- na.omit(data.table::copy(example_point))
  as_forecast_multivariate_point(
    data,
    forecast_unit = c(
      "location", "model", "target_type",
      "target_end_date", "horizon"
    ),
    joint_across = "location"
  )
}
# nolint end

test_that(
  "as_forecast_multivariate_point() creates expected structure",
  {
    result <- make_mv_point()

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

test_that(
  "is_forecast_multivariate_point() works as expected",
  {
    mv_point <- make_mv_point()
    expect_true(is_forecast_multivariate_point(mv_point))
    expect_false(
      is_forecast_multivariate_point(example_binary)
    )
    expect_false(
      is_forecast_multivariate_point(example_point)
    )
    expect_false(is_forecast_point(mv_point))
  }
)

test_that(
  "get_metrics.forecast_multivariate_point() works as expected",
  {
    mv_point <- make_mv_point()
    result <- get_metrics(mv_point)
    expect_type(result, "list")
    expect_named(result, "variogram_score")
  }
)

test_that(
  "score.forecast_multivariate_point() works as expected",
  {
    mv_point <- make_mv_point()

    scores <- score(mv_point)
    expect_true(is.data.table(scores))
    expect_true("variogram_score" %in% names(scores))
    expect_type(scores$variogram_score, "double")
  }
)

test_that(
  "variogram_score_multivariate_point() matches scoringRules::vs_sample()",
  {
    set.seed(42)
    d <- 4
    obs1 <- rnorm(d)
    pred1 <- rnorm(d)
    obs2 <- rnorm(d)
    pred2 <- rnorm(d)

    vs_sr1 <- scoringRules::vs_sample(
      y = obs1, dat = matrix(pred1, ncol = 1)
    )
    vs_sr2 <- scoringRules::vs_sample(
      y = obs2, dat = matrix(pred2, ncol = 1)
    )

    vs_su <- variogram_score_multivariate_point(
      observed = c(obs1, obs2),
      predicted = matrix(c(pred1, pred2), ncol = 1),
      mv_group_id = c(rep(1, d), rep(2, d))
    )
    expect_equal(
      unname(vs_su), c(vs_sr1, vs_sr2),
      tolerance = 1e-6
    )
  }
)

test_that(
  "score.forecast_multivariate_point() creates expected output",
  {
    mv_point <- make_mv_point()

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

    bad_data <- data.table::copy(data)
    bad_data[, predicted := as.character(predicted)]
    expect_error(
      as_forecast_multivariate_point(
        bad_data,
        forecast_unit = c(
          "location", "model", "target_type",
          "target_end_date", "horizon"
        ),
        joint_across = "location"
      ),
      "Checking `forecast`"
    )
  }
)
