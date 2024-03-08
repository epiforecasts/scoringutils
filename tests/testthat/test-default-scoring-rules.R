test_that("`select_metrics` works as expected", {

  expect_equal(
    scoringutils:::select_metrics(metrics_point(), select = NULL),
    metrics_point()
  )

  expect_equal(
    scoringutils:::select_metrics(metrics_point(), select = NULL),
    scoringutils:::select_metrics(metrics_point())
  )

  expect_equal(
    names(scoringutils:::select_metrics(metrics_point(), select = "ape")),
    "ape"
  )

  expect_equal(
    length(scoringutils:::select_metrics(metrics_point(), select = NULL, exclude = "ape")),
    length(metrics_point()) - 1
  )

  # if both select and exclude are specified, exclude is ignored
  expect_equal(
    names(scoringutils:::select_metrics(metrics_point(), select = "ape", exclude = "ape")),
    "ape"
  )

  # expect error if possibilities is not a list
  expect_error(
    scoringutils:::select_metrics(metrics_point, select = NULL),
    "Assertion on 'rules' failed: Must be of type 'list', not 'closure'."
  )
})


test_that("default rules work as expected", {

  expect_true(
    all(c(
      is.list(metrics_point()),
      is.list(metrics_binary()),
      is.list(metrics_quantile()),
      is.list(metrics_sample()))
    )
  )

  expect_equal(
    names(metrics_point(select = "ape")),
    "ape"
  )

  expect_equal(
    length(metrics_binary(select = NULL, exclude = "brier_score")),
    length(metrics_binary()) - 1
  )

  # if both select and exclude are specified, exclude is ignored
  expect_equal(
    names(scoringutils:::select_metrics(metrics_quantile(), select = "wis", exclude = "wis")),
    "wis"
  )

  # expect error if select is not included in the default possibilities
  expect_error(
    metrics_sample(select = "not-included"),
    "Must be a subset of"
  )

  # expect error if exclude is not included in the default possibilities
  expect_error(
    metrics_quantile(exclude = "not-included"),
    "Must be a subset of"
  )
})

