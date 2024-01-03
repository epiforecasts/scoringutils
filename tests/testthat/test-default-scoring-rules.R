test_that("`select_rules` works as expected", {

  expect_equal(
    scoringutils:::select_rules(rules_point(), select = NULL),
    rules_point()
  )

  expect_equal(
    scoringutils:::select_rules(rules_point(), select = NULL),
    scoringutils:::select_rules(rules_point())
  )

  expect_equal(
    names(scoringutils:::select_rules(rules_point(), select = "ape")),
    "ape"
  )

  expect_equal(
    length(scoringutils:::select_rules(rules_point(), select = NULL, exclude = "ape")),
    length(rules_point()) - 1
  )

  # if both select and exclude are specified, exclude is ignored
  expect_equal(
    names(scoringutils:::select_rules(rules_point(), select = "ape", exclude = "ape")),
    "ape"
  )

  # expect error if possibilities is not a list
  expect_error(
    scoringutils:::select_rules(rules_point, select = NULL),
    "Assertion on 'rules' failed: Must be of type 'list', not 'closure'."
  )
})


test_that("default rules work as expected", {

  expect_true(
    all(c(
      is.list(rules_point()),
      is.list(rules_binary()),
      is.list(rules_quantile()),
      is.list(rules_sample()))
    )
  )

  expect_equal(
    names(rules_point(select = "ape")),
    "ape"
  )

  expect_equal(
    length(rules_binary(select = "all", exclude = "brier_score")),
    length(rules_binary()) - 1
  )

  # if both select and exclude are specified, exclude is ignored
  expect_equal(
    names(scoringutils:::select_rules(rules_quantile(), select = "wis", exclude = "wis")),
    "wis"
  )

  # expect error if select is not included in the default possibilities
  expect_error(
    rules_sample(select = "not-included"),
    "Must be a subset of"
  )

  # expect error if exclude is not included in the default possibilities
  expect_error(
    rules_quantile(exclude = "not-included"),
    "Must be a subset of"
  )
})

