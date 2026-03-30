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

test_that(
  "build_missing_grid() uses observed target combinations", {
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
# filter_missing_scores()
# ==============================================================================
test_that(
  "filter_missing_scores() with default strategy drops incomplete", {
  scores <- data.table::data.table(
    model = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    wis = c(1, 2, 3)
  )
  scores <- new_scores(scores, "wis")
  result <- filter_missing_scores(scores)
  # Only DE should remain (both models have it)
  expect_equal(nrow(result), 2)
  expect_true(all(result$location == "DE"))
})

test_that(
  "filter_missing_scores() preserves scores class and metrics", {
  scores <- data.table::data.table(
    model = c("A", "A", "B"),
    location = c("DE", "US", "DE"),
    wis = c(1, 2, 3)
  )
  scores <- new_scores(scores, "wis")
  result <- filter_missing_scores(scores)
  expect_s3_class(result, "scores")
  expect_equal(attr(result, "metrics"), "wis")
})

test_that(
  "filter_missing_scores() returns unchanged when nothing missing", {
  scores <- data.table::data.table(
    model = c("A", "A", "B", "B"),
    location = c("DE", "US", "DE", "US"),
    wis = c(1, 2, 3, 4)
  )
  scores <- new_scores(scores, "wis")
  expect_message(
    result <- filter_missing_scores(scores),
    "No missing"
  )
  expect_equal(nrow(result), 4)
})


# ==============================================================================
# filter_to_intersection()
# ==============================================================================
test_that(
  "filter_to_intersection(min_coverage = 0.5) keeps partial", {
  scores <- data.table::data.table(
    model = c("A", "A", "A", "B", "C"),
    location = c("DE", "US", "FR", "DE", "DE"),
    wis = c(1, 2, 3, 4, 5)
  )
  scores <- new_scores(scores, "wis")
  strategy <- filter_to_intersection(min_coverage = 0.5)
  result <- strategy(scores, compare = "model")
  # DE covered by 3/3 = 1.0, US by 1/3 = 0.33, FR by 1/3 = 0.33
  # At min_coverage = 0.5, only DE qualifies
  expect_true(all(result$location == "DE"))

  strategy2 <- filter_to_intersection(min_coverage = 1 / 3)
  result2 <- strategy2(scores, compare = "model")
  # All locations have coverage >= 1/3
  expect_equal(nrow(result2), 5)
})

test_that(
  "filter_to_intersection(models = 'model1') keeps that model's targets", {
  scores <- data.table::data.table(
    model = c("m1", "m1", "m2", "m2", "m3"),
    location = c("DE", "US", "DE", "FR", "DE"),
    wis = c(1, 2, 3, 4, 5)
  )
  scores <- new_scores(scores, "wis")
  strategy <- filter_to_intersection(models = "m1")
  result <- strategy(scores, compare = "model")
  # m1 covers DE and US, so keep all rows with DE or US
  expect_true(all(result$location %in% c("DE", "US")))
  # FR should be dropped
  expect_false("FR" %in% result$location)
})

test_that(
  "filter_to_intersection(models = c('m1', 'm2')) keeps intersection", {
  scores <- data.table::data.table(
    model = c("m1", "m1", "m2", "m2", "m3"),
    location = c("DE", "US", "DE", "FR", "DE"),
    wis = c(1, 2, 3, 4, 5)
  )
  scores <- new_scores(scores, "wis")
  strategy <- filter_to_intersection(models = c("m1", "m2"))
  result <- strategy(scores, compare = "model")
  # m1 covers DE, US; m2 covers DE, FR; intersection = DE
  expect_true(all(result$location == "DE"))
})
