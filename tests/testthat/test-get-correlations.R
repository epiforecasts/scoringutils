test_that("get_correlations() works as expected", {
  # expect all to go well in the usual case
  correlations <- expect_no_condition(
    get_correlations(scores_quantile)
  )
  expect_identical(
    colnames(correlations), c(get_metrics.scores(scores_quantile), "metric")
  )

  # expect no error if scores are unsummarised
  # (meaning that coverage will be a logical vector instead of a numeric)
  correlations2 <- expect_no_condition(
    get_correlations(scores_quantile)
  )
  expect_identical(correlations, correlations2)

  expect_s3_class(
    get_correlations(scores_quantile),
    c("scores", "data.table", "data.frame"),
    exact = TRUE
  )

  # passing a data.frame works as long as the metrics attribute is still there
  expect_no_condition(
    get_correlations(as.data.frame(scores_quantile))
  )

  # check we get an error if metrics attribute is missing.
  expect_error(
    get_correlations(as.data.frame(as.matrix(scores_quantile))),
    "Assertion on 'metrics' failed: Must be a subset of"
  )

  # check we get an error if `metrics` contains duplicates
  expect_error(
    get_correlations(scores_quantile, metrics = c("wis", "wis", "bias")),
    "Assertion on 'metrics' failed: Contains duplicated values"
  )
})

test_that("get_correlations() respects the order of the `metrics` argument", {
  m <- rev(get_metrics.scores(scores_quantile))
  correlations <- get_correlations(
    summarise_scores(scores_quantile),
    metrics = m
  )
  # columns, rows and the stored attribute all share the requested order
  expect_identical(colnames(correlations), c(m, "metric"))
  expect_identical(correlations$metric, m)
  expect_identical(get_metrics.scores(correlations), m)
  # the diagonal of the correlation matrix is 1
  expect_equal(
    diag(as.matrix(correlations[, .SD, .SDcols = m])),
    rep(1, length(m)),
    ignore_attr = TRUE
  )
})

# ==============================================================================
# plot_correlation() # nolint: commented_code_linter
# ==============================================================================
test_that("plot_correlations() works as expected", {
  correlations <- get_correlations(
    summarise_scores(
      scores_quantile,
      by = get_forecast_unit(scores_quantile)
    )
  )
  p <- plot_correlations(correlations, digits = 2)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot__correlation", p)

  # expect an error if you forgot to compute correlations
  expect_error(
    plot_correlations(summarise_scores(scores_quantile)),
    "Did you forget to call `scoringutils::get_correlations()`?"
  )
})

test_that("plot_correlations() aligns cells with non-default metrics order", {
  summarised <- summarise_scores(scores_quantile)
  m <- rev(get_metrics.scores(scores_quantile))
  correlations <- get_correlations(summarised, metrics = m)
  p <- plot_correlations(correlations, digits = 2)
  pd <- data.table::as.data.table(p$data)

  # the diagonal of the plotted heatmap must be 1
  expect_true(
    all(pd[as.character(metric) == as.character(variable)]$value == 1)
  )

  # every plotted cell must match the true correlation matrix
  true_cor <- stats::cor(as.matrix(summarised[, .SD, .SDcols = m]))
  expect_equal(
    pd$value,
    round(
      true_cor[cbind(as.character(pd$metric), as.character(pd$variable))],
      2
    ),
    ignore_attr = TRUE
  )

  # same checks with a subset of metrics in non-default order
  m_subset <- c("dispersion", "wis", "bias")
  correlations_subset <- get_correlations(summarised, metrics = m_subset)
  p_subset <- plot_correlations(correlations_subset, digits = 2)
  pd_subset <- data.table::as.data.table(p_subset$data)
  expect_true(
    all(
      pd_subset[as.character(metric) == as.character(variable)]$value == 1
    )
  )
  true_cor_subset <- stats::cor(
    as.matrix(summarised[, .SD, .SDcols = m_subset])
  )
  expect_equal(
    pd_subset$value,
    round(
      true_cor_subset[
        cbind(as.character(pd_subset$metric), as.character(pd_subset$variable))
      ],
      2
    ),
    ignore_attr = TRUE
  )
})

test_that("plot_correlations() aligns rows of a row-scrambled input", {
  # simulate an object created by the pre-fix get_correlations():
  # rows (and the `metric` column) in data-column order, but the `metrics`
  # attribute in a different, user-supplied order
  summarised <- summarise_scores(scores_quantile)
  data_order <- get_metrics.scores(scores_quantile)
  m <- rev(data_order)
  true_cor <- stats::cor(as.matrix(summarised[, .SD, .SDcols = data_order]))
  scrambled <- data.table::as.data.table(true_cor)[, metric := data_order]
  attr(scrambled, "metrics") <- m

  p <- plot_correlations(scrambled, digits = 2)
  pd <- data.table::as.data.table(p$data)

  # the diagonal of the plotted heatmap must be 1
  expect_true(
    all(pd[as.character(metric) == as.character(variable)]$value == 1)
  )

  # every plotted cell must match the true correlation matrix
  expect_equal(
    pd$value,
    round(
      true_cor[cbind(as.character(pd$metric), as.character(pd$variable))],
      2
    ),
    ignore_attr = TRUE
  )
})
