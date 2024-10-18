# ==============================================================================
# as_forecast_sample()
# ==============================================================================
test_that("as_forecast_sample() works as expected", {
    test <- na.omit(data.table::copy(example_sample_continuous))
    data.table::setnames(test,
        old = c("observed", "predicted", "sample_id"),
        new = c("obs", "pred", "sample")
    )
    expect_no_condition(
        as_forecast_sample(test,
            observed = "obs", predicted = "pred",
            forecast_unit = c(
                "location", "model", "target_type",
                "target_end_date", "horizon"
            ),
            sample_id = "sample"
        )
    )
})

test_that("Running `as_forecast_sample()` twice returns the same object", {
    ex <- na.omit(example_sample_continuous)

    expect_identical(
        as_forecast_sample(as_forecast_sample(ex)),
        as_forecast_sample(ex)
    )
})


# ==============================================================================
# is_forecast_sample()
# ==============================================================================
test_that("is_forecast_sample() works as expected", {
    expect_true(is_forecast_sample(example_sample_continuous))
    expect_false(is_forecast_sample(example_binary))
    expect_false(is_forecast_sample(example_point))
    expect_false(is_forecast_sample(example_quantile))
    expect_false(is_forecast_sample(example_nominal))
    expect_false(is_forecast_sample(1:10))
})


# ==============================================================================
# get_metrics.forecast_sample()
# ==============================================================================

test_that("get_metrics.forecast_sample() works as expected", {
    expect_true(
        is.list(get_metrics(example_sample_continuous))
    )
    expect_true(
        is.list(get_metrics(example_sample_discrete))
    )
})


# ==============================================================================
# get_pit_histogram.forecast_sample()
# ==============================================================================
test_that("get_pit_histogram.forecast_sample() works as expected", {
    pit_continuous <- get_pit_histogram(example_sample_continuous, by = c("model", "target_type"))
    pit_integer <- get_pit_histogram(example_sample_discrete, by = c("model", "location"))

    expect_equal(names(pit_continuous), c("model", "target_type", "density", "bin", "mid"))
    expect_equal(names(pit_integer), c("model", "location", "density", "bin", "mid"))

    # check printing works
    expect_output(print(pit_continuous))
    expect_output(print(pit_integer))

    # check class is correct
    expect_s3_class(pit_continuous, c("data.table", "data.frame"), exact = TRUE)
    expect_s3_class(pit_integer, c("data.table", "data.frame"), exact = TRUE)
})
