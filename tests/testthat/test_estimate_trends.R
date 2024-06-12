testthat::test_that("estimate_trends looks good!",
{
  ms_yc <- estimate_trends(weather_data, 63831)
  testthat::expect_equal(dim(ms_yc), NULL)
  testthat::expect_equal(length(ms_yc), 3)
  testthat::expect_true(is.numeric(ms_yc))
}
)
