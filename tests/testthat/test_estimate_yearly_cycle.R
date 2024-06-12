testthat::test_that("estimate_yearly_cycle looks good!",
{
  ms_yc <- estimate_yearly_cycle(weather_data, 63831)
  testthat::expect_equal(dim(ms_yc), c(366,2))
  testthat::expect_equal(names(ms_yc),
   c("DAY_NUMBER","EXPECTED_TEMP"))
  testthat::expect_true(is.numeric(ms_yc[,2]))
}
)
