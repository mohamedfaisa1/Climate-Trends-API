testthat::test_that("extracting_time_series_id looks good!",
{
  ms_yc<- extract_time_series_id(weather_data, 63831,
                                           as.Date("2023-01-01"),
                                           as.Date("2023-12-31"))
  testthat::expect_equal(dim(ms_yc), c(365,13))
  testthat::expect_equal(names(ms_yc),
  c("WBANNO","state","station_name","LST_DATE","CRX_VN","LONGITUDE","LATITUDE",
    "T_DAILY_MAX","T_DAILY_MIN","T_DAILY_MEAN","T_DAILY_AVG","P_DAILY_CALC",
    "SOLARAD_DAILY"))
  testthat::expect_equal(unique(ms_yc[,1]), 63831)
}
)
