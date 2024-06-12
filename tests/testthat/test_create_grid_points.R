testthat::test_that("create_grid_points looks good!",
{
  g <- create_grid_points(50)
  testthat::expect_equal(dim(g), c(1401,2))
  testthat::expect_equal(names(g),
                         c("Longitude", "Latitude"))
  testthat::expect_equal(class(g), "data.frame")
  testthat::expect_equal(typeof(g), "list")
}
)
