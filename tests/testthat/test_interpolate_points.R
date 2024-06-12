testthat::test_that("interpolate_points looks good!",
{
  # make station data on contigous USA
  HI_inds <- which(station_data$state == "HI") # Hawaii indexes
  AK_inds <- which(station_data$state == "AK") # Alaska indexes
  joint_inds <- c(AK_inds, HI_inds)
  contig_data <- station_data[-joint_inds,]

  # extract station data
  stat_ids <- contig_data$station_id
  stat_lons <- contig_data$LONGITUDE
  stat_lats <- contig_data$LATITUDE
  n <- length(stat_ids)

  # make average temperature vector & setup dates
  avg_temps <- rep(NA, n)
  start_d <- as.Date("2024-03-01")
  end_d <- as.Date("2024-03-31")
  for(i in 1:n){
    wbanno <- stat_ids[i]
    station <- extract_time_series_id(weather_data, wbanno, start_d, end_d)
    avg_temps[i] <- mean(station$T_DAILY_AVG, na.rm=T)
  }

  # make data frame for temperature + spatial data
  WD <- cbind.data.frame(lon=stat_lons, lat=stat_lats, avg_temp=avg_temps)
  WD <- na.omit(WD) # remove NA

  # make spatial data frame
  spat.df <- data.frame(x=WD$lon, y=WD$lat)

  # make response vector
  Y <- WD$avg_temp

  # make design matrix for model
  X_mod <- model.matrix(~1+x+y, data=spat.df)

  # get grid points in contiguous USA + elevation data
  g <- create_grid_points(resolution=50)
  colnames(g) <- c("x", "y") # change lon to x and lat to y for elavatr

  # make design matrix for interpolations
  X_interp <- model.matrix(~1+x+y, data=g)

  # make interpolations
  interps <- interpolate_points(WD, Y, X_mod, g, X_interp)

  testthat::expect_null(dim(interps))
  testthat::expect_equal(length(interps),
                         nrow(g))
  testthat::expect_true(is.numeric(interps))
}
)
