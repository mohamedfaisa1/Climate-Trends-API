#' Interpolate Grid Points from Station Data
#'
#' Interpolate a response value for grid points in the contiguous United
#' States from the spatial coordinates (longitude and latitude) of stations
#' as well as with custom designed design matrices.
#'
#' @param x Data set which contains station's spatial data
#' (longitude and latitude)
#' @param Y Response vector
#' @param X_mod Design matrix for model
#' @param locs_interp Matrix or data frame of locations in which first column is
#' longitudes (-180, 180) and the second column is latitudes (-90,90), output of
#' \code{\link{create_grid_points}}
#' @param X_interp Design matrix for interpolations
#' @return A vector of length nrow(locs_interp) of interpolated grid points.
#' @examples
#' # interpolate T_DAILY_AVG for grid points within the contiguous United States
#' x <- weather_data[,c(3,4)]
#' Y <- weather_data$T_DAILY_AVG
#' locs_interp <- create_grid_points()
#' X_interp <- model.matrix(~1+Longitude+Latitude, data=locs_interp)
#' preds <- interpolate_points(x, Y, NULL, locs_interp, X_interp)
#' @export
interpolate_points <- function(x, Y, X_mod, locs_interp, X_interp){
  # make matrix of locations for model
  longlat <- cbind.data.frame(LONGITUDE=x[,1],
                              LATITUDE=x[,2])

  # choose specified covariance function
  cov_func <- "matern_sphere"

  # fit model
  sp.mod <- GpGp::fit_model(Y, longlat, X_mod,
                            covfun_name=cov_func,
                            silent=TRUE)
  # make predictions
  preds <- GpGp::predictions(sp.mod, locs_interp, X_interp)

  # return predictions
  return(preds)
}

