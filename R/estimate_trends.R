#' Estimates the Average Temperature Trend Over Time for a Station
#'
#' Given a data frame containing all station and corresponding weather data
#' and a WBANNO code for a specific station, a vector of predictions for
#' the average temperature trend over time (degrees Celsius per year)
#' for the station will be returned as well as information pertaining to
#' the statistical significance of the trend.
#'
#' @param x Data frame containing all stations and weather data
#' @param station_id WBANNO code for a specific station
#' @return A vector containing the estimated temperature trend overtime in
#' units of degrees Celsius per year, the statistical significance of
#' that trend, and the standard error estimate of the trend.
#' @examples
#' # Get Trend for Station NC_Asheville_13_S - WBANNO 53878
#' trend_dat <- estimate_trends(weather_data, 53878)
#' @export
estimate_trends <- function(x, station_id){
  # filter by station id (WBANNO)
  inds <- which(x$WBANNO == station_id)

  # get weather data for station
  stat_data <- x[inds,]

  # get day number (1 - 366)
  stat_data$yday <- lubridate::yday(stat_data$LST_DATE)

  # set "epoch" time -> include days
  min_date = as.Date("2000-01-01")

  # create new column tracking each entries number of days since the epoch
  stat_data$day <- as.numeric(stat_data$LST_DATE - min_date)

  # fit a linear model
  lin.mod <- lm(T_DAILY_AVG ~ day + cos((2*pi*day)/365.25)
               + sin((2*pi*day)/365.25) + cos((2*pi*day)/365.25)^2
               + cos(((2*pi*yday)/365.25) + 1.27)
               + sin((2*pi*yday)/365.25),
               data=stat_data, na.action=na.exclude)

  # get summary of linear model
  lin.sum <- summary(lin.mod)

  # get trend coefficient
  trend <- lin.sum$coefficients[,1][2]

  #get trend p-value
  trend_pval <- lin.sum$coefficient[,4][2]

  #get trend SE
  trend_se <- lin.sum$coefficients[,2][2]

  # return data
  trend_dat <- c(trend, trend_se, trend_pval)
  return(trend_dat)
}
