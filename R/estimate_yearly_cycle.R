#' Estimates the Expected Average Temperature per Day in a Year for a Given
#' Station
#'
#' Given a data frame containing all station and corresponding weather data
#' and a WBANNO code for a specific station, a data frame is returned
#' with the first column corresponding to the day number (1-366) and the
#' second column containing the expected average temperature (degrees Celsius)
#' for that day for the station will be returned.
#' @param x Data frame containing all stations and weather data
#' @param station_id WBANNO code for a specific station
#' @param start_d Start date (inclusive) of time series
#' @param end_d End date (inclusive) of time series
#' @return A data frame with the first column containing day number (1-366) and
#' the second column containing the expected average temperature for that day.
#' @examples
#' # Estimate yearly cycle for station MS_Newton_5_ENE - WBANNO 63831
#' ms_yearly_cycle <- estimate_yearly_cycle(weather_data, 63831)
#' @export
estimate_yearly_cycle <- function(x, station_id, start_d=NULL, end_d=NULL){
  # make column names for data frame that gets returned
  c.names <- c("DAY_NUMBER", "EXPECTED_TEMP")


  # make station data frame
  stat_data <- extract_time_series_id(x, station_id, start_d, end_d)

  # get day number (1 - 366)
  stat_data$yday <- lubridate::yday(stat_data$LST_DATE)

  # compute expected temperature data frame
  min_date <- as.Date("2000-01-01") #epoch
  stat_data$day <- as.numeric(stat_data$LST_DATE - min_date)

  lin.mod <- lm(T_DAILY_AVG ~ cos((2*pi*day)/365.25) + sin((2*pi*day)/365.25)
                + cos((2.5*pi*day)/365.25)^2
                + cos(((2*pi*yday)/365.25) + 1.27),
                data=stat_data, na.action=na.exclude)

  # make predictions
  day_nums <- sort(unique(stat_data$yday))
  expect_temp <- predict(lin.mod,newdata=data.frame(day=day_nums,
                                                    yday=day_nums))

  yearly.cycle.df <- cbind.data.frame(day_nums, expect_temp)
  colnames(yearly.cycle.df) <- c.names

  # return expected temperature per day df
  return(yearly.cycle.df)
}





