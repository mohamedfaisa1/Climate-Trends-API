#' Extracts Time Series Data for Specific Station
#'
#' Given a data frame containing all station and corresponding weather data
#' and a WBANNO code for a specific station, a data frame is containing all
#' weather data for the specific station between start_date and end_date
#' (inclusive).
#'
#' @param x Data frame containing all stations and weather data
#' @param station_id WBANNO code for a specific station
#' @param start_date Start date (inclusive) of time series
#' @param end_date End date (inclusive) of time series
#' @return A data frame consisting of weather data for a specific station
#' bounded between start_date and end_date.
#' @examples
#' # Get 2023 weather data for station MS_Newton_5_ENE - WBANNO 63831
#' ms_yearly_cycle <- extract_time_series_id(weather_data, 63831,
#'                                        as.Date("2023-01-01"),
#'                                        as.Date("2023-12-31"))
#' @export
extract_time_series_id <- function(x, stat_id, start_date=NULL, end_date=NULL){
  # get station rows via station id (WBANNO)
  inds <- which(x$WBANNO == stat_id)

  # make station data frame
  stat_dat <- x[inds,]

  # check if a start date was provided, if yes filter from (including)
  # the start date
  if(!is.null(start_date)){
    inds <- which(stat_dat$LST_DATE >= start_date) # check if date obj
    stat_dat <- stat_dat[inds,]
  }

  # check if an end date was provided, if yes, cut off all prior dates
  # from the (not including) end date
  if(!is.null(end_date)){
    inds <- which(stat_dat$LST_DATE <= end_date)
    stat_dat <- stat_dat[inds,]
  }

  # return station data
  return(stat_dat)
}

