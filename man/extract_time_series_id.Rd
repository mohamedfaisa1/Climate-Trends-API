% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_time_series_id.R
\name{extract_time_series_id}
\alias{extract_time_series_id}
\title{Extracts Time Series Data for Specific Station}
\usage{
extract_time_series_id(x, stat_id, start_date = NULL, end_date = NULL)
}
\arguments{
\item{x}{Data frame containing all stations and weather data}

\item{start_date}{Start date (inclusive) of time series}

\item{end_date}{End date (inclusive) of time series}

\item{station_id}{WBANNO code for a specific station}
}
\value{
A data frame consisting of weather data for a specific station
bounded between start_date and end_date.
}
\description{
Given a data frame containing all station and corresponding weather data
and a WBANNO code for a specific station, a data frame is containing all
weather data for the specific station between start_date and end_date
(inclusive).
}
\examples{
# Get 2023 weather data for station MS_Newton_5_ENE - WBANNO 63831
ms_yearly_cycle <- extract_time_series_id(weather_data, 63831,
                                       as.Date("2023-01-01"),
                                       as.Date("2023-12-31"))
}
