#' Information About All Stations corresponding to US Climate Reference Network
#' (USCRN)
#'
#' A dataset with metadata about all 236 USCRN stations
#'
#' @format a dataframe with 236 rows and 5 columns
#' \describe{
#'     \item{station_id}{station's WBAN number}
#'     \item{station_name}{station's name}
#'     \item{state}{station's state}
#'     \item{LONGITUDE}{longitude of city, in decimal degrees east}
#'     \item{LATITUDE}{latitude of city, in decimal degrees north}
#' }
"station_data"

#' Weather Data from All Stations Corresponding to US Climate Reference Network
#' (USCRN)
#'
#' A dataset with all data from 236 USCRN stations located across the United
#' States
#'
#' @format a dataframe with 1134351 rows and 13 columns
#' \describe{
#'     \item{WBANNO}{station's WBAN number}
#'     \item{state}{station's state}
#'     \item{station_name}{station's name}
#'     \item{LST_DATE}{Local Standard Time (LST) of observations}
#'     \item{CRX_VN}{version number of the station datalogger program}
#'     \item{LONGITUDE}{longitude of station, in decimal degrees east}
#'     \item{LATITUDE}{latitude of station, in decimal degrees north}
#'     \item{T_DAILY_MAX}{maximum air temperature, in degrees C}
#'     \item{T_DAILY_MIN}{minimum air temperature, in degrees C}
#'     \item{T_DAILY_MEAN}{mean air temperature (T_DAILY_MAX + T_DAILY_MIN)/2,
#'     in degrees C}
#'     \item{T_DAILY_AVG}{average air temperature, in degrees C}
#'     \item{P_DAILY_CALC}{total amount of precipitation, in mm}
#'     \item{SOLARAD_DAILY}{total solar energy, in MJ/meter^2}
#' }
"weather_data"
