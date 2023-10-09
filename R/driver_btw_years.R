#' Driver Between Years
#' @description
#' This function creates and return a data.frame for the driverStandings API for the years between 'from' and 'to'

#' 
#' @param from the starting year
#' @param to the ending year
#'
#' @return the concatenated data.frame for the requested years
#' @export
#'
source("driver_get_df")
driver_btw_years <- function(from,to){
  # This function creates and return a data.frame for the driverStandings API for the years between 'from' and 'to'
  df <- driver_get_df(from)
  for (x in (from+1):to) {
    new_year <- driver_get_df(x)
    df <- rbind(df, new_year )
  }
  return(df)
}