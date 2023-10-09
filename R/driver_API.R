#' Driver API
#' @description Creates the API request for the driverStandings API for a given year
#' @param year The year you wish to retrieve the data from
#'
#' @return returns a list
#' @export
#'
#' @importFrom jsonlite fromJSON
driver_API <- function(year){
  #This function gets the raw data from the driverStandings API for a specific year and returns a list
  api_reponse <- paste0("http://ergast.com/api/f1/", year, "/driverStandings.json?limit=1000")
  raw_data <- jsonlite::fromJSON(api_reponse)
  return(raw_data) #returns a list
}