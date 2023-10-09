#' Constructor API
#' @description This function gets the raw data from the constructorStandings API for a specific year and returns a list
#' @param year The year you wish to retrieve the data from
#'
#' @return returns a list
#' @export
#'
#' @importFrom jsonlite fromJSON
#' 
const_API <- function(year){
  # This function gets the raw data from the constructorStandings API for a specific year and returns a list
  api_reponse <- paste0("http://ergast.com/api/f1/", year, "/constructorStandings.json?limit=10000")
  raw_data <- jsonlite::fromJSON(api_reponse)
  return(raw_data) #returns a list
}