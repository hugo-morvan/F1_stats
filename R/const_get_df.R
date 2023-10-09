#' Constructor Get DF
#' @description
#' given a list from the const_API() function, returns the data into a data.frame format. See const_API.R for more details.
#' 
#' @param year The year you wish to retreive the data from
#'
#' @return the F1 statistics for a single year in a data.frame format
#' @export
#'
#' @importFrom dplyr bind_rows
#' 
source("const_API.R")
const_get_df <- function(year){
  # This function fetches the information from the const_API() function and converts it into a data.frame
  data2 <- const_API(year)
  data_2008 <- as.data.frame(data2$MRData)
  result_2008 <- as.data.frame(data_2008$StandingsTable.StandingsLists.ConstructorStandings)
  
  constructor <- result_2008$Constructor
  constructor_new <- subset(constructor, select = -c(constructorId, url))
  
  result_2008_new <- subset(result_2008, select = -c(position, Constructor, positionText))
  final_2008 <- cbind(result_2008_new, constructor_new)
  final_2008$year <- year
  final_2008 <- final_2008[, c("year","name", "nationality","points" ,"wins")]
  return(final_2008) #returns a dataframe
}