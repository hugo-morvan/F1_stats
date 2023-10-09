#' Driver Get DF
#' @description
#' given a list from the driver_API() function, returns the data into a data.frame format. See driver_API.R for more details.
#' 
#' @param year The year you wish to retreive the data from
#'
#' @return the F1 statistics for a single year in a data.frame format
#' @export
#'
#' @importFrom dplyr bind_rows
source("driver_API.R")
driver_get_df <- function(year){
  # This function fetches the information from the driver_API() function and converts it into a data.frame
  # Returns the F1 statistics for a single year
  raw_data <- driver_API(year)
  reponse_data <- as.data.frame(raw_data$MRData)
  result <- as.data.frame(reponse_data$StandingsTable.StandingsLists.DriverStandings)
  driver <- result$Driver
  constr_list <- result$Constructors
  for (i in 1:length(constr_list)){
    if(nrow(constr_list[[i]])){
      df <- as.data.frame(constr_list[i])
      merged_name <- paste(df$name, collapse = '/')
      merged_nationality <- paste(df$nationality, collapse = '/')
      df['name'] <- merged_name
      df['nationality'] <- merged_nationality
      constr_list[[i]] <- df[1,]
    }
  }
  constructer <- dplyr::bind_rows(constr_list)
  
  result_new <- subset(result, select = -c(Driver, Constructors, positionText))
  
  #Retrieve necessary data from constructor dataframe
  constructer_keeps <- c("name", "nationality")
  constructer_new <- constructer[constructer_keeps]
  colnames(constructer_new)[colnames(constructer_new) == "name"] <- "constructer"
  colnames(constructer_new)[colnames(constructer_new) == "nationality"] <- "constructer nationality"
  
  #Retrieve necessary data from driver dataframe
  driver_keeps <- c("givenName","familyName", "nationality")
  driver_new <- driver[driver_keeps]
  driver_new$driver <- paste0(driver$givenName, " ", driver$familyName)
  driver_new <- subset(driver_new, select = -c(givenName, familyName))
  colnames(driver_new)[colnames(driver_new) == "nationality"] <- "driver nationality"
  
  final <- cbind(result_new, constructer_new, driver_new)
  final$year <- year
  final <- final[, c("year","position", "points", "wins", "driver", "driver nationality", "constructer","constructer nationality")]
  #Return a data.frame
  return(final)
}
