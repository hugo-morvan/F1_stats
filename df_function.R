F1_df <- function(year){
  url2 <- paste0("http://ergast.com/api/f1/", year, "/driverStandings.json")
  data2 <- fromJSON(url2)
  
  data_2008 <- as.data.frame(data2$MRData)
  result_2008 <- as.data.frame(data_2008$StandingsTable.StandingsLists.DriverStandings)
  
  driver <- result_2008$Driver
  
  ls <- result_2008$Constructors
  for (i in 1:length(ls)){
    if(nrow(ls[[i]])){
      df <- as.data.frame(ls[i])
      merged_name <- paste(df$name, collapse = '/')
      merged_nationality <- paste(df$nationality, collapse = '/')
      df['name'] <- merged_name
      df['nationality'] <- merged_nationality
      ls[[i]] <- df[1,]
    }
  }
  constructer <- dplyr::bind_rows(ls)
  
  result_2008_new <- subset(result_2008, select = -c(Driver, Constructors, positionText))
  #result_2008_new
  
  constructer_keeps <- c("name", "nationality")
  constructer_new <- constructer[constructer_keeps]
  colnames(constructer_new)[colnames(constructer_new) == "name"] <- "constructer"
  colnames(constructer_new)[colnames(constructer_new) == "nationality"] <- "constructer nationality"
  
  driver_keeps <- c("givenName","familyName", "nationality")
  driver_new <- driver[driver_keeps]
  driver_new$driver <- paste0(driver$givenName, " ", driver$familyName)
  driver_new <- subset(driver_new, select = -c(givenName, familyName))
  colnames(driver_new)[colnames(driver_new) == "nationality"] <- "driver nationality"
  
  final_2008 <- cbind(result_2008_new, constructer_new, driver_new)
  final_2008 <- final_2008[, c("position", "points", "wins", "driver", "driver nationality", "constructer","constructer nationality")]
  return(final_2008)
}