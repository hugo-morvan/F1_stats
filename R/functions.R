library(jsonlite)

# TO BE DELETED

#=== Driver API Functions ===========
driver_API <- function(year){
  #This function gets the raw data from the driverStandings API for a specific year and returns a list
  api_reponse <- paste0("http://ergast.com/api/f1/", year, "/driverStandings.json?limit=1000")
  raw_data <- jsonlite::fromJSON(api_reponse)
  return(raw_data) #returns a list
}

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

driver_winner <- function(year){
  # This function returns the driver who won most races in that year, their number of wins and their number of points 
  
  #Fetch data
  df_driver <- driver_get_df(year)
  max_win <- df_driver[which.max(df_driver$wins), ]
  max_point <- df_driver[which.max(df_driver$points), ]
  
  #Print in console for package testing
  cat(max_win$driver, "won the most race which is", max_win$wins, "races with", max_win$points, "points in",max_win$year)
  
  #Return the answer
  return_vector <- c(max_win$driver, max_win$wins, max_win$points)
  return(return_vector)
}

driver_btw_years <- function(from,to){
  # This function creates and return a data.frame for the driverStandings API for the years between 'from' and 'to'
  df <- driver_get_df(from)
  for (x in (from+1):to) {
    new_year <- driver_get_df(x)
    df <- rbind(df, new_year )
  }
  return(df)
}

driver_winner_period <- function(from, to) {
  # Gives the driver with the most wins during a given period [from-to]
  
  # Fetch data
  data <- driver_btw_years(from, to)
  
  # Convert to numeric
  data$wins <- as.numeric(data$wins)
  
  # Calculate total wins per driver
  total_wins <- aggregate(wins ~ driver, data, sum)
  
  # Sort the data by total wins in descending order
  total_wins <- total_wins[order(-total_wins$wins), ]
  
  # Print the result for package testing
  cat(total_wins$driver[1], "has the highest number of wins with", total_wins$wins[1], "wins in total between years", from, "and", to)
  
  # return desired data
  tot_win_driv <-  c(total_wins$driver[1],total_wins$wins[1])
  return(tot_win_driv)
}


#=== Constructor API Functions ==========
const_API <- function(year){
  # This function gets the raw data from the constructorStandings API for a specific year and returns a list
  api_reponse <- paste0("http://ergast.com/api/f1/", year, "/constructorStandings.json?limit=10000")
  raw_data <- jsonlite::fromJSON(api_reponse)
  return(raw_data) #returns a list
}

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

const_winner <- function(year){
  # Find the Constructor who won the most races in the given year
  df_const <- const_get_df(year)
  max_win <- df_const[which.max(df_const$wins), ]
  max_point <- df_const[which.max(df_const$points), ]
  
  #Print result for package testing
  cat(max_win$name, "constructer won the most race which is", max_win$wins, "races with", max_win$points, "points in",max_win$year)
  
  #return desired result
  result_vector <- c(max_win$name, max_win$wins, max_win$points)
  return(result_vector)
}

const_btw_years <- function(from,to){
  # This function creates and return a data.frame for the constructorStandings API for the years between 'from' and 'to'
  df <- const_get_df(from)
  for (x in (from+1):to) {
    new_year <- const_get_df(x)
    df <- rbind(df, new_year )
  }
  #Return the combined data.frame
  return(df)
}

const_winner_period <- function(from, to) {
  # Gives the constructor with the most wins during a given period [from-to]
  data <- const_btw_years(from, to)
  data$wins <- as.numeric(data$wins)
  
  # Calculate total wins per constructor
  total_wins <- aggregate(wins ~ name, data, sum)
  
  # Sort the data by total wins in descending order
  total_wins <- total_wins[order(-total_wins$wins), ]
  
  # Print the result for package testing
  cat(total_wins$name[1], "constructor has the highest number of wins with", total_wins$wins[1], "wins in total between years", from, "and", to)
  
  #Return dersired info
  tot_win <-  c(total_wins$name[1],total_wins$wins[1])
  return(tot_win)
}