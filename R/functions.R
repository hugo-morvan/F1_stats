library(jsonlite)
library(dplyr)

#This function gets the row data for a specific year and returns a list
driver_API <- function(year){
  if (year > 2023 | year < 1958){
    stop('Year should be between 1958 and 2023!')
  }
  if (year%%1!=0){
    stop('Year should be integer')
  }
  api_reponse <- paste0("http://ergast.com/api/f1/", year, "/driverStandings.json?limit=1000")
  raw_data <- fromJSON(api_reponse)
  return(raw_data)
}

# Returns the F1 statistics for a single year
F1_driver_df <- function(year){
  
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

# Returns the driver who won most races in that year
winner_driver <- function(year){
  df_driver <- F1_driver_df(year)
  max_win <- df_driver[which.max(df_driver$wins), ]
  max_point <- df_driver[which.max(df_driver$points), ]
  cat(max_win$driver, "won the most race which is", max_win$wins, "races with", max_win$points, "points in",max_win$year)
}

# Returns the statistics between years
driver_btw_years <- function(from,to){
  df <- F1_driver_df(from)
  for (x in (from+1):to) {
    new_year <- F1_driver_df(x)
    df <- rbind(df, new_year )
  }
  return(df)
}

# Gives the driver with most win in a given interval
winner_driver_interval <- function(from, to){
  data <- driver_btw_years(from, to)
  data$wins <- as.numeric(data$wins)
  
  result_df <- data %>%
    group_by(driver) %>%
    summarise(total_wins = sum(wins)) %>%
    arrange(desc(total_wins))
  cat(result_df$driver[1], "has the highest number of wins with", result_df$total_wins[1], "wins in total between years", from, "and", to)
}

#------------------------------

# This function gets the row data for a specific year and returns a list
const_API <- function(year){
  if (year > 2023 | year < 1958){
    stop('Year should be between 1958 and 2023!')
  }
  if (year%%1!=0){
    stop('Year should be integer')
  }
  api_reponse <- paste0("http://ergast.com/api/f1/", year, "/constructorStandings.json?limit=10000")
  raw_data <- fromJSON(api_reponse)
  return(raw_data)
}

# Return the list from the raw data to desired dataframe format
F1_const_df <- function(year){
  data2 <- const_API(year)
  data_2008 <- as.data.frame(data2$MRData)
  result_2008 <- as.data.frame(data_2008$StandingsTable.StandingsLists.ConstructorStandings)
  
  constructor <- result_2008$Constructor
  constructor_new <- subset(constructor, select = -c(constructorId, url))
  
  result_2008_new <- subset(result_2008, select = -c(position, Constructor, positionText))
  final_2008 <- cbind(result_2008_new, constructor_new)
  final_2008$year <- year
  final_2008 <- final_2008[, c("year","name", "nationality","points" ,"wins")]
  return(final_2008)
}

# Find the constructer who won most race in the given year
winner_const <- function(year){
  df_const <- F1_const_df(year)
  max_win <- df_const[which.max(df_const$wins), ]
  max_point <- df_const[which.max(df_const$points), ]
  cat(max_win$name, "constructer won the most race which is", max_win$wins, "races with", max_win$points, "points in",max_win$year)
}

# Gives the data for a year interval
const_btw_years <- function(from,to){
  df <- F1_const_df(from)
  for (x in (from+1):to) {
    new_year <- F1_const_df(x)
    df <- rbind(df, new_year )
  }
  return(df)
}

# Gives the highest number of wins with its constructer
winner_const_interval <- function(from, to){
  data <- const_btw_years(from, to)
  data$wins <- as.numeric(data$wins)
  
  result_df <- data %>%
    group_by(name) %>%
    summarise(total_wins = sum(wins)) %>%
    arrange(desc(total_wins))
  cat(result_df$name[1], "constructer has the highest number of wins with", result_df$total_wins[1], "wins in total between years", from, "and", to)
}



