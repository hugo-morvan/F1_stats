#' Driver Winner
#' @description This function returns the driver who won most races in that year, their number of wins and their number of points 

#' @param year The year you wish to retrieve the data from
#'
#' @return the answer to your query, in text format
#' @export
#'
#' 
source("driver_get_df.R")
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