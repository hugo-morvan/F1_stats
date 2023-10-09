#' Driver Winner Period
#' @description
#' Gives the driver with the most wins during a given period [from-to]
#' 
#' @param from the starting year
#' @param to the ending year
#'
#' @return the most successful driver for the given period, in a text format
#' @export
#'
source("driver_btw_years.R")
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
