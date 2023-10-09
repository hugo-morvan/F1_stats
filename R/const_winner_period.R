#' Constructor Winner Period
#' @description
#' Gives the constructor with the most wins during a given period [from-to]
#' 
#' @param from the starting year
#' @param to the ending year
#'
#' @return the most successful driver for the given period, in a text format
#' @export
#'
source("const_btw_years.R")
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