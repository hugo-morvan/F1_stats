#' Constructor Winner
#' @description This function returns the constructor who won most races in that year, their number of wins and their number of points 

#' @param year The year you wish to retrieve the data from
#'
#' @return the answer to your query, in text format
#' @export
#'
#' 
source("const_get_df.R")
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