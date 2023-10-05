F1_constructor_df <- function(year){
  url2 <- paste0("http://ergast.com/api/f1/", year, "/constructorStandings.json?limit=10000")
  data2 <- fromJSON(url2)
  
  data_2008 <- as.data.frame(data2$MRData)
  result_2008 <- as.data.frame(data_2008$StandingsTable.StandingsLists.ConstructorStandings)
  
  constructor <- result_2008$Constructor
  constructor_new <- subset(constructor, select = -c(constructorId, url))
  
  result_2008_new <- subset(result_2008, select = -c(position, Constructor, positionText))
  final_2008 <- cbind(result_2008_new, constructor_new)
  
  final_2008 <- final_2008[, c("name", "nationality","points" ,"wins")]
  return(final_2008)
}