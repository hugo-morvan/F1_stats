library(testthat)
#source("your_script.R")
#context('function')

# 1) Check whether input is integer or not
test_that("Wrong input throws an error.", {
  expect_error(F1_driver_df("100"))  
  expect_error(F1_driver_df(TRUE))
  expect_error(F1_driver_df(2.8))  
})

# 2) Checking whether output is dataframe
test_that("F1_driver_df returns a DataFrame", {
  result <- F1_driver_df(2005)  
  expect_is(result, "data.frame",
            info = "The result should be of class 'data.frame'.")
})

# 3)
test_that("API function returns a list", {
  result <- driver_API(2005)  
  expect_is(result, "list",
            info = "The result should be of class 'list'.")
})

# 4)
test_that("driver_btw_years returns a DataFrame", {
  result <- driver_btw_years(2005, 2007)  
  expect_is(result, "data.frame",
            info = "The result should be of class 'data.frame'.")
})

# 5) Check the column names
test_that("DataFrame has expected column names", {
  df <- F1_driver_df(2005)  
  expected_column_names <- c("year", "position", "points", "wins", "driver", "driver nationality", "constructer", "constructer nationality")  
  actual_column_names <- names(df)
  expect_equal(actual_column_names, expected_column_names,
               info = "The column names of the DataFrame should match the expected names.")
})

# 6)
test_that("DataFrame has expected column names", {
  df <- F1_const_df(2005)  
  expected_column_names <- c("year","name", "nationality", "points", "wins")  
  actual_column_names <- names(df)
  expect_equal(actual_column_names, expected_column_names,
               info = "The column names of the DataFrame should match the expected names.")
})

# 7) check the winner result of driver for a specific year
test_that("winner_const() method works", {
  expect_output(winner_driver(2005), "Fernando Alonso won the most race which is 7 races with 133 points in 2005")
  expect_output(winner_driver(1972), "Emerson Fittipaldi won the most race which is 5 races with 61 points in 1972")
  expect_output(winner_driver(2023), "Max Verstappen won the most race which is 13 races with 400 points in 2023")
})

# 8) check the name of the driver who drive most between years a and b
test_that("winner_driver_interval() method works", {
  expect_output(winner_driver_interval(2020,2023), "Max Verstappen has the highest number of wins with 40 wins in total between years 2020 and 2023")
  expect_output(winner_driver_interval(1975, 1980), "Niki Lauda has the highest number of wins with 15 wins in total between years 1975 and 1980")
  expect_output(winner_driver_interval(2015, 2018), "Lewis Hamilton has the highest number of wins with 40 wins in total between years 2015 and 2018")
})


# 9) check winner constructer for a specific year
test_that("winner_const() method works", {
  expect_output(winner_const(2005), "McLaren constructer won the most race which is 10 races with 182 points in 2005")
  expect_output(winner_const(1972), "Team Lotus constructer won the most race which is 5 races with 61 points in 1972")
  expect_output(winner_const(2023), "Red Bull constructer won the most race which is 15 races with 623 points in 2023")
})

# 10) check the winner constructer for year interval
test_that("winner_const_interval() method works", {
  expect_output(winner_const_interval(2010, 2015), "Red Bull constructer has the highest number of wins with 44 wins in total between years 2010 and 2015")
  expect_output(winner_const_interval(1960, 1980), "Ferrari constructer has the highest number of wins with 50 wins in total between years 1960 and 1980")
  expect_output(winner_const_interval(2020, 2023), "Red Bull constructer has the highest number of wins with 45 wins in total between years 2020 and 2023")
})

# 11) ??
test_that("The given input is a function", {
  expect_is(F1_driver_df, class = "function",
            info = "Object 'F1_driver_df' is not a function.")
})



