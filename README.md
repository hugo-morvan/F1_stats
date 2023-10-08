<!-- badges: start -->
  [![R-CMD-check](https://github.com/hugo-morvan/F1_stats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hugo-morvan/F1_stats/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->


# F1 Results Visualization

This R package contains a Shiny app to visualize F1 results since the year 1958 to this date. The results are extracted from [Ergast Developer API](https://ergast.com/mrd/)




## Authors

- [@hugo-morvan](https://www.github.com/hugo-morvan)
- [@simgeecnr](https://www.github.com/simgeecnr)


## Installation

To install and use this package, you can follow these steps:

1. **Install `devtools` Package (if not already installed):**

   If you haven't already installed the `devtools` package, you can do so using the following command in your R console:

   ```R
   install.packages("devtools")
   ```
2. **Load the `devtools` Library:**
   After installing `devtools`, load the library in your R session:

   ```R
   library("devtools")
   ```
3. **Install Package from Github:**
   Use the install_github function from devtools to install the "F1-stats" package directly from this GitHub repository:

   ```R
   devtools::install_github("hugo-morvan/F1_stats")
   ```
   This command will download and install the package along with its dependencies from this GitHub repository.
4. **Load the Installed Package:**
  Once the installation is complete, load the package into your R session:
   ```R
   library("F1_stats")
   ```
   After loading the package, you can use its functions and features in your R environment.

## Usage
1. **Load and Install Shiny:**
   Ensure that the package shiny is installed and loaded:
   ```R
   library("shiny")
   ```
2. **Launch the Shiny App:**
  To run the shiny app, simply run the following commands :
  ```R
  runGitHub("F1_stats", "hugo-morvan", subdir = "R/")
  ```
3. **Explore the Dataset !**
   The app contains F1 driver and constructor standings since 1958. Have fun exploring its content !


## API Reference

The API used for this project is the [Ergast F1 Standings](http://ergast.com/mrd/methods/standings/) API. 

#### Usage: 
To list the driver or constructor standings after a specific race use the following URLs with the required year and round number:

http://ergast.com/api/f1/2008/5/driverStandings

http://ergast.com/api/f1/2008/5/constructorStandings

To list the driver or constructor standings at the end of a specific season use the following URLs with the required year. If the season hasn’t ended you will get the current standings:

http://ergast.com/api/f1/2008/driverStandings

http://ergast.com/api/f1/2008/constructorStandings

Current standings can always be obtained using the following static URLs:

http://ergast.com/api/f1/current/driverStandings

http://ergast.com/api/f1/current/constructorStandings
