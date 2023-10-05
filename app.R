#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(jsonlite)

F1_df <- function(year){
  url2 <- paste0("http://ergast.com/api/f1/", year, "/driverStandings.json?limit=1000")
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
  #Return a data.frame
  return(final_2008)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("F1 standings per year"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Select a Year:",
                  value = 2008,
                  min = 1958,
                  max = 2023,
                  step = 1,
                  sep="")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Dataset", tableOutput("table")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d <- reactive({
    year <- switch(input$year)
  })
  
  output$plot <- renderPlot({
    #Plot the histogram of point per driver per year
    
    
    # Sample data (replace this with your actual data)
    data <- F1_df(input$year)
    
    # Create a histogram
    ggplot(data, aes(x = driver, y = points, fill = constructer)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_brewer(palette = "Set3") + 
      labs(title = "Bar Chart of Points per Driver",
           x = "Driver",
           y = "Points") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  output$table <- renderTable({
    #Present the dataset obtained by the request in a table
    output$table <- renderTable({
      F1_df(input$year)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
