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

F1_driver_df <- function(year){
  api_reponse <- paste0("http://ergast.com/api/f1/", year, "/driverStandings.json?limit=1000")
  raw_data <- fromJSON(api_reponse)
  
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
  final <- final[, c("position", "points", "wins", "driver", "driver nationality", "constructer","constructer nationality")]
  #Return a data.frame
  return(final)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("F1 standings per year"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("year",
                  "Select a Year:",
                  choices = seq.int(1958, 2023),
                  selected = 2010)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Driver Plot", plotOutput("driver_plot")),
                  tabPanel("Dataset", tableOutput("table")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d <- reactive({
    year <- switch(input$year)
  })
  
  output$driver_plot <- renderPlot({
    #Plot the histogram of point per driver per year
    
    #Fetch the data
    data <- F1_driver_df(input$year)
    
    
    # Create a histogram of the points per driver, colored by constructor
    ggplot(data, aes(x = driver, y = as.numeric(points), fill = constructer)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_brewer(palette = "Set3") +  # Use a color palette (Set3 in this case) 
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
