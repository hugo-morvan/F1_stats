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
  # A function that takes in a year a return a dataframe from the driverStandings dataset
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

F1_const_df <- function(year){
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

# ===== User Interface ==============
ui <- fluidPage(
  
  # Application title
  titlePanel("F1 Standings per year"),
  
  # Sidebar with a slider input for the year
  sidebarLayout(
    sidebarPanel(
      selectInput("year",
                  "Select a Year:",
                  choices = seq.int(1958, 2023),
                  selected = 2010)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Driver Plot", plotOutput("driver_plot")),
                  tabPanel("Constructor Plot", plotOutput("const_plot")),
                  tabPanel("Driver Dataset", tableOutput("driver_table")),
                  tabPanel("Constructor Dataset", tableOutput("const_table")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d <- reactive({
    year <- switch(input$year)
  })
  
  #=== Driver Plot =========================
  output$driver_plot <- renderPlot({
    #Fetch the data
    data_driver <- F1_driver_df(input$year)
    
    # Create a histogram of the points per driver, colored by constructor
    ggplot(data_driver, aes(x = driver, y = as.numeric(points), fill = constructer)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_brewer(palette = "Set3") +
      labs(title = paste("Bar Chart of Points per Driver for the Year", input$year),
           x = "Driver",
           y = "Points") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #=== Constructor Plot =====================
  output$const_plot <- renderPlot({
    #Fetch the data
    data_const <- F1_const_df(input$year)
    
    # Creates a histogram of the points per driver, colored by constructor
    ggplot(data_const, aes(x = name, y = as.numeric(points), fill = nationality)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_brewer(palette = "Set3") + 
      labs(title = paste("Bar Chart of Points per Constructor for the Year", input$year),
           x = "Constructor",
           y = "Points") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #=== Driver Dataframe Table =======================
  output$driver_table <- renderTable({
    #Present the dataset obtained by the request in a table
    output$driver_table <- renderTable({
      F1_driver_df(input$year)
    })
  })
  
  #=== Constructor Dataframe Table =======================
  output$const_table <- renderTable({
    #Present the dataset obtained by the request in a table
    output$const_table <- renderTable({
      F1_const_df(input$year)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
