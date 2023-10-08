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

# ===== User Interface ==============
ui <- fluidPage(
  
  # Application title
  titlePanel("F1 Standings per year"),
  
  # Sidebar with a slider input for the year
  mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Driver Plot",
                           selectInput("year", 
                                       "Select a Year:",
                                       choices = seq.int(1958, 2023),
                                       selected = 2010),
                           plotOutput("driver_plot")),
                  tabPanel("Constructor Plot",
                           selectInput("year", 
                                       "Select a Year:",
                                       choices = seq.int(1958, 2023),
                                       selected = 2010),
                           plotOutput("const_plot")),
                  tabPanel("Driver Dataset",
                           selectInput("year", 
                                       "Select a Year:",
                                       choices = seq.int(1958, 2023),
                                       selected = 2010),
                           tableOutput("driver_table")),
                  tabPanel("Constructor Dataset",
                           selectInput("year", 
                                       "Select a Year:",
                                       choices = seq.int(1958, 2023),
                                       selected = 2010),
                           tableOutput("const_table")),
                  tabPanel("Best Driver/Constructor by Period",
                           sliderInput("range", "Select a Period:",
                                       min = 1958, max = 2023, value = c(2008,2010), sep=""),
                           textOutput("period_best_driver")),
                  tabPanel("Best Driver/Constructor by Year",
                           selectInput("year_text", 
                                       "Select a Year:",
                                       choices = seq.int(1958, 2023),
                                       selected = 2010),
                           textOutput("year_best")
                           ))
                  )
    
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d <- reactive({
    year <- switch(input$year)
  })
  a <- reactive({
    year_text <- switch(input$year_text)
  })
  my_range <- reactive({
    cbind(input$range[1],input$range[2])
  })
  
  source("R/functions.R")
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) #45° tilted legend
  })
  
  #=== Driver Dataframe Table =======================
  output$driver_table <- renderTable({
    #Present the dataset obtained by the request in a table
    F1_driver_df(input$year)
  })
  
  #=== Constructor Dataframe Table =======================
  output$const_table <- renderTable({
    #Present the dataset obtained by the request in a table
    F1_const_df(input$year)
  })
  #=== Period Best Driver============================
  output$period_best_driver <- renderText({
    
    #Best driver
    tot_win_driver <- winner_driver_interval(input$range[1],input$range[2])
    paste(tot_win_driver[1], "has the highest number of wins with", tot_win_driver[2], "wins in total between years", input$range[1], "and", input$range[2])
    #Best constructor
    tot_win_const <- winner_const_interval(input$range[1],input$range[2])
    paste(tot_win_driver[1], "has the highest number of wins with", tot_win_driver[2], "wins in total between years", input$range[1], "and", input$range[2],".\n \n",tot_win_const[1], "constructor has the highest number of wins with", tot_win_const[2], "wins in total between years", input$range[1], "and", input$range[2],".")
    
    })
  #=== Year Best Driver============================
  output$year_best <- renderText({
    
    driver <- winner_driver(input$year_text)
    const <- winner_const(input$year_text)
    
    paste("Driver", driver[1], "has the highest number of wins with", driver[2], "wins in the year", input$year_text, "with", driver[3], "points.",
          "\n \n",
          "Constructor", const[1], "has the highest number of wins with", const[2], "wins in the year", input$year_text, "with", const[3], "points",".")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
