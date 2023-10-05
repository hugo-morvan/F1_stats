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
    year <- switch(input$year,
                      driverStandings = driverStandings,
                      constructorStandings = constructorStandings
                      )
  })
  
  output$plot <- renderPlot({
    #Plot the histogram of point per driver per year
    
    
    # Sample data (replace this with your actual data)
    data <- data.frame(
      Driver = c("Driver A", "Driver B", "Driver C", "Driver D", "Driver E"),
      Points = c(10, 15, 8, 12, 20),
      Constructor = c("Team X", "Team Y", "Team X", "Team Z", "Team Z")
    )
    
    # Create a histogram
    ggplot(data, aes(x = Driver, y = Points, fill = Constructor)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_brewer(palette = "Set3") + 
      labs(title = "Bar Chart of Points per Driver",
           x = "Driver",
           y = "Points")
    
  })
  output$table <- renderTable({
    #Present the dataset obtained by the request in a table
    d()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
