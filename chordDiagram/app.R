library(shiny)
library(readr)
library(circlize)
library(migest)
library(dplyr)

ui <- navbarPage("Chord Diagram Generator",
  tabPanel("Diagram",
           sidebarLayout(
             sidebarPanel(
               sliderInput("timeBounds", "Time Interval",
                           min = start.time, max = end.time,
                           value = c(start.time, end.time)),
               uiOutput("chordCheckbox")
             ),
             
             mainPanel(
               plotOutput("circleChart")
             )
           )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$chordCheckbox <- renderUI({
    checkboxGroupInput("chordLocations", "Locations", choices = unique(df$locations))
  })
  
  output$circleChart <- renderPlot({
    selection <- df %<%
      filter(`_time` >= input$timeBounds[1] & `_time` <= input$timeBounds[2]) %<%
      filter(location.y %in% c("Perkins", "WestUnion", "BryanCenter", "Fuqua"))
    
    # Create adjacency list. 
    locs <- unique(selection$location.y)
    adjList = data.frame(from = rep(locs, times = length(locs)),
                         to = rep(locs, each = length(locs)),
                         value = 1,
                         stringsAsFactors = FALSE)
    chordDiagram(adjList)
  })}

# Run the application 
shinyApp(ui = ui, server = server)

