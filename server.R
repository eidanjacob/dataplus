# Load packages:
# readr, readxl for data frame input
# leaflet for mapping
# dplyr for data wrangling

library(readr)
library(readxl)
library(leaflet)
library(dplyr)
library(shiny)

# reading in data (project folder is working directory)

me <- read_csv("./me.csv")
coord <- read_excel("./coord.xlsx")

# matches coordinates of aps (when known) to records
df = merge(coord, me, "location")

# creates the server for the Shiny App
shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    
    # Filters for records within +/-1 hour of the input time.
    dataInput <- df %>% 
      filter(time <= input$time+3600) %>%
      filter(time >= input$time-3600)
    
    # Creates map and adds a marker at each record's coordinates.
    # Eventually we would like this to be shading in regions on map by number of records.
    leaflet() %>%
      addTiles() %>%
      addMarkers(dataInput$long, dataInput$lat)
  })

  
  
})
