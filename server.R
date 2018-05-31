# Load packages:
# readr, readxl for data frame input
# leaflet for mapping
# dplyr for data wrangling
# deldir for voronoi cell calculations

library(readr)
library(readxl)
library(leaflet)
library(dplyr)
library(shiny)
library(deldir)
library(sp)

# reading in data (project folder is working directory)
me <- read_csv("./me.csv")
coord <- read_excel("./coord.xlsx")

# calculating voronoi cells and converting to polygons to plot on map
tesselation <- deldir(coord$long, coord$lat) # computes cells
# converts to polygon data (adapted from https://rud.is/b/2015/07/26/making-staticinteractive-voronoi-map-layers-in-ggplotleaflet/)
vor_desc <- tile.list(tesselation)
vor_poly <- lapply(1:length(vor_desc), function(i){
  tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
  tmp <- rbind(tmp, tmp[1,])
  Polygons(list(Polygon(tmp)), ID = i)
})

# matches coordinates of aps (when known) to records
df = merge(coord, me, "location")

interval = 3600 # in seconds

# creates the server for the Shiny App
shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    
    # Filters for records within +/-1 interval of the input time.
    dataInput <- df %>% 
      filter(time <= input$time+interval) %>%
      filter(time >= input$time-interval)
    
    # Creates map and adds a marker at each record's coordinates.
    # Eventually we would like this to be shading in regions on map by number of records.
    leaflet() %>%
      addTiles() %>%
      addPolygons(vor_poly) %>%
      addMarkers(dataInput$long, dataInput$lat)
  })
  
})
