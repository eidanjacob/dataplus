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
library(rgdal)

# reading in data (project folder is working directory)
me <- read_csv("../me.csv")
coord <- read_excel("../coord.xlsx")

# matches coordinates of aps (when known) to records
df = merge(coord, me, "location")

coord <- unique(coord[,c(2,3)])

# calculating voronoi cells and converting to polygons to plot on map
z <- deldir(coord$long, coord$lat) # computes cells
w <- tile.list(z)
polys <- vector(mode="list", length=length(w))
for (i in seq(along=polys)) {
  pcrds <- cbind(w[[i]]$x, w[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1,])
  polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
}
SP <- SpatialPolygons(polys)
SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(x=coord[,1], y=coord[,2], 
                                                     row.names=sapply(slot(SP, "polygons"), function(x) slot(x, "ID"))))

#dukePolygons <- spTransform(SPDF, CRS("+init=epsg:4326"))

# ------------------------------
# Mess with these numbers if you want.

interval = 60 # in seconds
delay = 30 # in milliseconds

# ------------------------------


start.time = (min(df$time))
end.time = (max(df$time))

ui <- fluidPage(
   
  titlePanel("Duke Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      # input a time to show chronologically nearby records on map
      sliderInput("time", "Time", min = start.time, max = end.time,
                  value = start.time, animate = animationOptions(interval=delay),
                  step = 60)
),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  
  # Default coordinates that provide overview of entire campus
  defLong <- -78.9284148 # -78.9397541 W Campus
  defLati <- 36.0020571 # 36.0017932 W Campus
  zm <- 14


  # Creates the initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView("map", lng = defLong, lat = defLati,zoom = zm) %>% # fixes initial map zoom
      addTiles() %>%
      addPolygons(data = SPDF)
  })
  
  observe({
    # Filters for records within +/-1 interval of the input time.
    dataInput <- df %>% 
      filter(time <= input$time+interval) %>%
      filter(time >= input$time-interval)
    
    
    # Adds a marker at each record's coordinates.
    leafletProxy("map") %>% 
      clearMarkers() %>%
      addMarkers(dataInput$long, dataInput$lat) 
    
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
