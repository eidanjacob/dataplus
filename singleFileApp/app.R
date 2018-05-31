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

interval = 3600 # in seconds
delay = 300 # in milliseconds
start.time = (min(df$time))
end.time = (max(df$time))

ui <- fluidPage(
   
  titlePanel("Duke Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      # input a time to show chronologically nearby records on map
      sliderInput("time", "Time", min = start.time, max = end.time, value = start.time, animate = animationOptions(interval=delay))
    ),
    
    mainPanel(
      leafletOutput("map"),
      leafletOutput("polygonMap")
    )
  )
)

server <- function(input, output) {
   
  output$map <- renderLeaflet({
    
    # Filters for records within +/-1 interval of the input time.
    dataInput <- df %>% 
      filter(time <= input$time+interval) %>%
      filter(time >= input$time-interval)
    
    # Creates map and adds a marker at each location's coordinates. Also draws Voronoi regions.
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = SPDF) %>%
      addMarkers(dataInput$long, dataInput$lat)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
