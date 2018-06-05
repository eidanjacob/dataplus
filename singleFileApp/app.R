# Load packages:
# readr, readxl for data frame input
# leaflet for mapping
# dplyr for data wrangling
# deldir for voronoi cell calculations

library(readr)
library(leaflet)
library(dplyr)
library(shiny)
library(deldir)
library(sp)
library(rgdal)
library(readxl)

# reading in data (project folder is working directory)
coord <- read_csv("../locationsToCoordinates.csv")
coord <- coord[order(coord$location),]
splunkData <- read_csv("../eventData.csv")
validLocations <- read_csv("../locationsValid", col_types = cols(X1 = col_skip()))

# match aps to locations, merge for coordinates
df <- splunkData[!is.na(splunkData$ap),]
df <- df[1:1000, ]

nameMatch = which(validLocations$APname %in% df$ap)
numMatch = which(validLocations$APnum %in% df$ap)
validLocations$ap = c(NA)
validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
validLocations$ap[numMatch] = validLocations$APnum[numMatch]

validLocations <- merge(coord, validLocations)
df <- merge(df, validLocations, by = "ap") # this is taking a while :|

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
SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(x=coord[,2], y=coord[,3]))
SPDF@data$ID = coord$location
lapply(1:length(coord$location), function(x){SPDF@polygons[[x]]@ID <- coord$location[x]})

# ------------------------------
# Mess with these numbers if you want.
interval = 600 # in seconds
delay = 30 # in milliseconds
# ------------------------------

start.time = (min(df$`_time`))
end.time = (max(df$`_time`))

ui <- fluidPage(
   
  titlePanel("Duke Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      # input a time to show chronologically nearby records on map
      sliderInput("time", "Time", min = start.time, max = end.time,
                  value = start.time, animate = animationOptions(interval=delay),
                  step = 60),
      checkboxGroupInput("include", "Locations", choices = coord$location, selected = coord$location)
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
      setView("map", lng = defLong, lat = defLati, zoom = zm) %>% # fixes initial map zoom
      addTiles()
    
  })
  
  observe({
    #Filters for records within +/-1 interval of the input time.
    dataInput <- df %>%
      filter(`_time` <= input$time+interval) %>%
      filter(`_time` >= input$time-interval) %>%
      filter(`location.y` %in% input$include)
    
    # Calculate Population Densities
    locationBinnedPop <- data.frame("location" = coord$location, "pop" = c(0))
    locationBinnedPop$pop <- sapply(locationBinnedPop$location, function(x) {length(unique(df$macaddr[df$`location.y` == x]))})
    densities <- sapply(1:nrow(locationBinnedPop), function(x) {locationBinnedPop$pop[x] / SPDF@polygons[[x]]@area})
    
    # setting up for chloropleth
    palette <- colorNumeric("YlOrRd", densities)

    # Adds polygons.
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = SPDF[SPDF@data$ID %in% input$include, ],
                  weight = 2,
                  fillOpacity = .5,
                  fillColor = ~palette(densities[which(coord$location %in% input$include)])) %>%
      addLegend(pal = palette, values = densities)

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
