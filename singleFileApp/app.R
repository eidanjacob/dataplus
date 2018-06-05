# Load packages:
# readr, readxl for data frame input
# leaflet for mapping
# dplyr for data wrangling
# Shiny for interactive apps
# deldir for voronoi cell calculations
# sp, rgdal for drawing polygons
# lubridate for easy handling of times and dates
# geosphere for haversine formula (calculate distance on sphere)

library(readr)
library(readxl)
library(leaflet)
library(dplyr)
library(shiny)
library(deldir)
library(sp)
library(rgdal)
library(lubridate)
library(geosphere)

# reading in data (project folder is working directory)
coord <- read_csv("../locationsToCoordinates.csv") # locations <-> coordinates
coord <- coord[order(coord$location),] # alphabetize location - coordinate dictionary
splunkData <- read_csv("../eventData.csv") 
validLocations <- read_csv("../locationsValid", col_types = cols(X1 = col_skip())) # aps <-> locations

# match aps to locations, merge for coordinates
df <- splunkData[!is.na(splunkData$ap),] # remove observations with no ap

# Some aps are in splunk data with name, some with number - code below matches location using whichever is available
nameMatch = which(validLocations$APname %in% df$ap) # find which aps have their name in the data
numMatch = which(validLocations$APnum %in% df$ap) # find which aps have their number in the data
validLocations$ap = c(NA) # new "flexible" column to store either name or number
validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
validLocations$ap[numMatch] = validLocations$APnum[numMatch]

validLocations <- merge(coord, validLocations) # link coordinates to locations
# use the new "flexible" ap variable to merge coordinates onto df
df <- merge(df, validLocations, by = "ap") # this is the slow step

# calculating voronoi cells and converting to polygons to plot on map
z <- deldir(coord$long, coord$lat) # computes cells
# convert cell info to spatial data frame (polygons)
w <- tile.list(z)
polys <- vector(mode="list", length=length(w))
for (i in seq(along=polys)) {
  pcrds <- cbind(w[[i]]$x, w[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1,])
  polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
}
SP <- SpatialPolygons(polys)
SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(x=coord[,2], y=coord[,3]))
# tag polygons with location name
SPDF@data$ID = coord$location
sapply(1:length(coord$location), function(x){
  SPDF@polygons[[x]]@ID <- coord$location[x]
  SPDF <<- SPDF
  }
)

# ------------------------------
# Mess with these numbers if you want.
timeStep = 60 * 60 # in seconds
delay = 500 # in milliseconds
# ------------------------------

start.time = (min(df$`_time`))
end.time = (max(df$`_time`))

# app user interface
ui <- fluidPage(
  
  titlePanel("Duke Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      # input a time to show temporally close records on map
      sliderInput("time", "Time", min = start.time, max = end.time,
                  value = start.time, animate = animationOptions(interval=delay),
                  step = timeStep),
      checkboxGroupInput("include", "Locations", choices = coord$location, selected = coord$location) # select polygons to display (would like to remove this and allow user to click on polygon itself instead)
    ),
    
    mainPanel(
      leafletOutput("map", height = 850) # output the map, should check if height is ok with different screens
    )
  )
)

# app backend
server <- function(input, output) {
  
  # Default coordinates that provide overview of entire campus
  defLong <- -78.9284148 # -78.9397541 W Campus
  defLati <- 36.0020571 # 36.0017932 W Campus
  zm <- 14 # default zoom level
  # Areas of polygons were calculated in original units (degrees). The code below approximates a sq. meter measure to a square degree (In Durham)
  p1 <- c(defLong, defLati)
  degScale = -3
  p2 <- c(defLong + 10 ^ degScale, defLati)
  p3 <- c(defLong, defLati + 10 ^ degScale)
  # The Haversine formula calculates distances along a spherical surface.
  areaConvert = distHaversine(p1, p2) * distHaversine(p1, p3) # = square meters per 10^degScale square degrees (in Durham)
  areaConvert = areaConvert / 10^(2 * degScale) # square meters per square degree

  # Creates the initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView("map", lng = defLong, lat = defLati, zoom = zm) %>% # sets initial map zoom & center
      addTiles() # adds Open Street Map info (otherwise just a gray box)
    
  })
  
  observe({
    #Filters for records within +/-1 timeStep of the input time.
    selInt = interval(input$time - timeStep, input$time + timeStep) # window for selection
    dataInput <- df %>%
      filter(`_time` %within% selInt) %>% # remove out-of-time data
      filter(`location.y` %in% input$include) # remove data from deselected locations
    
    # Calculate Population Densities
    locationBinnedPop <- data.frame("location" = coord$location, "pop" = c(0))
    # For each location, count the number of unique devices (MAC addresses) that are present during the time window.
    locationBinnedPop$pop <- sapply(locationBinnedPop$location, function(x) {length(unique(dataInput$macaddr[dataInput$`location.y` == x]))})
    # Calculate a measure of people / (100 sq meters) 
    densities <- sapply(1:nrow(locationBinnedPop), function(x) {100 * locationBinnedPop$pop[x] / (SPDF@polygons[[x]]@area * areaConvert)})
    
    # setting up for chloropleth
    palette <- colorNumeric("YlOrRd", densities)
    
    # Adds polygons and colors by population density.
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