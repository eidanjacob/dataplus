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
splunkdf <- splunkData[!is.na(splunkData$ap),] # remove observations with no ap

# Some aps are in splunk data with name, some with number - code below matches location using whichever is available
nameMatch = which(validLocations$APname %in% splunkdf$ap) # find which aps have their name in the data
numMatch = which(validLocations$APnum %in% splunkdf$ap) # find which aps have their number in the data
validLocations$ap = c(NA) # new "flexible" column to store either name or number
validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
validLocations$ap[numMatch] = validLocations$APnum[numMatch]

validLocations <- merge(coord, validLocations) # link coordinates to locations
# use the new "flexible" ap variable to merge coordinates onto splunkdf
splunkdf <- merge(splunkdf, validLocations, by = "ap") # this is the slow step

start.time = (min(splunkdf$`_time`))
end.time = (max(splunkdf$`_time`))

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
})

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

# ------------------------------
# Mess with these numbers if you want.
timeSteps = c("3 hr" = 3*60*60, "4 hr" = 4*60*60) # in seconds
delay = 750 # in milliseconds
# ------------------------------

popDensityList <- list()
paletteList <- NULL

for(i in 1:length(timeSteps)){
  timeStep <- timeSteps[i]
  # Bin populations, calculate densities at each timestep, and cache for future plotting
  time.windowStart = start.time # time.window for selection
  populationDensities <- NULL
  
  while(end.time > time.windowStart){
    
    # Filter for time interval
    selInt = interval(time.windowStart, time.windowStart + timeStep)
    thisStep <- splunkdf %>%
      filter(`_time` %within% selInt)
    
    # Calculate Population Densities
    locationBinnedPop <- data.frame("location" = coord$location, "pop" = c(0))
    # For each location, count the number of unique devices (MAC addresses) that are present during the time time.window.
    locationBinnedPop$pop <- sapply(locationBinnedPop$location, function(x) {length(unique(thisStep$macaddr[thisStep$`location.y` == x]))})
    # Calculate a measure of people / (100 sq meters) 
    densities <- sapply(1:nrow(locationBinnedPop), function(x) {100 * locationBinnedPop$pop[x] / (SPDF@polygons[[x]]@area * areaConvert)})
    densitiesToSave <- data.frame("locations" = locationBinnedPop$location, "density" = densities, "time.window" = c(time.windowStart))
    populationDensities <- rbind(populationDensities, densitiesToSave)
    time.windowStart = time.windowStart + timeStep
  }
  
  # setting up for chloropleth
  palette <- colorNumeric("YlOrRd", populationDensities$density)
  
  # Cache these guys away for later
  popDensityList[[i]] <- populationDensities
  paletteList <- c(paletteList, palette)
}

# app user interface
ui <- fluidPage(
  
  titlePanel("Duke Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      
      # input a timeStep to choose time resolution
      selectInput("timeStepSelection", "Step Size", choices = timeSteps, selected = timeSteps[1]),
      
      # output the slider
      uiOutput("ui")
      
    ),
    
    mainPanel(
      leafletOutput("map", height = 850) # output the map, should check if height is ok with different screens
    )
  )
)

# app backend
server <- function(input, output, session){
  
  # Creates the initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView("map", lng = defLong, lat = defLati, zoom = zm) %>% # sets initial map zoom & center
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) # adds Open Street Map info (otherwise just a gray box)
  })
  
  output$ui <- renderUI({
    # input a time to show temporally close records on map
    sliderInput("time", "Time", min = start.time, max = end.time,
                value = start.time, animate = animationOptions(interval=delay),
                step = dseconds(input$timeStepSelection))
  })
  
  observe({
  
    #Filters for records within timeStep of the input time.
    populationDensities <- popDensityList[[which(timeSteps == input$timeStepSelection)]]
    if(is.null(input$time)){
      return()
    }
    thisStep <- populationDensities %>%
      filter(time.window == input$time)
    
    # Adds polygons and colors by population density.
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = SPDF[SPDF@data$ID, ],
                  weight = 2,
                  fillOpacity = .5,
                  fillColor = ~palette(thisStep$density)) %>%
      addLegend(pal = palette, values = thisStep$density)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)