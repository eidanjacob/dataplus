# Density map of Duke's campus for public usage.

# Load packages:
library(readr) # for reading csv files
library(leaflet) # for mapping
library(dplyr) # for data wrangling
library(shiny) # for interactive maps
library(deldir) # for voronoi cell calculations
library(sp) # for drawing polygons
library(rgdal) # for drawing polygons
library(lubridate) # for easy handling of times and dates
library(geosphere) # for haversine formula (calculate distance on sphere)
library(raster) # for constructing polygons  
library(mapview) # for saving leaflet map
library(ggplot2) # for building maps

# reading in data (project folder is working directory)
coord <- read_csv("locationsToCoordinates.csv") # locations to coordinates look up table
coord <- coord[order(coord$location),] # alphabetize location - coordinate dictionary
dukeShape <- read_csv("dukeShape.txt", col_names = FALSE) # shape of duke's campus

# List of buildings with floor info
buildings <- list.files("./Buildings")
# File name of data
fname <- "../data/mergedData.csv"
buildingsNamesList <- NULL # vector of names of already calculated maps
buildingsList <- NULL # list of plots
lastUpdate <- 0 # last time the data file was updated
lastUpdateMap <- 0 # last time the maps were updated
eventData <- NULL
first <- TRUE # indicates first run through

# draw duke border
p = Polygon(dukeShape)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))

# remove out-of-bounds locations
inBounds <- sapply(1:nrow(coord), function(x) {
  point.in.polygon(coord$long[x], coord$lat[x], unlist(dukeShape[,1]), unlist(dukeShape[,2]))
})
coord <- coord[inBounds == 1,]
N = nrow(coord)
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
SP <- intersect(SP, sps)
for(x in 1:nrow(coord)){
  SP@polygons[[x]]@ID <- as.character(x)
}
SPeventData <- SpatialPolygonsDataFrame(SP, data=data.frame(x=coord[,2], y=coord[,3]))
# tag polygons with location name
SPeventData@data$ID = coord$location
sapply(1:length(coord$location), function(x){
  SPeventData@polygons[[x]]@ID <- coord$location[x]
  SPeventData <<- SPeventData
})

# Default coordinates that provide overview of entire campus
defLong <- -78.932525 # default longitude
defLati <- 36.002145 # default latitude
zm <- 15 # default zoom level

# Areas of polygons were calculated in original units (degrees). The code below approximates a sq. meter measure to a square degree (In Durham)
p1 <- c(defLong, defLati)
degScale = -3
p2 <- c(defLong + 10 ^ degScale, defLati)
p3 <- c(defLong, defLati + 10 ^ degScale)
# The Haversine formula calculates distances along a spherical surface.
areaConvert = distHaversine(p1, p2) * distHaversine(p1, p3) # = square meters per 10^degScale square degrees (in Durham)
areaConvert = areaConvert / 10^(2 * degScale) # square meters per square degree

# App User Interface
ui <- fluidPage(
  navbarPage("WiFi Map", id = "tabs",
             tabPanel("General Map", 
                      leafletOutput("map", height = 850)
             ),
             tabPanel("Building Map", 
                      plotOutput("build"),
                      actionButton("back", "Back")
             )
  )
)

# App Server
server <- function(input, output, session) {
  
  hideTab("tabs", "Building Map")
  
  observe({
    invalidateLater(10 * 1000) # refresh rate in ms
    currUpdate <- file.info(fname)$ctime[1]
    if(lastUpdate != currUpdate) {
      eventData <<- read_csv(fname)
      eventData$`_time` <<- force_tz(ymd_hms(eventData$`_time`), "EST")
      lastUpdate <<- file.info(fname)$ctime[1]
      # Reset building lists
      buildingsNamesList <<- NULL
      buildingsList <<- NULL
    }
  })
  
  observe({
    
    invalidateLater(10 * 1000) # refresh rate in ms
    currUpdate <- file.info(fname)$ctime[1]
  
    if(currUpdate != lastUpdateMap){
    
  # Drawing map
  output$map <- renderLeaflet({ 
    
    lastUpdateMap <<- currUpdate
    
    # Calculate population density 
    end.time = (max(eventData$`_time`))
    
    # Color palette for polygons
    colorPal <- "Purples"
    
    # Calculate population densities
    locationBinnedPop <- data.frame("location" = coord$location, "pop" = c(0), "dens" = c(0))
    # For each location, count the number of unique devices (MAC addresses) that are present during the time time.window.
    locationBinnedPop$pop <- sapply(locationBinnedPop$location, function(x) {length(unique(eventData$macaddr[eventData$`location.y` == x]))})
    densities_area  <- sapply(1:N, function(x) {100 * locationBinnedPop$pop[x] / (SPeventData@polygons[[x]]@area * areaConvert)})
    locationBinnedPop$dens <- densities_area
    
    # Setting up palette for choropleth
    palette_area <- colorNumeric(colorPal, locationBinnedPop$dens)
    
    # Getting the marker color depending on its population density
    getColor <- function(step) {
      sapply(step$dens, function(val) {
        if(val < summary(step$dens)[[2]]) {
          "green"
        } else if(val > summary(step$dens)[[5]]) {
          "red" 
        } else {
          "orange"
        } 
      })
    }
    # Icon image
    icons <- awesomeIcons(
      icon = 'wifi',
      iconColor = 'black',
      library = 'fa',
      markerColor = getColor(locationBinnedPop)
    )
    
    # Creating map
    m <- leaflet() %>%
      setView("map", lng = defLong, lat = defLati, zoom = zm) %>% # sets initial map zoom & center
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
      addPolygons(data = SPeventData, 
                  layerId = locationBinnedPop$location,
                  group = locationBinnedPop$location,
                  weight = 1,
                  color = "black",
                  fillOpacity = 0.5,
                  fillColor = ~palette_area(densities_area)) %>% 
      addAwesomeMarkers(lng = coord$long, 
                        lat = coord$lat, 
                        group = locationBinnedPop$location,
                        icon = icons,
                        options = markerOptions())
    })
  }
  })
  
  # Polygon or marker clicked
  observeEvent({input$map_shape_click}, { # will also rerun if file changed
    
      clickedGroup <- input$map_shape_click$'group'
        
      invalidateLater(10 * 1000) # refresh rate in ms
      
      
      # Current issue is that if you're viewing a building map and the file updates, the building map will not update.
      
      
      if(clickedGroup %in% buildings) { # If we have building maps for the polygon picked,
        if(!clickedGroup %in% buildingsNamesList){ # and if we don't have a previously stored, current map,
          source(paste0("./Buildings/", clickedGroup, "/", tolower(clickedGroup), ".R")) # then read in file for creating the map,
          # notice that the file name must be lowercase.R in a folder with its Location name as written in the location lookup table
          
          buildingEventData <- eventData %>% # filter event data for those in the correct location,
            filter(location.y == clickedGroup)
          
          buildingMap <- createMap(buildingEventData) # create the map,
          buildingMap <- list(buildingMap)
          
          buildingsNamesList <<- c(buildingsNamesList, clickedGroup) # add to list of buildings that have already been generated,
          buildingsList <<- c(buildingsList, buildingMap) # and save drawn map
        }
        index <- base::match(clickedGroup, buildingsNamesList)
        output$build <- renderPlot(buildingsList[[index]]) # Display map
        showTab("tabs", "Building Map")
        hideTab("tabs", "General Map")
      }
    }, ignoreInit = TRUE) 
  
  # Back button pressed
  observeEvent(input$back, {
    showTab("tabs", "General Map")
    hideTab("tabs", "Building Map")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)