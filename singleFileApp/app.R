
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
library(raster)

# reading in data (project folder is working directory)
coord <- read_csv("../locationsToCoordinates.csv")
coord <- coord[order(coord$location),] # alphabetize location - coordinate dictionary
validLocations <- read_csv("../allAPs.csv") # aps <-> locations
dukeShape <- read_csv("../dukeShape.txt", col_names = FALSE)

numAPs <- validLocations %>% # number of APs per location
  group_by(location) %>% 
  summarise(num = n())

# splunkData <- read_csv("../eventData.csv")
# 
# # match aps to locations, merge for coordinates
# df <- splunkData[!is.na(splunkData$ap),] # remove observations with no ap
# 
# # Some aps are in splunk data with name, some with number - code below matches location using whichever is available
# nameMatch = which(validLocations$APname %in% df$ap) # find which aps have their name in the data
# numMatch = which(validLocations$APnum %in% df$ap) # find which aps have their number in the data
# validLocations$ap = c(NA) # new "flexible" column to store either name or number
# validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
# validLocations$ap[numMatch] = validLocations$APnum[numMatch]
# 
# validLocations <- merge(coord, validLocations) # link coordinates to locations
# # use the new "flexible" ap variable to merge coordinates onto df
# df <- merge(df, validLocations, by = "ap") # this is the slow step
# write.csv(df, "../mergedData.csv")

df <- read_csv("../mergedData.csv")
df$`_time` <- force_tz(ymd_hms(df$`_time`), "EST")

# draw duke border
p = Polygon(dukeShape)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))

# remove out-of-bounds locations
inBounds <- sapply(1:nrow(coord), function(x) {
  point.in.polygon(coord$long[x], coord$lat[x], unlist(dukeShape[,1]), unlist(dukeShape[,2]))
})
coord <- coord[inBounds == 1,]
coord <- merge.data.frame(coord,numAPs)
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
SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(x=coord[,2], y=coord[,3]))
# tag polygons with location name
SPDF@data$ID = coord$location
sapply(1:length(coord$location), function(x){
  SPDF@polygons[[x]]@ID <- coord$location[x]
  SPDF <<- SPDF
})

# Default coordinates that provide overview of entire campus
defLong <- -78.9272544 # default longitude
defLati <- 36.0042458 # default latitude
zm <- 15 # default zoom level

# Coordinates for West
wLati <- 36.0003456
wLong <- -78.939647
wzm <- 16

# Coordinates for East
eLati <- 36.0063344
eLong <- -78.9154213
ezm <- 17

# Coordinates for Central
cLati <- 36.0030883
cLong <- -78.9258819
czm <- 17


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
timeSteps = c("1hr" = 60*60, "2hr" = 2*60*60, "4 hr" = 4*60*60) # in seconds
# timeSteps = c("4 hr" = 4*60*60)
delay = 1500 # in milliseconds
# ------------------------------

start.time = (min(df$`_time`))
end.time = (max(df$`_time`))

popDensityList <- list()
paletteList <- list()
end.times <- rep(end.time, length(timeSteps))

for(i in 1:length(timeSteps)){
  timeStep <- timeSteps[i]
  # Bin populations, calculate densities at each timestep, and cache for future plotting
  time.windowStart = start.time # time.window for selection
  populationDensities <- NULL
  
  while(end.time > time.windowStart){
    
    # Filter for time interval
    selInt = interval(time.windowStart, time.windowStart + timeStep)
    thisStep <- df %>%
      filter(`_time` %within% selInt)
    
    # Calculate Population Densities
    locationBinnedPop <- data.frame("location" = coord$location, "pop" = c(0))
    # For each location, count the number of unique devices (MAC addresses) that are present during the time time.window.
    locationBinnedPop$pop <- sapply(locationBinnedPop$location, function(x) {length(unique(thisStep$macaddr[thisStep$`location.y` == x]))})
    # Calculate a measure of people / (100 sq meters)
    densities_area <- sapply(1:N, function(x) {100 * locationBinnedPop$pop[x] / (SPDF@polygons[[x]]@area * areaConvert)})
    densities_aps  <- sapply(1:N, function(x) {locationBinnedPop$pop[x] / coord$num[x]})
    densities_both <- sapply(1:N, function(x) {locationBinnedPop$pop[x] / SPDF@polygons[[x]]@area / areaConvert / coord$num[x]})
    info <- c(densities_area, densities_aps, densities_both, locationBinnedPop$pop)
    type <- c(rep(1, N), rep(2, N), rep(3, N), rep(4, N))
    densitiesToSave <- data.frame("location" = locationBinnedPop$location, 
                                  #"pop" = locationBinnedPop$pop, 
                                  "ap_num" = coord$num, 
                                  "info" = info,
                                  "type" = type,
                                  "time.window" = c(time.windowStart))
    populationDensities <- rbind(populationDensities, densitiesToSave)
    end.times[i] <- time.windowStart
    time.windowStart = time.windowStart + timeStep
  }
  
  # setting up for chloropleth
  palette_area <- colorNumeric("YlOrRd", (populationDensities %>% filter(type == 1))$info)
  palette_aps  <- colorNumeric("YlOrRd", (populationDensities %>% filter(type == 2))$info)
  palette_both <- colorNumeric("YlOrRd", (populationDensities %>% filter(type == 3))$info)
  palette_raw  <- colorNumeric("YlOrRd", (populationDensities %>% filter(type == 4))$info)
  
  thisStepPaletteList <- list(palette_area, palette_aps, palette_both, palette_raw)
  
  # Cache these guys away for later
  popDensityList[[i]] <- populationDensities
  paletteList[[i]] <- thisStepPaletteList
}

legendTitles <- c("Population Density (area)",
                  "Population Density (aps)", 
                  "Population Density (both)",
                  "Population (raw count)")

# app user interface
ui <- fluidPage(
  
  titlePanel("Duke Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      # input a time to show temporally close records on map
      selectInput("timeStepSelection", "Time Step", choices = timeSteps, selected = timeSteps[1]),
      uiOutput("ui"),
      selectInput("select", "View:", choices = list("Population Density (area)" = 1, "Population Density (aps)" = 2, 
                                                    "Population Density (both)" = 3, "Population (raw)" = 4), selected = 1),
      radioButtons("focus", "Zoom View", choices = c("All", "East", "Central", "West"), selected = "All")
    ),
    
    mainPanel(
      leafletOutput("map", height = 850) # output the map, should check if height is ok with different screens
    )
  )
)

# app backend
server <- function(input, output, session) {
  
  # list of locations to be included
  include <- reactiveValues(poly = coord$location)
  
  # Seeing if a polygon was clicked and hiding/showing it as needed
  observeEvent(input$map_shape_click, {
    clickedLoc <- input$map_shape_click$'id'
    if(clickedLoc %in% include$poly) {
      include$poly <- include$poly[!include$poly %in% clickedLoc] # taking out of included polygons
    }
    else {
      include$poly <- c(include$poly, clickedLoc) # putting it back into included polygons
      include$poly <- sort(include$poly) # need to sort so that shapes drawn have correct id
    }
  })
  
  # Creates the initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView("map", lng = defLong, lat = defLati, zoom = zm) %>% # sets initial map zoom & center
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) # adds Open Street Map info (otherwise just a gray box)
  })
  
  output$ui <- renderUI({
    # input a time to show temporally close records on map
    sliderInput("time", "Time", min = start.time, max = end.times[which(timeSteps == input$timeStepSelection)],
                value = start.time, animate = animationOptions(interval=delay),
                step = dseconds(input$timeStepSelection))
  })
  
  observe({
    #Filters for records within timeStep of the input time.
    if(is.null(input$time) | is.null(input$timeStepSelection)){
      return()
    }
    populationDensities <- popDensityList[[which(timeSteps == input$timeStepSelection)]]
    if(!any(populationDensities$time.window == input$time)){
      return()
    }
    myPaletteList <- paletteList[[which(timeSteps == input$timeStepSelection)]]
    print(myPaletteList)
    myPalette <- myPaletteList[[as.numeric(input$select)]]
    print(input$select)
    print(myPalette)
    thisStep <- populationDensities %>%
      filter(time.window == input$time) %>% 
      filter(location %in% include$poly) %>%
      filter(type == input$select)
    
    # Setting up for hover tooltips
    labels <- sprintf("<strong>%s</strong><br/ >%g APs<br/ >%g Value",
                      thisStep$location, # location
                      thisStep$ap_num,
                      thisStep$info) %>% # plotted value
      lapply(htmltools::HTML)
    
    # Adds polygons and colors by population density.
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = SPDF[SPDF@data$ID %in% thisStep$location, ], # draws the included polygons
                  layerId = thisStep$location,
                  weight = 1,
                  color = 'black',
                  fillOpacity = .5,
                  fillColor = ~myPalette(thisStep$info),
                  label = labels)
    leafletProxy("map") %>%
      addPolygons(data = SPDF[!SPDF@data$ID %in% thisStep$location, ], # draws the unincluded polygons
                  layerId = coord$location[!coord$location %in% thisStep$location],
                  weight = 1.5,
                  color = 'white',
                  opacity = 0.2,
                  fillColor = ~myPalette(thisStep$info),
                  fillOpacity = 0) 
    leafletProxy("map") %>%
      addLegend(pal = myPalette, 
                values = populationDensities$info,
                position = "topright",
                title = legendTitles[input$select])
    
  })
  
  observe({
    if(input$focus == "West"){
      leafletProxy("map") %>% 
        flyTo(wLong, wLati, wzm)
    }
    if(input$focus == "East"){
      leafletProxy("map") %>% 
        flyTo(eLong, eLati, ezm)
    }
    if(input$focus == "Central"){
      leafletProxy("map") %>% 
        flyTo(cLong, cLati, czm)
    }
    if(input$focus == "All"){
      leafletProxy("map") %>% 
        flyTo(defLong, defLati, zm)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
