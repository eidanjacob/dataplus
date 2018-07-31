# fromLoc toLoc app viz with lines
# Note: Will crash if you submit too many times.

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

source("../functions.R")

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
timeSteps = c("1hr" = 60*60, "2hr" = 2*60*60, "4hr" = 4*60*60) # in seconds
# timeSteps = c("4 hr" = 4*60*60)
delay = 2700 # in milliseconds
# ------------------------------

start.time = (min(df$`_time`))
end.time = (max(df$`_time`))

# global variables to be used later
currMacs <- NULL 
numOnMap <- 0 

popDensityList <- list()
paletteList <- list()
macsToLocList <- list()

end.times <- rep(end.time, length(timeSteps))

colorPal <- "Purples"

i <- 1
timeStep <- timeSteps[i]
# Bin populations, calculate densities at each timestep, and cache for future plotting
time.windowStart = start.time # time.window for selection
populationDensities <- NULL
macsToLoc <- NULL

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
  info <- c(densities_area)
  type <- c(rep(1, N))
  densitiesToSave <- data.frame("location" = locationBinnedPop$location,
                                #"pop" = locationBinnedPop$pop,
                                "ap_num" = coord$num,
                                "info" = info,
                                "type" = type,
                                "time.window" = c(time.windowStart))
  populationDensities <- rbind(populationDensities, densitiesToSave)
  
  # For each macaddr, keep track of where it currently is
  macs <- data.frame("macaddr" = thisStep$macaddr,
                     "location" = thisStep$location.y,
                     "long" = thisStep$long,
                     "lat" = thisStep$lat,
                     "time.window" = c(time.windowStart),
                     "realTime" = c(thisStep$`_time`))
  macs <- macs[order(macs$realTime), ]
  macsToLoc <- rbind(macsToLoc, macs)
  
  end.times[i] <- time.windowStart
  time.windowStart = time.windowStart + timeStep
  
  
  # setting up for chloropleth
  palette_area <- colorNumeric(colorPal, (populationDensities %>% filter(type == 1))$info)
  
  thisStepPaletteList <- list(palette_area)
  
  # Cache these guys away for later
  popDensityList[[i]] <- populationDensities
  paletteList[[i]] <- thisStepPaletteList
  macsToLocList[[i]] <- macsToLoc
}

legendTitles <- c("Population Density (area)")

# app user interface
ui <- fluidPage(
  
  titlePanel("Duke Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      # input a time to show temporally close records on map
      selectInput("timeStepSelection", "Time Step", choices = timeSteps, selected = timeSteps[1]),
      uiOutput("ui"),
      radioButtons("focus", "Zoom View", choices = c("All", "East", "Central", "West"), inline = TRUE, selected = "All"),
      
      p("Please see documentation for information on the meaning of the inputs. Scroll down for information on the map's interactability."),
      
      textInput("from", "From location: ", value = "Perkins"),
      textInput("to", "To location: ", value = "WestUnion"),
      numericInput("num", "Number of devices: ", min = 0, max = length(unique(df$macaddr)), value = 200),
      actionButton("submit", "Submit"), # must press submit button and have viz flow checked to enable flow viz
      p(),
      textOutput("submittedStuff"),
      textOutput("mapDetails"),
      p(),
      
      sliderInput("fromInte", "From interval", min = 0, max = 60 * 45, step = 300, value = 60 * 10),
      sliderInput("toInte", "To interval", min = 0, max = 60 * 45, step = 300, value = 60 * 5),
      sliderInput("betweenInte", "Between interval", min = 0, max = 60 * 45, step = 300, value = 60 * 5),
      sliderInput("distInte", "Location Interval", min = 1, max = 20, step = 1, value = 2),
      
      p("Hovering over the movement lines shows each macaddr and their path."),
      p("Clicking on the movement line sends that particular macaddr's information into both 'Macaddr's Locations' tabs"),
      p("The general table is the dataframe of all events relevant to the parameters given."),
      p("Macaddr's Locations is the general table filtered for only the macaddr that was just clicked."),
      p("Macaddr's Locations Truncated shows the amount of time it spent at each location for the entire time interval.")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", leafletOutput("map", height = 850)), # output the map, should check if height is ok with different screens
                  tabPanel("General Table", dataTableOutput("tableGen")),
                  tabPanel("Macaddr's Locations", dataTableOutput("tableMac")),
                  tabPanel("Macaddr's Locations Truncated", dataTableOutput("tableMacTrun"))
      )
    )
  )
)

# app backend
server <- function(input, output, session) {
  
  # Colors for movement lines
  from <- 'red'
  to <- 'blue'
  stationary <- 'green'
  highlight <- 'black'
  
  borderInclude <- 'black'
  
  flowOp <- 0.1
  
  locationsData <<- NULL
  
  observeEvent(input$map_shape_click, {
    clickedGroup <- input$map_shape_click$'group'
    clickedID <- input$map_shape_click$'id'
    if(clickedGroup == "severals") { # Line clicked when visualizing flow
      
      tempCurrMacs <- currMacs %>% 
        filter(time.window >= input$time[[1]]) %>% 
        filter(time.window <= input$time[[2]])
      
      macDF <- findIndex(clickedID, tempCurrMacs, input$from, input$to, input$fromInte, input$toInte, input$betweenInte, input$distInte)
      
      output$tableMac <- renderDataTable(macDF$macsTime) # view locations that passed the initial time filtering
      output$tableMacTrun <- renderDataTable(macDF$timeStayed) # view movement data
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
                value = c(start.time, end.times[which(timeSteps == input$timeStepSelection)]), animate = animationOptions(interval=delay),
                step = dseconds(input$timeStepSelection))
  })
  
  observe({
    #Filters for records within timeStep of the input time.
    if(is.null(input$time[[1]]) | is.null(input$timeStepSelection)){
      return()
    }
    populationDensities <- popDensityList[[which(timeSteps == input$timeStepSelection)]]
    currMacs <<- macsToLocList[[which(timeSteps == input$timeStepSelection)]]
    if(!any(populationDensities$time.window == input$time[[1]])){
      return()
    }
    
    thisStep <- populationDensities %>%
      filter(time.window == input$time[[1]]) %>% 
      filter(type == as.numeric(1))
    
    myPaletteList <- paletteList[[unname(which(timeSteps == input$timeStepSelection))]]
    myPalette <- myPaletteList[[as.numeric(1)]]
    # Setting up for hover tooltips
    labels <- sprintf("<strong>%s</strong><br/ >%g APs<br/ >%g value",
                      thisStep$location, # location
                      thisStep$ap_num,
                      thisStep$info) %>% # plotted value
      lapply(htmltools::HTML)
    
    # Adds polygons and colors by population density.
    leafletProxy("map") %>%
      clearGroup("shapes") %>%
      clearControls() %>%
      addPolygons(data = SPDF[SPDF@data$ID, ], 
                  layerId = thisStep$location,
                  group = "shapes",
                  weight = 1,
                  color = borderInclude,
                  fillOpacity = .5,
                  fillColor = ~myPalette(thisStep$info),
                  label = labels)
    legendVals <- (populationDensities %>% filter(type == as.numeric(1)))$info
    leafletProxy("map") %>%
      addLegend(pal = myPalette, 
                values = legendVals,
                position = "topright",
                title = legendTitles[as.numeric(1)])
    
  })
  
  # Visualizing the flow of people
  observeEvent({input$submit} ,{
    withProgress(message = "Loading..." , {
      initSubmit <- paste("Calculating with: \n fromInte", input$fromInte, "toInte", input$toInte, "betweenInte", input$betweenInte, "distInte", input$distInte, "\n",
                          "number of devices", input$num, "fromLoc", input$from, "toLoc", input$to,
                          "time interval", as.character(input$time[[1]]), "to", as.character(input$time[[2]]), "\n")
      output$submittedStuff <- renderText(initSubmit)
      
      gc() # Garbage collector! Prevents overuse of memory and crashes!
      
      macsTime <- currMacs %>% 
        filter(time.window >= input$time[[1]]) %>% # this is so that you only view events that happen within the time interval
        filter(time.window <= input$time[[2]]) 
      if(input$from != "" & input$to != "") {
        uniqMacs <- macsTime %>% # filtering for macs that have visited the to, from locations
          filter(location %in% c(input$from, input$to))
      } else {
        uniqMacs <- macsTime
      }
      uniqMacs <- unique(uniqMacs$macaddr) 
      uniqMacs <- as.character(uniqMacs)
      
      leafletProxy("map") %>% 
        clearGroup("severals") 
      
      numOnMap <<- 0
      locationsData <<- NULL
      
      # looping through each macaddr to determine its movement
      for(i in 1:length(uniqMacs)) { 
        if(i == as.numeric(input$num)) { # to prevent stuff from crashing
          break
        }
        incProgress(amount = 1/as.numeric(input$num))
        
        macDF <- findIndex(uniqMacs[[i]], macsTime, input$from, input$to, input$fromInte, input$toInte, input$betweenInte, input$distInte, numOnMap)
        if(is.null(macDF)) {
          next
        }
        numOnMap <<- macDF$num
        macDF <- macDF$macsTime
        locationsData <<- rbind(locationsData, macDF) # for viewing in tabs 
        
        macLabels <- sprintf("macaddr: %s",
                             macDF$macaddr) %>%
          lapply(htmltools::HTML)
        
        leafletProxy("map") %>% 
          addPolylines(lng = macDF$long, 
                       lat = macDF$lat,
                       layerId = macDF$macaddr,
                       group = "severals",
                       weight = 2,
                       opacity = flowOp,
                       label = macLabels,
                       color = to,
                       highlightOptions = highlightOptions(
                         weight = 5,
                         color = highlight,
                         fillOpacity = 1,
                         bringToFront = TRUE)) %>% 
          addCircles(lng = macDF$long[1], # red is from
                     lat = macDF$lat[1],
                     group = "severals",
                     radius = 5,
                     weight = 2,
                     opacity = flowOp,
                     color = from) %>% 
          addCircles(lng = macDF$long[length(macDF$long)], # blue is to
                     lat = macDF$lat[length(macDF$lat)],
                     group = "severals",
                     radius = 5,
                     weight = 2,
                     opacity = flowOp,
                     color = to)
      } 
    })
    numSubmit <- paste("Number of devices present:", numOnMap, "\n")
    output$mapDetails <- renderText(numSubmit)
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
  
  observeEvent(input$submit ,{
    output$tableGen <- renderDataTable(locationsData)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)