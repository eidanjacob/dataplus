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

# # reading in data (project folder is working directory)
# coord <- read_csv("../locationsToCoordinates.csv")
# coord <- coord[order(coord$location),] # alphabetize location - coordinate dictionary
# validLocations <- read_csv("../allAPs.csv") # aps <-> locations
# dukeShape <- read_csv("../dukeShape.txt", col_names = FALSE)
# 
# numAPs <- validLocations %>% # number of APs per location
#   group_by(location) %>%
#   summarise(num = n())
# 
# # splunkData <- read_csv("../eventData.csv")
# #
# # # match aps to locations, merge for coordinates
# # df <- splunkData[!is.na(splunkData$ap),] # remove observations with no ap
# #
# # # Some aps are in splunk data with name, some with number - code below matches location using whichever is available
# # nameMatch = which(validLocations$APname %in% df$ap) # find which aps have their name in the data
# # numMatch = which(validLocations$APnum %in% df$ap) # find which aps have their number in the data
# # validLocations$ap = c(NA) # new "flexible" column to store either name or number
# # validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
# # validLocations$ap[numMatch] = validLocations$APnum[numMatch]
# #
# # validLocations <- merge(coord, validLocations) # link coordinates to locations
# # # use the new "flexible" ap variable to merge coordinates onto df
# # df <- merge(df, validLocations, by = "ap") # this is the slow step
# # write.csv(df, "../mergedData.csv")
# 
# df <- read_csv("../mergedData.csv") # this is only commented out to save me time. when viewing this code, put it back in
# df$`_time` <- force_tz(ymd_hms(df$`_time`), "EST")
# 
# # draw duke border
# p = Polygon(dukeShape)
# ps = Polygons(list(p),1)
# sps = SpatialPolygons(list(ps))
# 
# # remove out-of-bounds locations
# inBounds <- sapply(1:nrow(coord), function(x) {
#   point.in.polygon(coord$long[x], coord$lat[x], unlist(dukeShape[,1]), unlist(dukeShape[,2]))
# })
# coord <- coord[inBounds == 1,]
# coord <- merge.data.frame(coord,numAPs)
# N = nrow(coord)
# # calculating voronoi cells and converting to polygons to plot on map
# z <- deldir(coord$long, coord$lat) # computes cells
# # convert cell info to spatial data frame (polygons)
# w <- tile.list(z)
# polys <- vector(mode="list", length=length(w))
# for (i in seq(along=polys)) {
#   pcrds <- cbind(w[[i]]$x, w[[i]]$y)
#   pcrds <- rbind(pcrds, pcrds[1,])
#   polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
# }
# SP <- SpatialPolygons(polys)
# SP <- intersect(SP, sps)
# for(x in 1:nrow(coord)){
#   SP@polygons[[x]]@ID <- as.character(x)
# }
# SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(x=coord[,2], y=coord[,3]))
# # tag polygons with location name
# SPDF@data$ID = coord$location
# sapply(1:length(coord$location), function(x){
#   SPDF@polygons[[x]]@ID <- coord$location[x]
#   SPDF <<- SPDF
# })
# 
# # Default coordinates that provide overview of entire campus
# defLong <- -78.9272544 # default longitude
# defLati <- 36.0042458 # default latitude
# zm <- 15 # default zoom level
# 
# # Coordinates for West
# wLati <- 36.0003456
# wLong <- -78.939647
# wzm <- 16
# 
# # Coordinates for East
# eLati <- 36.0063344
# eLong <- -78.9154213
# ezm <- 17
# 
# # Coordinates for Central
# cLati <- 36.0030883
# cLong <- -78.9258819
# czm <- 17
# 
# 
# # Areas of polygons were calculated in original units (degrees). The code below approximates a sq. meter measure to a square degree (In Durham)
# p1 <- c(defLong, defLati)
# degScale = -3
# p2 <- c(defLong + 10 ^ degScale, defLati)
# p3 <- c(defLong, defLati + 10 ^ degScale)
# # The Haversine formula calculates distances along a spherical surface.
# areaConvert = distHaversine(p1, p2) * distHaversine(p1, p3) # = square meters per 10^degScale square degrees (in Durham)
# areaConvert = areaConvert / 10^(2 * degScale) # square meters per square degree
# 
# # ------------------------------
# # Mess with these numbers if you want.
# timeSteps = c("1hr" = 60*60, "2hr" = 2*60*60, "4hr" = 4*60*60) # in seconds
# # timeSteps = c("4 hr" = 4*60*60)
# delay = 2700 # in milliseconds
# # ------------------------------
# 
# start.time = (min(df$`_time`))
# end.time = (max(df$`_time`))
# 
# # Filtering for all macaddrs that moved/was registered within a certain period of the start time
# # to limit the size of the data set and prevent RStudio from crashing
# # It then samples num random macaddrs
# period <- 60 * 10 # in seconds
# num <- 200 # number of macaddrs to visualize
# inte <- interval(start.time, start.time + period)
# macaddrInLoc <- df %>%
#   filter(`_time` %within% inte)
# macaddrInLoc <- unique(macaddrInLoc$macaddr)
# #macaddrInLoc <- sample(macaddrInLoc, num) # should be implemented later so that indiv macaddrs that arent in this group can still be tracked
# df <- df %>% filter(macaddr %in% macaddrInLoc)
# currMacs <- NULL # just to initialize so it exists globally
# 
# popDensityList <- list()
# paletteList <- list()
# macsToLocList <- list()
# 
# end.times <- rep(end.time, length(timeSteps))
# 
# for(i in 1:length(timeSteps)){
#   timeStep <- timeSteps[i]
#   # Bin populations, calculate densities at each timestep, and cache for future plotting
#   time.windowStart = start.time # time.window for selection
#   populationDensities <- NULL
#   macsToLoc <- NULL
# 
#   while(end.time > time.windowStart){
# 
#     # Filter for time interval
#     selInt = interval(time.windowStart, time.windowStart + timeStep)
#     thisStep <- df %>%
#       filter(`_time` %within% selInt)
# 
#     # Calculate Population Densities
#     locationBinnedPop <- data.frame("location" = coord$location, "pop" = c(0))
#     # For each location, count the number of unique devices (MAC addresses) that are present during the time time.window.
#     locationBinnedPop$pop <- sapply(locationBinnedPop$location, function(x) {length(unique(thisStep$macaddr[thisStep$`location.y` == x]))})
# 
#     # Calculate a measure of people / (100 sq meters)
#     densities_area <- sapply(1:N, function(x) {100 * locationBinnedPop$pop[x] / (SPDF@polygons[[x]]@area * areaConvert)})
#     densities_aps  <- sapply(1:N, function(x) {locationBinnedPop$pop[x] / coord$num[x]})
#     densities_both <- sapply(1:N, function(x) {locationBinnedPop$pop[x] / SPDF@polygons[[x]]@area / areaConvert / coord$num[x]})
#     info <- c(densities_area, densities_aps, densities_both, locationBinnedPop$pop)
#     type <- c(rep(1, N), rep(2, N), rep(3, N), rep(4, N))
#     densitiesToSave <- data.frame("location" = locationBinnedPop$location,
#                                   #"pop" = locationBinnedPop$pop,
#                                   "ap_num" = coord$num,
#                                   "info" = info,
#                                   "type" = type,
#                                   "time.window" = c(time.windowStart))
#     populationDensities <- rbind(populationDensities, densitiesToSave)
# 
#     # For each macaddr, keep track of where it currently is
#     macs <- data.frame("macaddr" = thisStep$macaddr,
#                        "location" = thisStep$location.y,
#                        "long" = thisStep$long,
#                        "lat" = thisStep$lat,
#                        "time.window" = c(time.windowStart),
#                        "realTime" = c(thisStep$`_time`))
#     macs <- macs[order(macs$realTime), ]
#     macsToLoc <- rbind(macsToLoc, macs)
# 
#     end.times[i] <- time.windowStart
#     time.windowStart = time.windowStart + timeStep
#   }
# 
#   # setting up for chloropleth
#   palette_area <- colorNumeric("YlOrRd", (populationDensities %>% filter(type == 1))$info)
#   palette_aps  <- colorNumeric("YlOrRd", (populationDensities %>% filter(type == 2))$info)
#   palette_both <- colorNumeric("YlOrRd", (populationDensities %>% filter(type == 3))$info)
#   palette_raw  <- colorNumeric("YlOrRd", (populationDensities %>% filter(type == 4))$info)
#   palette_area_log <- colorNumeric("YlOrRd", log((populationDensities %>% filter(type == 1))$info+1))
#   palette_aps_log  <- colorNumeric("YlOrRd", log((populationDensities %>% filter(type == 2))$info+1))
#   palette_both_log <- colorNumeric("YlOrRd", log((populationDensities %>% filter(type == 3))$info+1))
#   palette_raw_log  <- colorNumeric("YlOrRd", log((populationDensities %>% filter(type == 4))$info+1))
# 
#   thisStepPaletteList <- list(palette_area, palette_aps, palette_both, palette_raw,
#                               palette_area_log, palette_aps_log, palette_both_log, palette_raw_log)
# 
#   # Cache these guys away for later
#   popDensityList[[i]] <- populationDensities
#   paletteList[[i]] <- thisStepPaletteList
#   macsToLocList[[i]] <- macsToLoc
# }
# 
# legendTitles <- c("Population Density (area)",
#                   "Population Density (aps)",
#                   "Population Density (both)",
#                   "Population (raw count)")

numOnMap <- 0 # just so it can be used later - note to self: put these all in one place
fromLoc <- "Perkins" # default
toLoc <- "WestUnion" # default

# Script that calculates how long a macaddr stayed in one place
howLong <- function(mac, currMacs) {
  macLocs <- currMacs %>%
    filter(macaddr == mac)
  
  macLocs$realTimeEnd <- lead(macLocs$realTime, 1)
  macLocs <- macLocs %>% 
    mutate(timeDiff = difftime(macLocs$realTimeEnd, macLocs$realTime)) # timeDiff is in seconds
  
  n <- 1 # unique index tied to groups of locations
  i <- 1 # row index
  # function that adds a column that has numbers that will increment when a value before it changes
  col <- sapply(macLocs$location, function(x) { 
    if(i != 1) {
      if (x != macLocs[i-1, ]$location) {
        n <<- n + 1
        i <<- i + 1
        return(n)
      } else {
        i <<- i + 1
        return(n)
      }
    } else {
      i <<- i + 1
      return(n)
    }
  })
  
  macLocs$id <- col
  
  # if a macaddr has only visited one location
  if(length(macLocs$id) == 1) {
    return(NULL)
  }
  
  timeStayed <- aggregate(timeDiff ~ id, data = macLocs, sum)
  
  # creating column that has the amount of time spent at a locations, in order of visited locations, repeats allowed.
  timeStayed <- macLocs %>% 
    group_by(id, location) %>% 
    summarise(totalTime = sum(timeDiff))
  
  timeStart <- macLocs[!duplicated(macLocs$id), ] 
  
  # creating dataframe with other useful columns
  timeStayed <- cbind(timeStayed, 
                      startTimeGeneral = timeStart$time.window, 
                      startTimeReal = timeStart$realTime, 
                      macaddr = timeStart$macaddr)
  timeStayed[is.na(timeStayed)] <- 0
  return(timeStayed)
}

# This script takes a macaddr, a dataframe of general event information,
# and returns a list of two things: 
# 1. a dataframe that tells how long a macaddr has stayed for each location
# 2. the shortened dataframe of visited locations used in drawing lines
# it returns null when the macaddr did not visit the locations properly.
# IDEAS: fromLoc and toLoc are groups of locations so you can see how people from dorms go to WU, etc.
findIndex <- function(mac, currMacs) {
  # the intervals can be changed so that locations like WU can be more accurately represented
  # aka people might intend to go to WU for take out, which means they might stay for only 5 min instead
  # of 10 min, a better indicator that the lines were long or they sat down and ate.
  # the betweenInte is 10 min just to keep travel times travel times instead of loitering times
  # the time spent determines whether a device passed through vs stayed
  fromInte <- 60 * 5 # in seconds
  betweenInte <- 60 * 10 # in seconds
  toInte <- 60 * 5 # in seconds
  # max number of locations in between the target locations
  # increasing this number increases the devices present, but it decreases the "straightforwardness"
  # of the lines present
  distInte <- 1 
  
  # filtering to find each location a macaddr has visited
  macsTime <- currMacs %>% 
    filter(macaddr == mac)
  
  # filtering to find each location a macaddr has stayed
  timeStayed <- howLong(mac, macsTime)
  if(is.null(timeStayed)) {
    return(NULL)
  }
  # filtering for locations based on the time intervals above
  n1 <- which(timeStayed$location == fromLoc & timeStayed$totalTime > fromInte)
  n2 <- which(timeStayed$location == toLoc & timeStayed$totalTime > toInte)
  n3 <- which(timeStayed$location != fromLoc & timeStayed$location != toLoc & timeStayed$totalTime > betweenInte)
  n4 <- c(n1, n2, n3)
  n4 <- sort(n4)
  macsLocs <- timeStayed[n4, ]
  
  # filter out if macaddr does not stay at the locations correctly
  if(!fromLoc %in% macsLocs$location | !toLoc %in% macsLocs$location) {
    return(NULL)
  }
  # finding when device visited locations
  indexFrom <- match(fromLoc, macsLocs$location)
  indexTo <- match(toLoc, macsLocs$location)
  
  while(!is.na(indexTo) & !is.na(indexFrom)) {
    if(indexTo > indexFrom) {
      # to minimize the distance between the two locations -- that is, for ex,
      # to filter out if a device starts at perkins, chills in their room
      # for two hours, goes on East, goes back to west, goes to WU.
      if(indexTo - indexFrom > distInte) {
        indexFrom <- match(fromLoc, macsLocs[indexFrom+1:length(macsLocs), ]$location) + indexFrom
        next
      }
      break
    }
    indexTo <- match(toLoc, macsLocs[indexTo+1:length(macsLocs), ]) 
  }
  # macaddr does not visit locations correctly
  if(is.na(indexTo) | is.na(indexFrom)) {
    return(NULL)
  }
  
  # by this point, the device has visited the appropriate locations
  # printing this stuff later/now just to see how many macaddrs were caught by the script for intuition/debugging purposes
  numOnMap <<- numOnMap + 1
  #print(mac)
  
  # using the time visited, grab the subset of visited locations to viz path from one loc to another
  indexFrom <- match(macsLocs$startTimeReal[indexFrom], macsTime$realTime)
  indexTo <- match(macsLocs$startTimeReal[indexTo], macsTime$realTime)
  macsTime <- macsTime[indexFrom:indexTo, ]
  return(list(timeStayed = timeStayed, macsTime = macsTime))
}


# app user interface
ui <- fluidPage(
  
  titlePanel("Duke Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      # input a time to show temporally close records on map
      selectInput("timeStepSelection", "Time Step", choices = timeSteps, selected = timeSteps[1]),
      uiOutput("ui"),
      selectInput("select", "View:", choices = c("Population Density (area)" = 1, "Population Density (aps)" = 2, 
                                                 "Population Density (both)" = 3, "Population (raw)" = 4), selected = 1),
      radioButtons("focus", "Zoom View", choices = c("All", "East", "Central", "West"), selected = "All"),
      checkboxInput("log", "Log Scale", value = FALSE),
      checkboxInput("flow", "Viz Flow", value = FALSE),
      textInput("from", "From location: ", value = fromLoc),
      textInput("to", "To location: ", value = toLoc),
      actionButton("submitLocs", "Submit"),
      textInput("num", "Number of devices: ", value = num),
      actionButton("submitNum", "Submit")
    ),
    
    mainPanel(
      leafletOutput("map", height = 850) # output the map, should check if height is ok with different screens
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
  
  observeEvent(input$map_shape_click, {
    clickedGroup <- input$map_shape_click$'group'
    clickedID <- input$map_shape_click$'id'
    if(clickedGroup == "severals") { # Line clicked when visualizing flow
      print(clickedID) # print macaddr
      
      macDF <- findIndex(clickedID, currMacs)
      
      View(macDF$timeStayed) # view locations that passed the initial time filtering
      View(macDF$macsTime) # view movement data
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
    currMacs <<- macsToLocList[[which(timeSteps == input$timeStepSelection)]]
    if(!any(populationDensities$time.window == input$time)){
      return()
    }
    
    thisStep <- populationDensities %>%
      filter(time.window == input$time) %>% 
      filter(type == as.numeric(input$select))
    
    myPaletteList <- paletteList[[unname(which(timeSteps == input$timeStepSelection))]]
    myPalette <- myPaletteList[[as.numeric(input$select)]]
    if(input$log){
      thisStep$info <- log(thisStep$info + 1)
      myPalette <- myPaletteList[[as.numeric(input$select) + length(myPaletteList)/2]]
    }
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
    legendVals <- (populationDensities %>% filter(type == as.numeric(input$select)))$info
    if(input$log){
      legendVals <- log(1 + legendVals)
    }
    leafletProxy("map") %>%
      addLegend(pal = myPalette, 
                values = legendVals,
                position = "topright",
                title = legendTitles[as.numeric(input$select)])
    
  })
  
  # Visualizing the flow of people
  observeEvent({input$flow; input$submitLocs; input$submitNum} ,{
    if(input$flow) {
      
      print("Calculating...")
      uniqMacs <- unique(currMacs$macaddr)
      uniqMacs <- as.character(uniqMacs)
      
      leafletProxy("map") %>% 
        clearGroup("severals") 
      
      numOnMap <<- 0
      
      # looping through each macaddr to determine its movement
      for(i in 1:length(uniqMacs)) { 
        if(i == num) { # to prevent stuff from crashing
          break
        }
        
        macDF <- findIndex(uniqMacs[[i]], currMacs)
        if(is.null(macDF)) {
          next
        }
        macsTime <- macDF$macsTime
        
        macLabels <- sprintf("macaddr: %s",
                             macsTime$macaddr) %>%
          lapply(htmltools::HTML)
        
          leafletProxy("map") %>% 
            addPolylines(lng = macsTime$long, 
                         lat = macsTime$lat,
                         layerId = macsTime$macaddr,
                         group = "severals",
                         weight = 2,
                         opacity = flowOp,
                         label = macLabels,
                         highlightOptions = highlightOptions(
                           weight = 5,
                           color = highlight,
                           fillOpacity = 1,
                           bringToFront = TRUE)) %>% 
            addCircles(lng = macsTime$long[1], # red is from
                       lat = macsTime$lat[1],
                       group = "severals",
                       radius = 5,
                       weight = 2,
                       opacity = flowOp,
                       color = from) %>% 
            addCircles(lng = macsTime$long[length(macsTime$long)], # blue is to
                       lat = macsTime$lat[length(macsTime$lat)],
                       group = "severals",
                       radius = 5,
                       weight = 2,
                       opacity = flowOp,
                       color = to)

        
      } 
      cat("Number of devices present:", numOnMap, "\n")
      }
  })
  
  
  observeEvent(input$submitLocs, {
    fromLoc <<- input$from
    toLoc <<- input$to
    print("Submitted locations.")
  })
  
  observeEvent(input$submitNum, {
    num <<- input$num
    print("Submitted num.")
  })
  
  # Clearing the map of various elements when they are selected/deselected
  observeEvent({input$flow; input$time}, priority = 1, ignoreInit = TRUE, 
               handlerExpr =  {
                 leafletProxy("map") %>% 
                   clearGroup("severals") %>% 
                   clearGroup("cluster")
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
