# The original, with flow viz and mac tracking
# BUGS
# Polygons will be on top of lines when they are redrawn when you change time steps and view
# Solution is to uncheck and check the lines box.

# Load packages:
# readr, readxl for data frame input
# leaflet for mapping
# dplyr for data wrangling
# Shiny for interactive apps
# deldir for voronoi cell calculations
# sp, rgdal for drawing polygons
# lubridate for easy handling of times and dates
# geosphere for haversine formula (calculate distance on sphere)
# raster for coercing sp object to vector/avoid error
# data.table for rleid function - indexing consecuitive locations

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
library(data.table)

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
#
# # merge with OUI table to identify manufacturers
# df$prefix <- sapply(df$macaddr, function(mac){
#   str <- substr(mac, 1, 8)
#   return(gsub(":", "-", toupper(str)))
# })
# oui <- read_csv("../ouiDF.txt")
# df <- merge(df, oui)
# write.csv(df, "../mergedData.csv")

# reading in data from directory
df <- NULL
directory <- "../data" # name of directory with data
files <- list.files(directory, full.names = TRUE)
lapply(files, function(fname) {
  df <<- rbind(df, read_csv(paste0(fname)))
})

#df <- read_csv("../mergedData0419.csv")
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

# # Filtering for all macaddrs that moved/was registered within a certain period of the start time
# # to limit the size of the data set and prevent RStudio from crashing
# # It then samples num random macaddrs later in the code
num <- 1000 # number of macaddrs to visualize
currMacs <- NULL # just to initialize so it exists globally

popDensityList <- list()
paletteList <- list()
macsToLocList <- list()

end.times <- rep(end.time, length(timeSteps))

colorPal <- "Purples"

for(i in 1:length(timeSteps)){
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
    densities_area  <- sapply(1:N, function(x) {100 * locationBinnedPop$pop[x] / (SPDF@polygons[[x]]@area * areaConvert)})
    densities_aps   <- sapply(1:N, function(x) {locationBinnedPop$pop[x] / coord$num[x]})
    densities_both  <- sapply(1:N, function(x) {locationBinnedPop$pop[x] / SPDF@polygons[[x]]@area / areaConvert / coord$num[x]})
    info <- c(densities_area, densities_aps, densities_both, locationBinnedPop$pop)
    type <- c(rep(1, N), rep(2, N), rep(3, N), rep(4, N))
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
                       "campus" = thisStep$campus,
                       "long" = thisStep$long,
                       "lat" = thisStep$lat,
                       "time.window" = c(time.windowStart),
                       "realTime" = c(thisStep$`_time`))
    macs <- macs[order(macs$realTime), ]
    macsToLoc <- rbind(macsToLoc, macs)

    end.times[i] <- time.windowStart
    time.windowStart = time.windowStart + timeStep
  }

  # setting up for chloropleth
  palette_area <- colorNumeric(colorPal, (populationDensities %>% filter(type == 1))$info)
  palette_aps  <- colorNumeric(colorPal, (populationDensities %>% filter(type == 2))$info)
  palette_both <- colorNumeric(colorPal, (populationDensities %>% filter(type == 3))$info)
  palette_raw  <- colorNumeric(colorPal, (populationDensities %>% filter(type == 4))$info)
  palette_area_log <- colorNumeric(colorPal, log((populationDensities %>% filter(type == 1))$info+1))
  palette_aps_log  <- colorNumeric(colorPal, log((populationDensities %>% filter(type == 2))$info+1))
  palette_both_log <- colorNumeric(colorPal, log((populationDensities %>% filter(type == 3))$info+1))
  palette_raw_log  <- colorNumeric(colorPal, log((populationDensities %>% filter(type == 4))$info+1))

  thisStepPaletteList <- list(palette_area, palette_aps, palette_both, palette_raw,
                              palette_area_log, palette_aps_log, palette_both_log, palette_raw_log)

  # Cache these guys away for later
  popDensityList[[i]] <- populationDensities
  paletteList[[i]] <- thisStepPaletteList
  macsToLocList[[i]] <- macsToLoc
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
      selectInput("select", "View:", choices = c("Population Density (area)" = 1, "Population Density (aps)" = 2, 
                                                 "Population Density (both)" = 3, "Population (raw)" = 4), selected = 1),
      radioButtons("focus", "Zoom View", choices = c("All", "East", "Central", "West"), selected = "All"),
      checkboxInput("log", "Log Scale", value = FALSE),
      checkboxInput("flow", "Track flow", value = FALSE),
      conditionalPanel( 
        condition = "input.flow",
        checkboxInput("removeEW", "Remove Cross Campus Lines", value = FALSE),
        checkboxInput("cluster", "Enable clustering", value = FALSE)
      ),
      checkboxInput("track", "Track Single Macaddr", value = FALSE),
      conditionalPanel( 
        condition = "input.track",
        textInput("mac", "Track", value = "00:b3:62:16:56:05"),
        actionButton("submitMac", "Submit"),
        p(),
        htmlOutput("canTrack")
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", leafletOutput("map", height = 850)), # output the map, should check if height is ok with different screens
                  tabPanel("Table", dataTableOutput("genT")),
                  tabPanel("Mac Table", dataTableOutput("macT"))
      )
    )
  )
)

# app backend
server <- function(input, output, session) {
  
  include <- reactiveValues(poly = coord$location, # list of locations to be included
                            singleMac = "00:b3:62:16:56:05", # default macaddr to track
                            removeEW = FALSE) 
  
  # Colors for movement lines
  from <- 'red'
  to <- 'blue'
  stationary <- 'blue'
  highlight <- 'black'
  
  borderInclude <- 'black'
  borderUninclude <- 'white'
  
  flowOp <- 0.3
  trackOp <- 0.5
  polyOp <- 0.5
  
  # Seeing if something was clicked and acting as needed
  observeEvent(input$map_shape_click, {
    clickedGroup <- input$map_shape_click$'group'
    clickedID <- input$map_shape_click$'id'
    if(clickedGroup == "shapes") { # Polygon clicked
      if(clickedID %in% include$poly) {
        include$poly <- include$poly[!include$poly %in% clickedID] # taking out of included polygons
      }
      else {
        include$poly <- c(include$poly, clickedID) # putting it back into included polygons
        include$poly <- sort(include$poly) # need to sort so that shapes drawn have correct id
      }
    }
    else if(clickedGroup == "severals" | clickedGroup == "singles") { # Line clicked
      macLocs <- currMacs %>% 
        filter(macaddr == clickedID) %>%
        filter(time.window == input$time)
      macLocs <- macLocs[order(macLocs$realTime), ]
      output$macT <- renderDataTable(macLocs) # view its visited locations
    }
  })
  
  observeEvent(input$submitMac, { # changing which macaddr to track
    include$singleMac <- input$mac 
    if(include$singleMac %in% currMacs$macaddr) {
      output$canTrack <- renderUI({
       HTML(paste0(unique((currMacs %>% 
                             filter(macaddr == include$singleMac))$time.window), sep = '<br/>'))
      })
    } else {
      output$canTrack <- renderText("Macaddr not found. Note that macaddr must be lowercase.")
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
      filter(location %in% include$poly) %>%
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
      addPolygons(data = SPDF[SPDF@data$ID %in% thisStep$location, ], # draws the included polygons
                  layerId = thisStep$location,
                  group = "shapes",
                  weight = 1,
                  color = borderInclude,
                  fillOpacity = polyOp,
                  fillColor = ~myPalette(thisStep$info),
                  label = labels)
    leafletProxy("map") %>%
      addPolygons(data = SPDF[!SPDF@data$ID %in% thisStep$location, ], # draws the unincluded polygons
                  layerId = coord$location[!coord$location %in% thisStep$location],
                  group = "shapes",
                  weight = 1.5,
                  color = borderUninclude,
                  opacity = 0.2,
                  fillOpacity = 0)
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
  observe({
    
    observeEvent(input$flow, { # sampling num amount of macaddrs
      uniqMacs <<- unique(currMacs$macaddr)
      uniqMacs <<- as.character(uniqMacs)
      uniqMacs <<- sample(uniqMacs, num)
    }, ignoreInit = TRUE, priority = 1)
    
    include$removeEW <- input$removeEW
    
    observeEvent({input$time}, {}, priority = -1) # here just to trigger this observe; there probably exists a better way to do this
    
    if(input$flow) {
      noMove <- NULL
      
      leafletProxy("map") %>% 
        clearGroup("severals") %>% 
        clearGroup("cluster")
      
      withProgress(message = "Loading...", {
        # looping through each macaddr to determine its movement
        for(i in 1:length(uniqMacs)) { 
          if(i == num) { # to prevent stuff from crashing
            break
          }
          incProgress(amount = 1/(num+100))
          # filtering to find each location a macaddr has visited
          macsTime <- currMacs %>% 
            filter(macaddr == uniqMacs[[i]]) %>% 
            filter(time.window == input$time)
          macLocs <- macsTime %>% # this is to determine whether a macaddr has moved at all
            group_by(location) %>%
            summarise(num = n())
          
          macLabels <- sprintf("macaddr: %s",
                               macsTime$macaddr) %>%
            lapply(htmltools::HTML)
          
          if(length(macLocs$location) == 1) { # stores the macaddrs in a dataframe so that they can be clustered later
            noMoveMacs <- data.frame(long = macsTime$long[[1]],
                                     lat = macsTime$lat[[1]],
                                     macaddr = macsTime$macaddr[[1]],
                                     location = macsTime$location[[1]])
            noMove <- rbind(noMove, noMoveMacs)
          } else if(length(macsTime$location) != 0) { # drawing movement lines
            # drawing "broken" paths by checking if one of the locations to be not included was visited by the mac
            # or the mac went to a different campus. Note that nothing will be drawn if the mac only went to one location on another campus.
            if((TRUE %in% (!macsTime$location %in% include$poly)) | (length(unique(macsTime$campus)) > 1 & include$removeEW)   ) {
              n <- 1 # unique index tied to groups of locations
              i <- 1 # row index
              
              # function that adds a column that has numbers that will increment when a value before it changes
              if(TRUE %in% (!macsTime$location %in% include$poly)) {
                col <- rleid(macsTime$location)
              } else {
                col <- rleid(macsTime$campus)
              }
              macsTime$id <- col
              splitPaths <- split(macsTime, f = macsTime$id) # list of individual paths that resulted from breaking it up into pieces
              allLines <- NULL
              if(length(splitPaths) == 0) {
                next
              }
              for(j in 1:length(splitPaths)) { # making lines and putting them into a SLDF in order to be associated with the correct macaddr
                l <- Line(subset(splitPaths[[j]], select=c("long", "lat")))
                allLines <- c(allLines, l)
              }
              l2 <- Lines(allLines, ID = macsTime$macaddr[[1]])
              SL <- SpatialLines(c(l2))
              
              leafletProxy("map") %>%
                addPolylines(data = SL,
                             layerId = macsTime$macaddr,
                             group = "severals",
                             weight = 2,
                             opacity = flowOp,
                             label = macLabels,
                             highlightOptions = highlightOptions(
                               weight = 5,
                               color = highlight,
                               fillOpacity = 1,
                               bringToFront = TRUE))
              next
            }
            
            leafletProxy("map") %>% 
              addPolylines(lng = macsTime$long, 
                           lat = macsTime$lat,
                           layerId = macsTime$macaddr,
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
              addCircles(lng = macsTime$long[1], # red is from
                         lat = macsTime$lat[1],
                         group = "severals",
                         weight = 2,
                         opacity = flowOp,
                         color = from) %>% 
              addCircles(lng = macsTime$long[length(macsTime$long)], # blue is to
                         lat = macsTime$lat[length(macsTime$lat)],
                         group = "severals",
                         weight = 2,
                         opacity = flowOp,
                         color = to)
          }
        } 
        
        # Controls to enable clustering of people who don't move during the given timeframe
        if(input$cluster) {
          leafletProxy("map") %>% 
            addCircleMarkers(lng = noMove$long,
                             lat = noMove$lat,
                             group = "cluster",
                             color = stationary,
                             radius = 5,
                             weight = 2,
                             opacity = flowOp,
                             clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE,
                                                                   maxClusterRadius = 10))
        } else {
          leafletProxy("map") %>% 
            addCircles(lng = noMove$long,
                       lat = noMove$lat,
                       group = "cluster",
                       color = stationary,
                       radius = 5,
                       weight = 2,
                       opacity = flowOp)
        }
      })
    }
  })
  
  # Tracking a single macaddr
  observe({
    if(input$track) {
      leafletProxy("map") %>%
        clearGroup("singles")
      
      macTime <- currMacs %>% 
        filter(macaddr == include$singleMac) %>% 
        filter(time.window == input$time) %>% 
        filter(location %in% include$poly) 
      macLocs <- macTime %>% 
        group_by(location) %>%
        summarise(num = n())
      
      # draws a circle if there is no movement
      if(length(macLocs$location) == 1) { 
        leafletProxy("map") %>% 
          addCircles(lng = macTime$long,
                     lat = macTime$lat,
                     group = "singles",
                     radius = 3,
                     weight = 2,
                     opacity = trackOp,
                     color = stationary)
        # drawing movement lines
      } else if(length(macTime$location) != 0) { 
        leafletProxy("map") %>% 
          addPolylines(lng = macTime$long, 
                       lat = macTime$lat,
                       layerId = include$singleMac,
                       group = "singles",
                       weight = 5,
                       opacity = trackOp,
                       color = to,
                       highlightOptions = highlightOptions(
                         weight = 8,
                         color = highlight,
                         fillOpacity = 1,
                         bringToFront = TRUE)) %>% 
          addCircles(lng = macTime$long[1], # from
                     lat = macTime$lat[1],
                     group = "singles",
                     weight = 2,
                     opacity = trackOp,
                     color = from) %>% 
          addCircles(lng = macTime$long[length(macTime$long)], # to
                     lat = macTime$lat[length(macTime$lat)],
                     group = "singles",
                     weight = 2,
                     opacity = trackOp,
                     color = to)  
      }
    }
  })
  
  # Clearing the map of various elements when they are selected/deselected
  observeEvent({input$flow; input$time}, priority = 1, ignoreInit = TRUE, 
               handlerExpr =  {
                 leafletProxy("map") %>% 
                   clearGroup("severals") %>% 
                   clearGroup("cluster")
               })
  observeEvent({input$cluster; input$time}, priority = 1, ignoreInit = TRUE, 
               handlerExpr =  {
                 leafletProxy("map") %>% 
                   clearGroup("cluster")
               })
  observeEvent({input$track; input$time}, priority = 1, ignoreInit = TRUE, 
               handlerExpr =  {
                 leafletProxy("map") %>% 
                   clearGroup("singles")
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
  
  output$genT <- renderDataTable(currMacs %>% filter(time.window == input$time))
  
}

# Run the application 
shinyApp(ui = ui, server = server)