# singleFileFromTo Take Two
# Note that as of 9/15/18 there are some errors with how findIndex works.
# The fixed version will be uploaded likely in a few days.

# Load packages:
# readr for data frame input
# leaflet for mapping
# dplyr for data wrangling
# Shiny for interactive apps
# deldir for voronoi cell calculations
# sp, rgdal, raster for drawing polygons
# lubridate for easy handling of times and dates
# geosphere for haversine formula (calculate distance on sphere)
# data.table for creating column indicating same location (rleid function)

library(readr)
library(leaflet)
library(dplyr)
library(shiny)
library(deldir)
library(sp)
library(rgdal)
library(lubridate)
library(geosphere)
library(rgeos)
library(raster)
library(data.table) 
library(ggplot2)

# Global variables
#################
# Variables for filtering
fLoc <- NULL
tLoc <- NULL
num.macs <- 200 # number of macs to filter through
dir <- "./data/" # what directory the data is in
data.fname <- "mergedData.csv" # name of data file
coord.fname <- "locationsToCoordinates.csv" # name of locations to coordinates file

# Variables for Time Step Selection
start.time <- min(df0419$`_time`)
end.time <- max(df0419$`_time`)
delay <- 2700 # in ms
step <- 60 * 1 # in seconds

# Default coordinates that provide overview of entire campus
defLong <- -78.9272544 # default longitude
defLati <- 36.0042458 # default latitude
zm <- 15 # default zoom level

# Colors for mapping
fromCirc <- "red" # from circle
toCirc <- "blue" # to circle
colorL <- "Blues" # color palette for lines

# Data sets for mapping
# coord <- read_csv(coord.fname) # for location data
# df0419 <- read_csv(paste0(dir,data.fname)) # dataset we are using
df0419 <- df0419 %>% # reorder data frame
  arrange(`_time`, macaddr)
#################

# Functions
#################
# Returns dataframe with columns
# macaddr, id, fromLoc, totalTime, startTime, and endTime,
# where total time is how long the mac spent at the location.
# Requires macdf to have columns called
# macaddr and fromLoc.
howLong <- function(macdf) {
  macdf <- macdf %>% # creating column with index that changes when location changes
    group_by(macaddr) %>% 
    mutate(id = rleid(fromLoc))
  macdf <- macdf %>% # summing up the amount of time spent at a location
    group_by(macaddr, id, fromLoc) %>% 
    summarise(totalTime = sum(timeDiff), 
              startTime = min(`_time`), 
              endTime = max(`_time`))
  return(macdf)
}

# Returns a list of two things
# 1. Original dataframe filtered down to relevant info
# 2. Truncated dataframe filtered to qualifying locations
# Requires macdf to have columns called
# macaddr and fromLoc.
# If the parameter full = TRUE, then it will return all paths, regardless of 
# beginning or destination. In the future, we could plot the paths such that
# each location that was stayed at for longer than some specified time
# would have some sort of indicator.
findIndex <- function(macdf, fLoc, tLoc, 
                      fromInte = 60 * 5, toInte = 60 * 5, 
                      betweenInte = 60 * 10, distInte = 1, full = FALSE) {
  orig <- macdf # saving original dataframe
  macdf <- howLong(macdf)
  
  # If we only carry about the entire path, then return the whole thing
  if(full) {
    macs <- unique(macdf$macaddr)
    returnPaths <- lapply(macs, function(mac) {
      macLoc <- macdf %>% 
        filter(macaddr == mac)
      orig <- orig %>% 
        filter(macaddr == mac)
      return(list(orig = orig, trun = macLoc))
    })
    return(returnPaths)
  }
  
  # Filter dataframe based on parameters
  # e.g. whether the macaddr stayed in a location for the correct amount of time
  macdf <- macdf %>% mutate(doesQualify = (fromLoc == fLoc & totalTime > fromInte) | 
                              (fromLoc == tLoc & totalTime > toInte) |
                              (!fromLoc %in% c(fLoc, tLoc) & totalTime > betweenInte))
  macdf <- macdf[macdf$doesQualify, ]
  
  macdf <- macdf[!is.na(macdf$doesQualify), ] # getting rid of NA
  
  # Finding relevant path from fLoc to tLoc
  macs <- unique(macdf$macaddr)
  returnPaths <- sapply(macs, function(mac) {
    macLoc <- macdf %>% 
      filter(macaddr == mac)
    orig <- orig %>% 
      filter(macaddr == mac)
    
    if(!fLoc %in% macLoc$fromLoc | !tLoc %in% macLoc$fromLoc) { # does not visit locations correctly
      return(NULL)
    }
    # finding when device visited locations
    indexFrom <- match(fLoc, macLoc$fromLoc)
    indexTo <- match(tLoc, macLoc$fromLoc)
    
    while(!is.na(indexTo) & !is.na(indexFrom)) {
      if(indexTo > indexFrom) {
        # to minimize the distance between the two locations -- that is, for ex,
        # to filter out if a device starts at perkins, chills in their room
        # for two hours, goes on East, goes back to west, goes to WU.
        if(indexTo - indexFrom > distInte) {
          indexFrom <- match(fLoc, macLoc[indexFrom+1:length(macLoc), ]$fromLoc) + indexFrom
          next
        }
        break
      }
      indexTo <- match(tLoc, macLoc[indexTo+1:length(macLoc), ]$fromLoc) + indexTo
    }
    # macaddr does not visit locations correctly
    if(is.na(indexTo) | is.na(indexFrom)) {
      return(NULL)
    }
    
    indexFrom2 <- match(macLoc$startTime[indexFrom], orig$`_time`)
    indexTo2 <- match(macLoc$startTime[indexTo], orig$`_time`)
    orig <- orig[indexFrom2:indexTo2, ]
    trun <- macLoc[indexFrom:indexTo, ]
    return(list(orig = orig, trun = trun))
  })
  returnPaths <- returnPaths[!sapply(returnPaths, is.null)] # remove null values
  return(returnPaths)
}
# This function takes in the same parameters as findIndex, but instead it calls findIndex and returns three things.
# 1. The original return of findIndex - for debugging purposes.
# 2. The most popular complete paths.
# 3. The most popular, single location to location paths.
# This is useful when one wants to visualize several different paths from different locations.
# The dataframe, macdf, originally has 19 column names, but only columns macaddr and fromLoc should be required,
# prefix, ap, _time, asa_code, macaddr, slotnum, ssid, ipaddr, location.x, fromLoc, lat, long, campus, APname, APnum, org, toLoc, nextTime, timeDiff
# fromLoc - location.y from mergedData
# toLoc - next location visited by that macaddr
# nextTime - the time of the next event registered by that macaddr
# timeDiff - nextTime minus _time
findPaths <- function(macdf, fLoc, tLoc, 
                      fromInte = 60 * 5, toInte = 60 * 5, 
                      betweenInte = 60 * 10, distInte = 1, full = FALSE) {
  findI <- findIndex(macdf, fLoc, tLoc, 
                     fromInte, toInte, 
                     betweenInte, distInte, full)
  
  # Checking to see if any viable paths were found
  if(length(findI) == 0) { # none were found
    return(NULL)
  }
  
  # Binning multiple events of a single location into one
  # E.g. three Kilgo ap events gets binned to just one event
  # Returns the full path as a string separated by ", "
  fullPaths <- sapply(findI, function(mac) {
    path <- mac$orig
    path <- path %>% 
      mutate(id = rleid(fromLoc)) %>% 
      group_by(id, fromLoc) %>%
      summarise(totalTime = sum(timeDiff)) # total time stayed at a location before moving on
    
    return(toString(paste0(path$fromLoc)))
  })
  
  # Grabbing the most common (full) paths
  fullPaths <- as.data.frame(fullPaths) %>% 
    group_by(fullPaths) %>% 
    summarise(freq = n()) %>% 
    arrange(desc(freq))
  
  # Grabbing the most common A->B jumps
  subPaths <- NULL
  sink("NUL") # suppress output
  sapply(findI, function(mac) {
    path <- mac$orig
    path <- path %>% 
      group_by(fromLoc, toLoc) %>% 
      summarise(freq = n())
    
    subPaths <<- rbind(subPaths, path)
    return(NULL)
  })
  sink()
  
  # Find most common A->B jumps and sorting accordingly
  subPaths <- subPaths %>% 
    group_by(fromLoc, toLoc) %>% 
    summarise(freq = n()) %>% 
    arrange(desc(freq))
  
  # Removing A->A events
  subPaths <- subPaths[sapply(1:nrow(subPaths), function(i) {subPaths$fromLoc[i] != subPaths$toLoc[i]}),]
  
  return(list(orig = findI, fullPaths = fullPaths, subPaths = subPaths))
}
#################

# Creating location polygons
#################
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
for(x in 1:nrow(coord)){
  SP@polygons[[x]]@ID <- as.character(x)
}
SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(x=coord[,2], y=coord[,3]))
# tag polygons with location name
SPDF@data$ID = coord$location
a <- sapply(1:length(coord$location), function(x){
  SPDF@polygons[[x]]@ID <- coord$location[x]
  SPDF <<- SPDF
})
#################

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Duke Wireless Data - Path Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       uiOutput("ui"),
       textInput("fromLoc", "From Location", value = "Perkins"),
       textInput("toLoc", "To Location", value = "WestUnion"),
       actionButton("submitLocs", "Submit Locations"),
       checkboxInput("full", "View all paths"),
       p(),
       actionButton("calculate", "Calculate Paths")
     ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(type = "tabs", 
                     tabPanel("Map", leafletOutput("map", height = 850)),
                     tabPanel("Table", tableOutput("table")))
      )
   )
)

server <- function(input, output, session) {
  
  output$ui <- renderUI({
    # input a time to show temporally close records on map
    sliderInput("time", "Time", min = start.time, max = end.time,
                value = c(start.time, end.time), animate = animationOptions(interval=delay),
                step = step)
  })
  
  # Creating labels that appear when you click on a polygon
  polyLabels <- sprintf("%s",
                        coord$location) %>% 
    lapply(htmltools::HTML)
  
  # Creating map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView("map", lng = defLong, lat = defLati, zoom = zm) %>% # sets initial map zoom & center
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
      addPolygons(data = SPDF, 
                  layerId = coord$location,
                  group = coord$location,
                  popup = polyLabels,
                  weight = 1,
                  color = "gray81",
                  fillOpacity = 0.2,
                  fillColor = "azure")
  })

  # Submit locations is pressed
  observeEvent(input$submitLocs, {
    fLoc <<- input$fromLoc
    tLoc <<- input$toLoc
  })
  
  observeEvent(input$calculate,{
    withProgress({
      # Clear map
      leafletProxy("map") %>% 
        clearGroup("lines")
      
      # Check to see if we want to see all possible paths
      if(input$full) {
        bothMacs <- unique(df0419$macaddr)
        dflocs <- df0419 %>% 
          filter(macaddr %in% bothMacs[1:num.macs]) # notice it only filters for a portion
      } else {
        # Filter for macs that have visited the desired locations to speed up runtimes
        bothMacs <- unique((df0419 %>%
                              filter(location %in% c(fLoc,tLoc)))$macaddr)
        dflocs <- df0419 %>%
          filter(macaddr %in% bothMacs[1:num.macs]) # notice it only filters for a portion
      }
      
      # Filtering for events within time step selected
      dflocs <- dflocs %>% 
        filter(`_time` %within% interval(input$time[[1]], input$time[[2]])) 
      
      # Creating new columns
      dflocs <- dflocs %>%
        group_by(macaddr) %>%
        mutate(toAP = lead(ap, order_by = macaddr),
               toLoc = lead(location, order_by = macaddr),
               nextTime = lead(`_time`, order_by = macaddr),
               timeDiff = as.numeric(difftime(nextTime, `_time`, units = "secs"))) %>%
        rename(fromAP = ap,
               fromLoc = location)
      
      # Creating datatable of most common ap to ap jumps
      dfap <<- dflocs %>%
        group_by(fromAP, toAP, fromLoc, toLoc) %>%
        summarise(freq = n()) %>%
        arrange(desc(freq))
      
      incProgress()
      
      # Finding viable paths
      temp <- findPaths(dflocs, fLoc, tLoc, full = input$full)
      if(is.null(temp)) { # no viable paths were found
        return()
      }
      
      fullP <- temp$fullPaths # full fLoc->tLoc paths
      
      incProgress()
      
      # The paths are in order from most common to least common,
      # here, we are splitting the strings the paths are stored as in order
      # to bin them accordingly and attach appropriate colors to their 
      # frequencies
      splitPaths <- sapply(as.character(fullP$fullPaths), strsplit, split = ", ")
      colorPal <- colorNumeric(colorL, fullP$freq)
      pathsToCoords <- NULL
      for(i in 1:length(splitPaths)) {
        # Matching paths to their coordinates and counts
        indivLocs <- data.frame(location = splitPaths[[i]], count = fullP$freq[[i]], index = i) # index for grouping lines
        pathsToCoords <- merge.data.frame(indivLocs, coord, sort = FALSE)
        
        # Creating labels
        lineLabels <- sprintf("%s <br/ >%g",
                          toString(splitPaths[[i]]), # the path
                          indivLocs$count[[1]]) %>% # how many macs went that path
          lapply(htmltools::HTML)
        
        # Adding lines
        leafletProxy("map") %>%
          addPolylines(lng = pathsToCoords$long, 
                       lat = pathsToCoords$lat, 
                       layerId = pathsToCoords$index,
                       group = "lines",
                       weight = pathsToCoords$count,
                       label = lineLabels,
                       opacity = 0.5, 
                       color = colorPal(pathsToCoords$count),
                       highlightOptions = highlightOptions(
                         weight = pathsToCoords$count,
                         color = "red",
                         fillOpacity = 1,
                         bringToFront = TRUE))
        
        # Add circles to indicate fLoc and tLoc
        if(tLoc == "" | fLoc == "") {
          leafletProxy("map") %>% 
            addCircles(lng = pathsToCoords$long[[1]],
                       lat = pathsToCoords$lat[[1]], 
                       radius = 5,
                       weight = 2,
                       opacity = 0.1,
                       color = fromCirc) %>% 
            addCircles(lng = pathsToCoords$long[[length(pathsToCoords$long)]],
                       lat = pathsToCoords$lat[[length(pathsToCoords$long)]], 
                       radius = 5,
                       weight = 2,
                       opacity = 0.1,
                       color = toCirc)
        }
        
      }
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

