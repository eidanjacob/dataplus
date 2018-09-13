# singleFileFromTo Take Two
# Note that as of 9/13/18 there are some errors with how findIndex works.
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

# coord <- read_csv("locationsToCoordinates.csv") # for location data
# df0419 <- read_csv("mergedData0419.csv") # dataset we are using
df0419 <- df0419 %>% # reorder data frame
  arrange(`_time`, macaddr)
# Global variables
dflocs <- NULL
fLoc <- NULL
tLoc <- NULL


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
findIndex <- function(macdf, fLoc, tLoc, 
                      fromInte = 60 * 5, toInte = 60 * 5, 
                      betweenInte = 60 * 10, distInte = 1) {
  orig <- macdf # saving original dataframe
  macdf <- howLong(macdf)
  # filter dataframe based on parameters
  # e.g. whether the macaddr stayed in a location for the correct amount of time
  macdf <- macdf %>% mutate(doesQualify = (fromLoc == fLoc & totalTime > fromInte) | 
                              (fromLoc == tLoc & totalTime > toInte) |
                              (!fromLoc %in% c(fLoc, tLoc) & totalTime > betweenInte))
  macdf <- macdf[macdf$doesQualify, ]
  
  macdf <- macdf[!is.na(macdf$doesQualify), ] # getting rid of NA
  
  # finding relevant path from fLoc to tLoc
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
                      betweenInte = 60 * 10, distInte = 1) {
  findI <- findIndex(macdf, fLoc, tLoc, 
                     fromInte, toInte, 
                     betweenInte, distInte)
  
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

# Creating adjacency table
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
adjTable <- gTouches(SPDF, byid = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Duke Wireless Data - Path Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       textInput("fromLoc", "From Location", value = "Perkins"),
       textInput("toLoc", "To Location", value = "WestUnion"),
       actionButton("submitLocs", "Submit Locations"),
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
  

  observeEvent(input$submitLocs, {
    fLoc <<- input$fromLoc
    tLoc <<- input$toLoc
  })
  
  observeEvent(input$calculate,{
    withProgress({
    # Filter for macs that have visited both locations to speed up runtimes
    if(fLoc != "" & tLoc != ""){
      bothMacs <- unique((df0419 %>%
                            filter(fromLoc %in% c(fLoc,tLoc)))$macaddr)
      dflocs <- df0419 %>%
        filter(macaddr %in% bothMacs[1:400]) # notice it only filters for a portion
    }
    incProgress()

    # Creating new columns 
    dflocs <<- dflocs %>%
      group_by(macaddr) %>%
      mutate(toAP = lead(ap, order_by = macaddr),
             toLoc = lead(location.y, order_by = macaddr),
             nextTime = lead(`_time`, order_by = macaddr),
             timeDiff = as.numeric(difftime(nextTime, `_time`, units = "secs"))) %>%
      rename(fromAP = ap,
             fromLoc = location.y)

    dfap <- dflocs %>%
      group_by(fromAP, toAP, fromLoc, toLoc) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq))

    incProgress()

    # Finding viable paths
    temp <- findPaths(dflocs, fLoc, tLoc)
    paths <- temp$subPaths # A->B paths
    fullP <- temp$fullPaths # full fLoc->tLoc paths
    
    incProgress()
    
    # Creating map
    m <- leaflet() %>%
      setView("map", lng = defLong, lat = defLati, zoom = zm) %>% # sets initial map zoom & center
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
      addPolygons(data = SPDF, 
                  layerId = coord$location,
                  group = coord$location,
                  weight = 1,
                  color = "gray81",
                  fillOpacity = 0.2,
                  fillColor = "azure") 
    
    # The paths are in order from most common to least common,
    # here, we are splitting the strings the paths are stored as in order
    # to bin them accordingly and attach appropriate colors to their 
    # frequencies
    splitPaths <- sapply(as.character(fullP$fullPaths), strsplit, split = ", ")
    # Matching paths to their coordinates
    pathsToCoords <- NULL
    for(i in 1:length(splitPaths)) {
      indivLocs <- data.frame(location = splitPaths[[i]], count = fullP$freq[[i]], index = i) # index for grouping lines
      pathsToCoords <- merge.data.frame(indivLocs, coord, sort = FALSE)
      
      # Creating labels
      labels <- sprintf("%s <br/ >%g",
                        toString(splitPaths[[i]]),
                        indivLocs$count[[1]]) %>% # plotted value - note to self: use temp$allPaths$path[i] instead
        lapply(htmltools::HTML)
      
      # Adding lines
      m <- m %>% 
        addPolylines(lng = pathsToCoords$long, 
                     lat = pathsToCoords$lat, 
                     layerId = pathsToCoords$index,
                     weight = pathsToCoords$count,
                     label = labels,
                     opacity = 0.5, 
                     highlightOptions = highlightOptions(
                       weight = 5,
                       color = "red",
                       fillOpacity = 1,
                       bringToFront = TRUE))
    }
    
    incProgress()
    
    # Viewing map
    output$map <- renderLeaflet(m)
    
    # Create table
    output$table <- renderDataTable(paths) 
    
    })
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

