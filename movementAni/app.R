# This app utilizes animation to create an interesting visualization of how people move.
# Note: Does not have error handling if text inputs are invalid.

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
library(ggplot2)
library(gganimate)
library(tweenr)

# importing functions
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

df <- read_csv("../mergedData.csv") # this is only commented out to save me time. when viewing this code, put it back in
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

# polygon data -- to be used as a background
SPDF2 <- SPDF # separating the two variables because I was working with SPDF in the global, rstudio environment
SPDF2@data$id = rownames(SPDF2@data)
SPDF2.points = fortify(SPDF2, region="id")
SPDF2.df = plyr::join(SPDF2.points, SPDF2@data, by="id")

# Mess with these numbers if you want.
timeSteps = c("1hr" = 60*60, "2hr" = 2*60*60, "4hr" = 4*60*60) # in seconds
# timeSteps = c("4 hr" = 4*60*60)
delay = 2700 # in milliseconds
# ------------------------------

start.time = (min(df$`_time`))
end.time = (max(df$`_time`))

macsToLocList <- list()

end.times <- rep(end.time, length(timeSteps))

for(i in 1){ # didn't feel like replacing i -> 1 # note to self: this for loop could possibly be deleted since all it's doing is tagging ea mac with a time.window
  timeStep <- timeSteps[i]
  # Bin populations, calculate densities at each timestep, and cache for future plotting
  time.windowStart = start.time # time.window for selection
  macsToLoc <- NULL

  while(end.time > time.windowStart){

    # Filter for time interval
    selInt = interval(time.windowStart, time.windowStart + timeStep)
    thisStep <- df %>%
      filter(`_time` %within% selInt)

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
  }

  # Cache these guys away for later
  macsToLocList[[i]] <- macsToLoc
}

# mac data -- to be used for point plotting
macData <- macsToLocList[[1]]

# Define UI 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Duke Wireless Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      p("To begin, check the box with the gif you want."),
      p("Next, type in the # of devices you want to visualize."),
      p("For the Location to Location gif, keeping the number under 20 allows for fast calculations."),
      
      checkboxInput("gen", "General"),
      checkboxInput("loc", "Location to Location"),
      conditionalPanel(
        condition = "input.loc",
        textInput("from", "From location: ", value = "Perkins"),
        textInput("to", "To location: ", value = "WestUnion")
      ),
      textInput("num", "Number of devices: ", value = 20), 
      actionButton("submit", "Submit"),
      p(),
      textOutput("submitInfo")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      conditionalPanel(
        condition = "input.gen",
        imageOutput("plotGen")
      ),
      conditionalPanel(
        condition = "input.loc",
        imageOutput("plotLoc")
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # Getting all the macaddrs that actually move -- things that don't move can be hardcore students or desktop computers
  # This can be useful if you want a really animated and cool looking gif. otherwise, comment it out for realism.
  # uniqMacs <- doesMove(macData)
  
  # getting list of macaddrs
  uniqMacs <- macData$macaddr
  
  # initial polygon background
  polys <- ggplot(SPDF2, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill = "gray84") + coord_equal() + geom_path(color = "white")
  
  start.time = min(macData$time.window)
  observeEvent(input$submit, {  
  
    output$plotGen <- renderImage({
      
      
      
      isolate({
        tx <- paste("Submitted with num:", input$num)
        output$submitInfo <- renderText(tx)
        
        withProgress(message = "Loading General Map...", { # note that the loading bars are approximate and the increments likely do not add to 1
          uniqMacs <- sample(uniqMacs, input$num) # sampling num random macs
          # A temp file to save the output.
          # This file will be removed later by renderImage
          outfile1 <- tempfile(fileext='.gif')
          
          # grabbing data from the first num macaddrs and setting up for plot
          macSample <- macData %>% 
            filter(macaddr %in% uniqMacs[1:input$num]) %>%
            arrange(macaddr, realTime) %>%
            dplyr::select(lat, long, realTime, macaddr, location) %>%
            rename(x = lat, y = long, time = realTime, id = macaddr) %>%
            mutate(ease = "linear")
          macSample$realTime <- macSample$time
          # tween_elements requires dividing with the time component, so we have a separate
          # column for the seconds just to make the function work.
          # later, after the merge, we keep the minimum times associated with each frame
          # in column "min" to use as a title for the plot.
          macSample$time <- as.double(macSample$time, units='secs') 
          mac_tween <- tween_elements(macSample, "time", "id", "ease", nframes = 300) # (24hr * 60min)/300frames ~ 5 min per frame
          incProgress(detail = "Selection complete.")
          
          # creating column "min" so the plot can be titled appropriately
          minTimes <- aggregate(realTime ~ .frame, mac_tween, function(x) min(x))
          minTimes <- minTimes %>% 
            rename(min = realTime)
          mac_tween <- merge.data.frame(mac_tween, minTimes)
          
          mapGen <- polys + geom_point(data = mac_tween, aes(x = y, y = x, group = .group, frame = min), color = "blue", alpha = 0.2) + # overlaying points on polygons
            theme_bw() + xlab("Longitude") + ylab("Latitude")
          
          incProgress(detail = "Points added to polys.")
          
          gg_animate(mapGen, filename = "outfile1.gif", title_frame = TRUE, interval = 0.10)
          incProgress(detail = "gganimate complete.")
          
          list(src = "outfile1.gif",
               contentType = 'image/gif',
               width = 720,
               height = 720
               # alt = "This is alternate text"
          )
        })
      })
      
      }, deleteFile = TRUE)
      
    })
  
  observeEvent(input$submit, {
  
    output$plotLoc <- renderImage({
      
      
      
      isolate({
        tx <- paste("Submitted with num:", input$num)
        output$submitInfo <- renderText(tx)
        
        withProgress(message = "Loading Location Map...",{
          
          # A temp file to save the output.
          # This file will be removed later by renderImage
          outfile2 <- tempfile(fileext='.gif')
          
          uniqMacs <- macData %>% # filtering for macs that have visited locations to speed up runtimes
            filter(location %in% c(input$from, input$to))
          uniqMacs <- unique(uniqMacs$macaddr)
          
          n <- 0
          macsDF <- NULL # whether this is or is not a list is questionable
          for(i in 1:length(uniqMacs)) { # grabbing data from macaddrs that visited certain places
            if(n == input$num | i == 300) { # second if condition is just to make sure code will run in a reasonable timeframe
              break
            }
            truncatList <- findIndex(uniqMacs[[i]], macData, input$from, input$to)
            if(!is.null(truncatList)) {
              # changing the times so that they can all be viewed at once
              timeDiff <- truncatList[[3]]$time.window[[1]] - start.time
              truncatList[[3]]$realTime <- sapply(truncatList[[3]]$realTime, FUN = function(x) {x - timeDiff})
              
              macsDF <- rbind(macsDF, truncatList[[3]])
              n <- n + 1
            }
            incProgress(amount = 1/(as.numeric(input$num)+100))
          }
          incProgress(detail = "Selection complete.")
          
          # setting up for tweenr
          macLocs <- macsDF %>% 
            arrange(macaddr, realTime) %>%
            dplyr::select(lat, long, realTime, macaddr, location) %>%
            rename(x = lat, y = long, time = realTime, id = macaddr) %>%
            mutate(ease = "linear")
          
          loc_tween <- tween_elements(macLocs, "time", "id", "ease", nframes = 300) %>% # (24hr * 60min)/300frames ~ 5 min per frame
            mutate(year = round(time), macaddr = .group)
          
          mapLoc <- polys + geom_point(data = loc_tween, aes(x = y, y = x, group = macaddr, frame = .frame), color = "blue", alpha = 0.3) + # overlaying points on polygons
            theme_bw() + xlab("Longitude") + ylab("Latitude")
          incProgress(detail = "Points added to polys.")
          
          gg_animate(mapLoc, filename = "outfile2.gif", title_frame = TRUE, interval = 0.10)
          incProgress(detail = "gganimate complete.")
          
          list(src = "outfile2.gif",
               contentType = 'image/gif',
               width = 720,
               height = 720
               # alt = "This is alternate text"
          )
        }) })
    }, deleteFile = TRUE)
      
    })
  
}

# Create Shiny app ----
shinyApp(ui, server)