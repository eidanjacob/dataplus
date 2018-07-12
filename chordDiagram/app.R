library(shiny)
library(readr)
library(circlize)
library(migest)
library(dplyr)
library(lubridate)
# Global vars
eventsdf <- adjListLocation <- adjListCampus <- locs <- NULL
coord <- read_csv("../locationsToCoordinates.csv")
coord <- coord[order(coord$location),] # alphabetize location - coordinate dictionary
validLocations <- read_csv("../allAPs.csv") # aps <-> locations

ui <- fluidPage(
  titlePanel("Chord Diagram Generator"),
  column(6, wellPanel(
    fileInput("events", "Event Data"),
    actionButton("read", "Go")
  )),
  column(6, wellPanel(
    uiOutput("chordCheckbox"),
    checkboxInput("campus", "By Campus"),
    uiOutput("generateDiag")
  )),
  plotOutput("circleChart")
)

server <- function(input, output) {
  
  options(shiny.maxRequestSize=1024^6)
  observeEvent(input$read, {
    req(input$events)
    tryCatch({
      eventsdf <<- read_csv(input$events$datapath)
    }, error = function(e) {
      stop(safeError(e))
    })
    # Match aps to locations, merge for coordinates
    eventsdf <<- eventsdf[!is.na(eventsdf$ap),] # Remove observations with no ap
    # Some aps are in splunk data with name, some with number - code below matches location using whichever is available
    nameMatch = which(validLocations$APname %in% eventsdf$ap) # Find which aps have their name in the data
    numMatch = which(validLocations$APnum %in% eventsdf$ap) # Find which aps have their number in the data
    validLocations$ap = c(NA) # New "flexible" column to store either name or number
    validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
    validLocations$ap[numMatch] = validLocations$APnum[numMatch]
    validLocations <- merge(coord, validLocations) # Link coordinates to locations
    # Use the new "flexible" ap variable to merge coordinates onto df
    eventsdf <<- merge(eventsdf, validLocations, by = "ap") # This is the slow step
    locationNs <- summarise(group_by(eventsdf, location.y), n()) # Count events by location
    locationNs <- locationNs[order(locationNs$`n()`, decreasing = TRUE),] # Sort descending
    N <- min(10, length(locationNs$location.y)) # Show at most 10 locations
    locs <<- locationNs$location.y[1:N] 
    names(locs) <<- locs # Add labels for checkbox inputs.
    output$chordCheckbox <- renderUI({
      checkboxGroupInput("chordLocations", "Locations", choices = locs, inline = TRUE, selected = locs)
    })
    # Adjacency list
    adjListLocation <<- data.frame(from  = rep(locs, times = length(locs)),
                                   to    = rep(locs, each  = length(locs)),
                                   value = 0, # To be filled in below
                                   stringsAsFactors = FALSE)
    adjListCampus <<- data.frame(from  = rep(c("East", "West", "Central", "Off"), times = 4),
                                 to    = rep(c("East", "West", "Central", "Off"), each  = 4),
                                 value = 0)
    macs <- unique(eventsdf$macaddr)
    lapply(macs, function(mac){
      subset <- filter(eventsdf, macaddr == mac)
      subset <- subset[order(subset$`_time`),]
      lapply(1:(nrow(subset)-1), function(i){
        index <- which(adjListLocation$from == subset$location.y[i] & adjListLocation$to == subset$location.y[i+1])
        if(length(index) == 1){
          if(adjListLocation$to[index] != adjListLocation$from[index]){
            adjListLocation$value[index] <<- adjListLocation$value[index] + 1
          }
        }
        index <- which(adjListCampus$from == subset$campus[i] & adjListCampus$to == subset$campus[i+1])
        if(length(index) == 1){
          if(adjListCampus$to[index] != adjListCampus$from[index]){
            adjListCampus$value[index] <<- adjListCampus$value[index] + 1
          }
        }
      })
    })
  })
  
  output$circleChart <- renderPlot({
    if(input$campus){
      chordDiagram(adjListCampus)
    } else {
      req(input$chordLocations)
      selLoc <- input$chordLocations
      adjListLocationSubset <- adjListLocation %>%
        filter(to   %in% selLoc) %>%
        filter(from %in% selLoc)
      chordDiagram(adjListLocationSubset)
    }
  },
  width = 1024, height = 1024, res = 128)
}

# Run the application 
shinyApp(ui = ui, server = server)