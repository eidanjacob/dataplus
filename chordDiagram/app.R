library(shiny)
library(readr)
library(circlize)
library(migest)
library(dplyr)

# Global vars
eventsdf <- NULL
coord <- read_csv("../locationsToCoordinates.csv")
coord <- coord[order(coord$location),] # alphabetize location - coordinate dictionary
validLocations <- read_csv("../allAPs.csv") # aps <-> locations

ui <- navbarPage("Chord Diagram Generator",
                 tabPanel("File Upload",
                          fileInput("events", "Event Data"),
                          actionButton("read", "Go")
                 ),
                 tabPanel("Diagram",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("chordSlider"),
                              uiOutput("chordCheckbox")
                            ),
                            
                            mainPanel(
                              plotOutput("circleChart")
                            )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  options(shiny.maxRequestSize=1024^6)
  observeEvent(input$read, {
    req(input$events)
    tryCatch({
      eventsdf <<- read_csv(input$events$datapath)
    },
    error = function(e) {stop(safeError(e))}
    )
    
    # match aps to locations, merge for coordinates
    eventsdf <<- eventsdf[!is.na(eventsdf$ap),] # remove observations with no ap
    
    # Some aps are in splunk data with name, some with number - code below matches location using whichever is available
    nameMatch = which(validLocations$APname %in% eventsdf$ap) # find which aps have their name in the data
    numMatch = which(validLocations$APnum %in% eventsdf$ap) # find which aps have their number in the data
    validLocations$ap = c(NA) # new "flexible" column to store either name or number
    validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
    validLocations$ap[numMatch] = validLocations$APnum[numMatch]

    validLocations <- merge(coord, validLocations) # link coordinates to locations
    # use the new "flexible" ap variable to merge coordinates onto df
    eventsdf <<- merge(eventsdf, validLocations, by = "ap") # this is the slow step

    start.time <<- min(eventsdf$`_time`)
    end.time <<- max(eventsdf$`_time`)
    
    output$chordSlider <- renderUI({
      sliderInput("timeBounds", "Interval", min = start.time, max = end.time, value = c(start.time, end.time))
    })
    
    output$chordCheckbox <- renderUI({
      checkboxGroupInput("chordLocations", "Locations", choices = unique(eventsdf$locations))
    })
    
  })
  
  output$circleChart <- renderPlot({
    selection <- eventsdf %>%
      filter(`_time` >= input$timeBounds[1] & `_time` <= input$timeBounds[2]) %>%
      filter(location.y %in% c("Perkins", "WestUnion", "BryanCenter", "Fuqua"))
    
    # Create adjacency list.
    locs <- unique(selection$location.y)
    adjList = data.frame(from = rep(locs, times = length(locs)),
                         to = rep(locs, each = length(locs)),
                         value = 1,
                         stringsAsFactors = FALSE)
    chordDiagram(adjList)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

