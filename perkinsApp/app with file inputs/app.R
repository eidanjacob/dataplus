library(shiny)
library(readr)
library(raster)
library(sp)
library(deldir)
library(lubridate)
library(dplyr)
library(rgeos)
library(gridExtra)
library(ggplot2)

wallsdf <- NULL
apsdf <- NULL
eventsdf <- NULL

ui <- fluidPage(
  
  navbarPage("Single Building Wireless", id = "tabs",
             tabPanel("File Upload", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("shape", "Floor Shapes"),
                          fileInput("aps", "Access Point Positions"),
                          fileInput("events", "Event Data"),
                          actionButton("read", "Go")
                        ),
                        mainPanel(
                          htmlOutput("diagnostic")
                        )
                      )
             ),
             tabPanel("Confirm Upload",
                      uiOutput("plotList"),
                      actionButton("plotsOk", "Looking Good")
             ),
             tabPanel("Summary",
                      wellPanel(tableOutput("summaryTable"))),
             tabPanel("Charts")
  )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=1024^4)
  hideTab("tabs", "Confirm Upload")
  hideTab("tabs", "Charts")
  observeEvent(input$read, {
    hideTab("tabs", "Confirm Upload")
    # The user has indicated they are ready to proceed. Read the submitted data frames and output diagnostics.
    req(input$shape)
    req(input$aps)
    req(input$events)
    tryCatch(
      {
        wallsdf <<- read_csv(input$shape$datapath)
        apsdf <<- read_csv(input$aps$datapath)
        eventsdf <<- read_csv(input$events$datapath)
      },
      error = function(e) {stop(safeError(e))}
    )
    # Null or non-csv inputs will not make it past this point. Check that columns name match.
    if(!all(c("transX", "transY", "floor") %in% names(wallsdf)) | 
       !all(c("transX", "transY", "floor", "ap") %in% names(apsdf))){
      warning("Invalid Input! Fix before Proceeding.")
    } else {
      # Passed basic sanity check, run more advanced?
      eventsdf <- eventsdf %>% filter(ap %in% apsdf$ap)
      floors <- sort(unique(wallsdf$floor))
      if(any(floors != sort(unique(apsdf$floor)))){
        warning("Files contain information on different floors.")
      }
      msgText <- paste(as.character(nrow(apsdf)), "APs on", 
                       as.character(length(floors)), "floors.",
                       "<br/> Events:", as.character(nrow(eventsdf)))
      output$diagnostic <- renderUI(HTML(msgText))
    }
    # Split stuff by floor and draw some polygons.
    voronoiMaps <- lapply(floors, function(f){
      fwalls <- wallsdf %>% filter(floor == f)
      border <- SpatialPolygons(list(Polygons(list(Polygon(fwalls[,c("transX", "transY")])),1)))
      faps <- apsdf %>% filter(floor == f)
      fcells <- deldir(data.frame(x = faps$transX, y = faps$transY))
      w = tile.list(fcells)
      polys = vector(mode = "list", length = length(w))
      for (j in seq(along=polys)) {
        pcrds <- cbind(w[[j]]$x, w[[j]]$y)
        pcrds <- rbind(pcrds, pcrds[1,])
        polys[[j]] <- Polygons(list(Polygon(pcrds)), ID=as.character(j))
      }
      SP <- SpatialPolygons(polys)
      SP <- intersect(SP, border)
      for(x in 1:nrow(apsdf[apsdf$floor == f,])){
        SP@polygons[[x]]@ID <- as.character(x)
      }
      SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(
        x = apsdf[apsdf$floor == f,"transX"], 
        y = apsdf[apsdf$floor == f,"transY"]))
      # tag polygons with ap name
      SPDF@data$id = as.character(apsdf[apsdf$floor == f, "ap"])
      sapply(1:length(apsdf[apsdf$floor == f, "ap"]), function(x){
        SPDF@polygons[[x]]@ID <- as.character(apsdf[apsdf$floor == f, "ap"][x])
        SPDF <<- SPDF
      })
      return(SPDF)
    })
    names(voronoiMaps) = as.character(floors)
    
    # Generate floor plan plots
    output$plotList <- renderUI({
      plotOutList <- lapply(floors, function(f){
        plotName <- paste0("floor", f)
        plotOutput(plotName)
      })
      do.call(tagList, plotOutList)
    })
    for(f in floors){
      local({
        myF <- as.character(f)
        plotName <- paste0("floor", myF)
        output[[plotName]] <- renderPlot({
          plot(voronoiMaps[[myF]], main = plotName, axes = TRUE)
        })
      })
    }
    
    output$diagnostic <- renderUI(HTML(paste(msgText, "<br/> Polygons drawn, please confirm.")))
    showTab("tabs", "Confirm Upload")
  })
  
  observeEvent(input$plotsOk, {
    showTab("tabs", "Charts")
    showTab("tabs", "Summary")
    output$summaryTable <- renderTable({
      # some summary statistics
      binByAp <- eventsdf %>% count(ap, sort = TRUE)
      t(
        data.frame(
          "Unique APs" = length(unique(eventsdf$ap)),
          "Unique MACs" = length(unique(eventsdf$macaddr)),
          "Most Total Events" = paste(binByAp[1,1], ", n =", binByAp[1,2]),
          "First Event" = min(eventsdf$`_time`),
          "Last Event" = max(eventsdf$`_time`),
          "Duration" = max(eventsdf$`_time`) - min(eventsdf$`_time`)
        )
      )
    }, rownames = TRUE, colnames = FALSE)
    
    hideTab("tabs", "File Upload")
    hideTab("tabs", "Confirm Upload")
  })
}

shinyApp(ui = ui, server = server)