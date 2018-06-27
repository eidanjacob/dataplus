library(shiny)
library(readr)
library(raster)
library(sp)
library(deldir)
library(lubridate)
library(dplyr)
library(rgeos)
library(ggplot2)
library(leaflet)
library(animation)
library(gganimate)

# Global Vars
wallsdf <- apsdf <- eventsdf <- voronoiMaps <- floors <- plots <- imagesToShow <- NULL
timeSteps <- c("30 min" = 1800, "1 hr" = 3600, "2 hr" = 7200)
ani.options(convert = "C:/Autodesk/ImageMagick-7.0.8-Q16/convert")

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
             tabPanel("Charts",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("stepSize", "Time Step", choices = names(timeSteps)),
                          sliderInput("frameDelay", "Frame Delay (ms)",
                                      min = 100, max = 5000, value = 1000,
                                      step = 50),
                          uiOutput("floorCheckBox"),
                          actionButton("generateGIF", "Run Animation")
                        ),
                        mainPanel(
                          uiOutput("chartDisplay")
                        )
                      )
             )
  )
)

server <- function(input, output) {
  
  options(shiny.maxRequestSize=1024^4)
  hideTab("tabs", "Confirm Upload")
  hideTab("tabs", "Summary")
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
      floors <<- sort(unique(wallsdf$floor))
      if(any(floors != sort(unique(apsdf$floor)))){
        warning("Files contain information on different floors.")
      }
      msgText <- paste(as.character(nrow(apsdf)), "APs on", 
                       as.character(length(floors)), "floors.",
                       "<br/> Events:", as.character(nrow(eventsdf)))
      output$diagnostic <- renderUI(HTML(msgText))
    }
    # Split stuff by floor and draw some polygons.
    voronoiMaps <<- lapply(floors, function(f){
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
      SP <- raster::intersect(SP, border)
      for(x in 1:nrow(apsdf[apsdf$floor == f,])){
        SP@polygons[[x]]@ID <- as.character(x)
      }
      SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(
        x = apsdf[apsdf$floor == f,"transX"], 
        y = apsdf[apsdf$floor == f,"transY"]))
      # tag polygons with ap name
      SPDF@data$id = as.character(apsdf[apsdf$floor == f, "ap"][[1]])
      sapply(1:length(apsdf[apsdf$floor == f, "ap"]), function(x){
        SPDF@polygons[[x]]@ID <- as.character(apsdf[apsdf$floor == f, "ap"][[1]][x])
        SPDF <<- SPDF
      })
      return(SPDF)
    })
    names(voronoiMaps) <<- as.character(floors)
    
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
  
  suppressWarnings(observeEvent(input$plotsOk, {
    showTab("tabs", "Charts")
    showTab("tabs", "Summary")
    binByAp <- eventsdf %>% count(ap, sort = TRUE)
    output$summaryTable <- renderTable({
      # some summary statistics
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
    
    output$floorCheckBox <- renderUI({
      checkboxGroupInput("floorsToView", "Floors", choices = floors, selected = floors)
    })
    
    # Make calculations for choropleths.
    startTime <- min(eventsdf$`_time`)
    endTime <- min(eventsdf$`_time`)
    
    # convert polys to ggplot2 usable format and save in list
    
    fortifiedList <- lapply(voronoiMaps, function(vor){
      fortified <- fortify(vor, region = "id")
    })
    names(fortifiedList) = as.character(floors)
    
    # intended result of these nested lapplys is a list of (indexed by timeStep size) of lists 
    # (indexed by time bin) of plots (indexed by floor), to be used in future animations.
    
    plots <<- lapply(timeSteps, function(delta){
      breaks <- seq(startTime, endTime, delta)
      binnedEvents <- eventsdf[,"ap"] # match events to appropriate bin
      binnedEvents$bin <- cut_interval(as.numeric(eventsdf$`_time`), length = delta, ordered_result = TRUE)
      chartData <- summarise(group_by(binnedEvents, ap, bin), n()) %>%
        mutate(binStart = bin[[1]])
      bins <- unique(binnedEvents$bin)
      frames <- lapply(floors, function(f){
        fortified <- fortifiedList[[as.character(f)]]
        ready <- left_join(fortified, chartData, by = c("id" = "ap")) 
        ready$`n()`[which(is.na(ready$`n()`))] <- 0
        p <- ggplot() +
          geom_polygon(data = ready, aes(fill = `n()`,
                                         x = long,
                                         y = lat,
                                         frame = factor(as.numeric(bin)),
                                         group = group)) +
          scale_fill_gradient(low = "gray", high = "red") +
          geom_path(data = ready, aes(x = long,
                                      y = lat,
                                      group = group),
                    color = "white",
                    size = 1)
        return(p)
      })
      names(frames) = as.character(floors)
      return(frames)
    })
    
    hideTab("tabs", "File Upload")
    hideTab("tabs", "Confirm Upload")
  }))
  
  observeEvent(input$generateGIF, {
    myPlots <- plots[[input$stepSize]]
    imagesToShow <<- NULL
    lapply(input$floorsToView, function(f){
      imageName <- paste0("images", as.character(f))
      filename = paste0(imageName, ".gif")
      imagesToShow <<- c(imagesToShow, imageName)
      print(imagesToShow)
      gganimate(myPlots[[as.character(f)]], filename = filename)
      output[[imageName]] <- renderImage(list(src = filename))
    })
    
    output$chartDisplay <- renderUI({
      print(imagesToShow)
      image_output_list <- lapply(imagesToShow, function(img){
        imageOutput(img)
      })
      print(image_output_list)
      do.call(tagList, image_output_list)
    })
    
    print("end")
  })
  
  
  
}

shinyApp(ui = ui, server = server)
