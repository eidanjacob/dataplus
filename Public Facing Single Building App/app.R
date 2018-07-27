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
library(tidyr)
library(RColorBrewer)
library(maps)

# Global Vars
eventsdf <- voronoiSPDF <- NULL

apsdf <- read_csv("../perkins_aps")
wallsdf<- read_csv("../perkins_walls")
floors = length(unique(apsdf$floor))
fn = length(floors)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel("Text and Stuff"),
    mainPanel("A pretty picture")
  )
)

server <- function(input, output){
  
  reactive(eventsdf, {
    xRange <- max(
      sapply(floors, function(f){
        max(wallsdf$transX[wallsdf$floor == f]) - 
          min(wallsdf$transX[wallsdf$floor == f])
      })
    )
    yRange <- max(
      sapply(floors, function(f){
        max(wallsdf$transY[wallsdf$floor == f]) - 
          min(wallsdf$transY[wallsdf$floor == f])
      })
    )
    offsets <- data.frame(
      t(
        sapply(1:fn, function(i){
          xOff <- (xRange+20) * ((i - 1) %% 2)
          yOff <- (yRange) * floor(i/2 - 1/2)
          return(c(xOff, yOff))
        })
      )
    )
    rownames(offsets) <- as.character(floors)
    colnames(offsets) <- c("xOff", "yOff")
    
    xMin <- min(wallsdf$transX)
    yMin <- min(wallsdf$transY)
    labelLocations <- offsets %>%
      mutate(xOff = xOff + xMin + xRange/2) %>%
      mutate(yOff = yOff + yMin + yRange + 50)
    
    wallsdf <- wallsdf %>%
      mutate(transX = transX + offsets[as.character(wallsdf$floor), "xOff"]) %>%
      mutate(transY = transY + offsets[as.character(wallsdf$floor), "yOff"])
    
    apsdf <- apsdf %>%
      mutate(transX = transX + offsets[as.character(apsdf$floor), "xOff"]) %>%
      mutate(transY = transY + offsets[as.character(apsdf$floor), "yOff"])
    
    # Split stuff by floor and draw some polygons.
    borderList <- lapply(floors, function(f){
      fwalls <- wallsdf %>% filter(floor == f)
      border <- Polygons(list(Polygon(fwalls[,c("transX", "transY")])),as.character(f))
    })
    joined <- SpatialPolygons(borderList)
    fcells <- deldir(data.frame(x = apsdf$transX, y = apsdf$transY))
    w = tile.list(fcells)
    polys = vector(mode = "list", length = length(w))
    for (j in seq(along=polys)) {
      pcrds <- cbind(w[[j]]$x, w[[j]]$y)
      pcrds <- rbind(pcrds, pcrds[1,])
      polys[[j]] <- Polygons(list(Polygon(pcrds)), ID=as.character(j))
    }
    SP <- SpatialPolygons(polys)
    for(x in 1:nrow(apsdf)){
      SP@polygons[[x]]@ID <- as.character(x)
    }
    SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(
      x = apsdf$transX, 
      y = apsdf$transY))
    # tag polygons with ap name
    SPDF@data$id = as.character(apsdf[, "ap"][[1]])
    sapply(1:length(apsdf[, "ap"]), function(x){
      SPDF@polygons[[x]]@ID <- as.character(apsdf[, "ap"][[1]][x])
      SPDF <<- SPDF
    })
    voronoiSPDF <- raster::intersect(SPDF, joined)
    output$floorPlan <- renderPlot({
      plot(voronoiSPDF, main = "Floor Plan Generated from Uploaded Files")})
    output$diagnostic <- renderUI(HTML(
      paste(msgText, "<br/> Polygons drawn, please confirm.")))
    showTab("tabs", "Confirm Upload")
    
    chartData <- summarise(group_by(eventsdf, ap), n())
    
    chartData$n <- chartData$`n()` * as.numeric((chartData$n < 5))
    
    # convert polys to ggplot2 usable format and save in list
    fortified <- fortify(voronoiSPDF, region = "id")
    
    
    ready <- left_join(fortified, chartData, by = c("id" = "ap"))
    ggplot() +
      geom_polygon(data = ready, aes(fill = n,
                                     x = long,
                                     y = lat,
                                     group = group)
      ) +
      scale_fill_gradient(low = "#e6e6Fa", high = "#4b0082") +
      coord_fixed() +
      geom_path(data = ready, aes(x = long,
                                  y = lat,
                                  group = group),
                color = "gray",
                size = 1) + 
      theme_bw() + xlab("Feet") + ylab("Feet") +
      geom_text(aes(x = labelLocations[,1],
                    y = labelLocations[,2],
                    label = paste("Floor", floors)))
    
  })
  
}

shinyApp(ui = ui, server = server)
