library(shiny)
library(readr)
library(raster)
library(deldir)
library(dplyr)
library(rgeos)
library(ggplot2)
library(maps)

# Global Vars
apsdf <- read_csv("./apData.csv")
wallsdf<- read_csv("./buildingData.csv")
floors = unique(apsdf$floor)

threshold = 10 # APs with this many or fewer events will display 0 
refresh = 1 # Refresh rate in minutes

ui <- fluidPage(
  plotOutput("myPlot")
)

server <- function(input, output){
  observe({
    invalidateLater(refresh * 60000)
    eventsdf <- read_csv("./eventData.csv")
    # Finds the largest floor's span along each dimension.
    xRange <- max(
      sapply(floors, function(f){
        max(wallsdf$X[wallsdf$floor == f]) - 
          min(wallsdf$X[wallsdf$floor == f])
      })
    )
    yRange <- max(
      sapply(floors, function(f){
        max(wallsdf$Y[wallsdf$floor == f]) - 
          min(wallsdf$Y[wallsdf$floor == f])
      })
    )
    # Offsets calculated to arrange each floor to avoid overplotting.
    offsets <- data.frame(
      t(
        sapply(1:length(floors), function(i){
          xOff <- (xRange+20) * ((i - 1) %% 2)
          yOff <- (yRange) * floor(i/2 - 1/2)
          return(c(xOff, yOff))
        })
      )
    )
    rownames(offsets) <- as.character(floors)
    colnames(offsets) <- c("xOff", "yOff")
    # (xMin, yMin) is a sort of 'origin' point
    xMin <- min(wallsdf$X)
    yMin <- min(wallsdf$Y)
    # Want to label floors, so calculate location of labels.
    labs <- offsets %>%
      mutate(xOff = xOff + xMin + xRange/2) %>%
      mutate(yOff = yOff + yMin + yRange + 50)
    names(labs) <- c("X", "Y")
    labs$text <- paste("Floor:", floors)
    # Add the offsets.
    wallsdf <- wallsdf %>%
      mutate(X = X + offsets[as.character(wallsdf$floor), "xOff"]) %>%
      mutate(Y = Y + offsets[as.character(wallsdf$floor), "yOff"])
    apsdf <- apsdf %>%
      mutate(X = X + offsets[as.character(apsdf$floor), "xOff"]) %>%
      mutate(Y = Y + offsets[as.character(apsdf$floor), "yOff"])
    # Draw a polygon around each floor.
    borderList <- lapply(floors, function(f){
      fwalls <- wallsdf %>% filter(floor == f)
      border <- Polygons(list(Polygon(fwalls[,c("X", "Y")])),as.character(f))
    })
    joined <- SpatialPolygons(borderList)
    # Draw voronoi cells around each Access Point.
    fcells <- deldir(data.frame(x = apsdf$X, y = apsdf$Y))
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
      x = apsdf$X, 
      y = apsdf$Y))
    # Tag voronoi cells with ap name
    SPDF@data$id = as.character(apsdf[, "ap"][[1]])
    sapply(1:length(apsdf[, "ap"]), function(x){
      SPDF@polygons[[x]]@ID <- as.character(apsdf[, "ap"][[1]][x])
      SPDF <<- SPDF
    })
    # Intersect voronoi cells with polygons of floors
    voronoiSPDF <- raster::intersect(SPDF, joined)
    # Count the number of events per access point. This is a crude measure of network load but it's good enough for students.
    chartData <- summarise(group_by(eventsdf, ap), n())
    # If there are 10 or fewer events, display 0 events. (Don't want to point out where people may be sitting alone).
    chartData$Utilization <- chartData$`n()` * as.numeric((chartData$`n()` > threshold))
    # Convert polygons to ggplot2-usable format and save in list
    fortified <- fortify(voronoiSPDF, region = "id")
    ready <- left_join(fortified, chartData, by = c("id" = "ap"))
    output$myPlot <- renderPlot({
      ggplot() + geom_polygon(data = ready, aes(fill = Utilization, x = long, y = lat, group = group)) +
        scale_fill_gradient(low = "#e6e6Fa", high = "#4b0082") +
        coord_fixed() +
        geom_path(data = ready, aes(x = long, y = lat, group = group), color = "gray", size = 1) + 
        theme_bw() +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
              axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
        geom_text(aes(x = X, y = Y, label = text), data = labs)
    })
  })
}

shinyApp(ui = ui, server = server)