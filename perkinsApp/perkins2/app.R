library(shiny)
library(readr)
library(ggplot2)
library(png)
library(dplyr)
library(plotly)
library(lubridate)
library(deldir)
library(sp)
library(rgeos)
library(raster)
library(leaflet)
library(plyr)

ap_xy <- read_csv("../perkins_aps")
walls <- read_csv("../perkins_walls")
events <- read_csv("../../perkinsOnly.csv")
aps <- data.frame("ap" = unique(events$ap))
aps <- merge(aps, ap_xy)
events <- merge(events, aps)

# Creating polygons for the walls of the library.
walls0 <- SpatialPolygons(list(Polygons(list(Polygon(walls[walls$floor == 0, c("transX", "transY")])),1)))
walls1 <- SpatialPolygons(list(Polygons(list(Polygon(walls[walls$floor == 1, c("transX", "transY")])),1)))
walls2 <- SpatialPolygons(list(Polygons(list(Polygon(walls[walls$floor == 2, c("transX", "transY")])),1)))
walls3 <- SpatialPolygons(list(Polygons(list(Polygon(walls[walls$floor == 3, c("transX", "transY")])),1)))
wallList <- list(walls0, walls1, walls2, walls3)

# Creating voronoi cells for each floor.
cells0 <- deldir(aps[aps$floor == 0, c("x" = "transX", "y" = "transY")])
cells1 <- deldir(aps[aps$floor == 1, c("x" = "transX", "y" = "transY")])
cells2 <- deldir(aps[aps$floor == 2, c("x" = "transX", "y" = "transY")])
cells3 <- deldir(aps[aps$floor == 3, c("x" = "transX", "y" = "transY")])

# Convert cell info to spatial data frames (polygons)
ws <- list(tile.list(cells0), tile.list(cells1), tile.list(cells2), tile.list(cells3))
SPDFlist <- lapply(0:3, function(i){
  w <- ws[[i+1]]
  sps <- wallList[[i+1]]
  polys <- vector(mode="list", length=length(w))
  for (j in seq(along=polys)) {
    pcrds <- cbind(w[[j]]$x, w[[j]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[j]] <- Polygons(list(Polygon(pcrds)), ID=as.character(j))
  }
  SP <- SpatialPolygons(polys)
  SP <- intersect(SP, sps)
  for(x in 1:nrow(aps[aps$floor == i,])){
    SP@polygons[[x]]@id <- as.character(x)
  }
  SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(
    x = aps[aps$floor == i,"transX"], 
    y = aps[aps$floor == i,"transY"]))
  # tag polygons with ap name
  SPDF@data$id = as.character(aps[aps$floor == i, "ap"])
  sapply(1:length(aps[aps$floor == i, "ap"]), function(x){
    SPDF@polygons[[x]]@id <- as.character(aps[aps$floor == i, "ap"][x])
    SPDF <<- SPDF
  })
  return(SPDF)
})

binnedTable <- data.frame()
startTime <- min(events$`_time`)
endTime <- max(events$`_time`)
timeStep <- 3600

while(startTime < endTime){
  
  # Filter for time interval
  selInt = interval(startTime, startTime + timeStep)
  thisStep <- events %>%
    filter(`_time` %within% selInt)
  
  # Bin to AP
  numMACs <- sapply(aps$ap, function(i) {nrow(thisStep %>% filter(ap == i))})
  binnedTable <- rbind(binnedTable, data.frame(macs = numMACs, ap = aps$ap, time = c(startTime)))
  startTime <- startTime + timeStep
}

pal <- colorNumeric("YlOrRd", binnedTable$macs)

ui <- fluidPage(
  titlePanel("Perkins Library Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      #
    ),
    
    mainPanel(
      # ideas for this thing: tween chloropleth, track individual macaddrs over time (filter to a small number for readability)
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    f <- fortify(SPDFlist[[1]], region = "id")
    bah <- join(f, SPDFlist[[1]]@data, by = "id")
    ggplot(data = bah, aes(x = long, y = lat)) + geom_polygon()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

