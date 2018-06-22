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

floors = 0:3

# Convert cell info to spatial data frames (polygons)
ws <- list(tile.list(cells0), tile.list(cells1), tile.list(cells2), tile.list(cells3))
SPDFlist <- lapply(floors, function(i){
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
    SP@polygons[[x]]@ID <- as.character(x)
  }
  SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(
    x = aps[aps$floor == i,"transX"], 
    y = aps[aps$floor == i,"transY"]))
  # tag polygons with ap name
  SPDF@data$id = as.character(aps[aps$floor == i, "ap"])
  sapply(1:length(aps[aps$floor == i, "ap"]), function(x){
    SPDF@polygons[[x]]@ID <- as.character(aps[aps$floor == i, "ap"][x])
    SPDF <<- SPDF
  })
  return(SPDF)
})

binnedTable <- data.frame()
startTime <- min(events$`_time`)
endTime <- max(events$`_time`)
lastBin <- endTime
timeStep <- 3600
iter <- startTime
 
# while(iter < endTime){
# 
#   # Filter for time interval
#   selInt = interval(iter, iter + timeStep)
#   thisStep <- events %>%
#     filter(`_time` %within% selInt)
# 
#   # Bin to AP
#   numMACs <- sapply(aps$ap, function(i) {nrow(thisStep %>% filter(ap == i))})
#   binnedTable <- rbind(binnedTable, data.frame(macs = numMACs, ap = aps$ap, floor = aps$floor, time = c(iter)))
#   iter <- iter + timeStep
#   lastBin <- iter
# }
# write.csv(binnedTable, "./binnedTable", row.names = FALSE)
binnedTable <- read_csv("./binnedTable")
pal <- colorNumeric("Reds", binnedTable$macs)

joinedList <- list()

for(i in floors){
  fortified <- fortify(SPDFlist[[i+1]], region = "id")
  joined <- join(fortified, SPDFlist[[i+1]]@data, by = "id")
  joinedList[[i+1]] = joined
}


ui <- fluidPage(
  titlePanel("Perkins Library Wireless Data"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("time", "Time", min = startTime, max = lastBin,
                  value = startTime, animate = animationOptions(interval = 1000),
                  step = dseconds(timeStep))
    ),
    
    mainPanel(
      # ideas for this thing: tween choropleth, track individual macaddrs over time (filter to a small number for readability)
      plotOutput("floor0plot")
    )
  )
)

server <- function(input, output) {
  
  output$floor0Plot <- renderPlot({
    ggplot(data = joinedList[[0+1]], aes(long, lat, group = group, 
                              fill = (binnedTable %>% 
                                        filter(floor == 0) %>% 
                                        filter(time == startTime))$macs)) + 
      geom_polygon() + 
      geom_path(color = "white")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

