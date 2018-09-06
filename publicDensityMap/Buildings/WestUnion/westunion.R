# WestUnion - This is just a test to see if it works with multiple building maps

library(shiny)
library(readr)
library(raster)
library(deldir)
library(dplyr)
library(rgeos)
library(ggplot2)
library(maps)

createMap <- function(data){
  
  test <- data.frame(x = c(1,2,3),
                     y = c(1,2,3))
  
  buildingMap <- ggplot(test, aes(x=x, y=y)) + 
    geom_point()
  
  return(buildingMap)
}

