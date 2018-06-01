# speedR.R
# Takes a macaddr and returns vector of (time differences between records) / (distance between records), i.e. average speeds
# Intended use to flag suspicious behavior
# Requires coordinate data
require(dplyr)
require(geosphere)
speedR <- function(mac, data){
  unsorted <- data[data$macaddr == mac,]
  sorted <- unsorted[ order( unsorted$time), ]
  n <- length(sorted$time)
  speeds <- rep(NA, n-1)
  for(i in 1:n-1){
    timediff <- sorted$time[i+1] - sorted$time[i]
    c1 <- c(sorted$lat[i], sorted$long[i])
    c2 <- c(sorted$lat[i+1], sorted$long[i+1])
    distance <- distHaversine(c1, c2)
    speeds[i] = distance / timediff
  }
  return(speeds)
}