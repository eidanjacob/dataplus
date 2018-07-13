# speedR.R
# Takes a macaddr and returns vector of (time differences between records) / (distance between records), i.e. average speeds
# also returns time-sorted list of device appearances
# Intended use to flag suspicious behavior
# Requires coordinate data
require(dplyr)
require(geosphere)
require(lubridate)
speedR <- function(mac, data){
  unsorted <- data[data$macaddr == mac,]
  sorted <- unsorted[ order(unsorted$`_time`), ]
  rownames(sorted) <- c()
  n <- length(sorted$`_time`)
  if(n < 2){
    return()
  }
  speeds <- sapply(1:(n-1), function(i){
    a = sorted$`_time`[i+1]
    b = sorted$`_time`[i]
    timediff = difftime(a, b)
    if(timediff <= 0){
      return(Inf)
      next
    }
    c1 <- c(sorted$lat[i], sorted$long[i])
    c2 <- c(sorted$lat[i+1], sorted$long[i+1])
    distance <- distHaversine(c1, c2)
    return(distance / as.numeric(timediff, units = "secs"))
  })
  return(list(speeds, sorted))
}