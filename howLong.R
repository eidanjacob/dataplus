# Script that calculates how long a macaddr stayed in one place
howLong <- function(mac, currMacs) {
  macLocs <- currMacs %>%
    filter(macaddr == mac)
  
  macLocs$realTimeEnd <- lead(macLocs$realTime, 1)
  macLocs <- macLocs %>% 
    mutate(timeDiff = difftime(macLocs$realTimeEnd, macLocs$realTime)) # timeDiff is in seconds
  
  n <- 1
  i <- 1
  # function that adds a column that has numbers that will increment when a value before it changes
  col <- sapply(macLocs$location, function(x) { 
    if(i != 1) {
      if (x != macLocs[i-1, ]$location) {
        n <<- n + 1
        i <<- i + 1
        return(n)
      } else {
        i <<- i + 1
        return(n)
      }
    } else {
      i <<- i + 1
      return(n)
    }
  })
  
  macLocs$id <- col
  
  timeStayed <- aggregate(timeDiff ~ id, data = macLocs, sum)
  
  timeStayed <- macLocs %>% 
    group_by(id, location) %>% 
    summarise(totalTime = sum(timeDiff))
  
  timeStart <- macLocs[!duplicated(macLocs$id), ] 
  
  timeStayed <- cbind(timeStayed, startTimeGeneral = timeStart$time.window, startTimeReal = timeStart$realTime, macaddr = timeStart$macaddr)
  timeStayed[is.na(timeStayed)] <- 0
  View(timeStayed)
  return(timeStayed)
}

# mac <- "dc:a9:04:85:2d:63"
# macsLocs <- howLong(mac, currMacs)
# 
# # filtering for locations where it is most likely the device stayed instead of just passing through
# macsLocs <- macsLocs[macsLocs$totalTime > 60 * 10, ]
# 
# View(macsLocs)



