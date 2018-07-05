library(dplyr)
library(lubridate)
library(data.table) # for rleid

# Script that calculates how long a macaddr stayed in some places
# mac is a macaddr, macdf is dataframe of events
howLong <- function(mac, macdf) {
  macLocs <- macdf %>%
    filter(macaddr == mac)
  
  macLocs$realTimeEnd <- lead(macLocs$realTime, 1)
  macLocs <- macLocs %>% 
    mutate(timeDiff = as.numeric(difftime(macLocs$realTimeEnd, macLocs$realTime)), units = "secs") # timeDiff is in seconds

  # function that adds a column that has numbers that will increment when the location before it changes
  col <- rleid(macLocs$location)
  
  macLocs$id <- col
  
  # if a macaddr has only visited one/less than one location
  if(length(macLocs$id) <= 1) {
    return(NULL)
  }
  
  timeStayed <- aggregate(timeDiff ~ id, data = macLocs, sum)
  
  # creating column that has the amount of time spent at a locations, in order of visited locations, repeats allowed.
  timeStayed <- macLocs %>% 
    group_by(id, location) %>% 
    summarise(totalTime = sum(timeDiff), endTimeReal = max(realTime))
  
  timeStart <- macLocs[!duplicated(macLocs$id), ] 
  
  # creating dataframe with other useful columns
  timeStayed <- cbind(timeStayed, 
                      startTimeGeneral = timeStart$time.window, 
                      startTimeReal = timeStart$realTime, 
                      macaddr = timeStart$macaddr)
  timeStayed$totalTime[is.na(timeStayed$totalTime)] <- 0
  return(timeStayed)
}

# This script takes a macaddr, a dataframe of general event information,
# and returns a list of four things: 
# 1. a dataframe that tells how long a macaddr has stayed for each location
# 2. a shortened dataframe of the locations stayed after it has gone through the filtering
# 3. the shortened dataframe of visited locations used in drawing lines
# 4. the number of devices on the map
# it returns null when the macaddr did not visit the locations properly.
# IDEAS: fromLoc and toLoc are groups of locations so you can see how people from dorms go to WU, etc.
findIndex <- function(mac, macdf, fromLoc, toLoc, fromInte=60*5, toInte=60*5, betweenInte=60*10, distInte=2, numOnMap=0) {
  # filtering to find each location a macaddr has visited
  macsTime <- macdf %>% 
    filter(macaddr == mac)

  # filtering to find each location a macaddr has stayed
  timeStayed <- howLong(mac, macsTime)
  if(is.null(timeStayed)) {
    return(NULL)
  }
  # filtering for locations based on the time intervals above
  n1 <- which(timeStayed$location == fromLoc & timeStayed$totalTime > fromInte)
  n2 <- which(timeStayed$location == toLoc & timeStayed$totalTime > toInte)
  n3 <- which(timeStayed$location != fromLoc & timeStayed$location != toLoc & timeStayed$totalTime > betweenInte)
  n4 <- c(n1, n2, n3)
  n4 <- sort(n4) # note that the last location the macaddr visited visited is cut off
  macsLocs <- timeStayed[n4, ]
  
  # dealing with the case that fromLoc is empty string by grabbing everything to see how ppl get to toLoc
  # if someone moves from A to B, and then A to B again in a later time, maybe display both times?
  # currently the only way to visualize both paths is by changing the time inputs
  if(fromLoc == "") {
    if(!toLoc %in% macsLocs$location) {
      return(NULL)
    }
    indexFrom <- 1
    indexTo <- match(toLoc, macsTime$location)
    if(indexFrom == indexTo) {
      indexTo <- match(toLoc, macsTime[2, ]$location) + indexTo
      if(is.na(indexTo)) {
        return(NULL)
      }
    }
    numOnMap <- numOnMap + 1
    return(list(timeStayed = timeStayed, orig = macsLocs, macsTime = macsTime, num = numOnMap))
  }
  # toLoc is empty string
  if(toLoc == "") {
    if(!fromLoc %in% macsLocs$location) {
      return(NULL)
    }
    indexFrom <- match(fromLoc, macsTime$location)
    indexTo <- length(macsTime$location)
    if(indexFrom == indexTo) {
      return(NULL)
    }
    numOnMap <- numOnMap + 1
    return(list(timeStayed = timeStayed, orig = macsLocs, macsTime = macsTime, num = numOnMap))
  }
  
  # filter out if macaddr does not stay at the locations correctly
  if(!fromLoc %in% macsLocs$location | !toLoc %in% macsLocs$location) {
    return(NULL)
  }
  # finding when device visited locations
  indexFrom <- match(fromLoc, macsLocs$location)
  indexTo <- match(toLoc, macsLocs$location)
  
  while(!is.na(indexTo) & !is.na(indexFrom)) {
    if(indexTo > indexFrom) {
      # to minimize the distance between the two locations -- that is, for ex,
      # to filter out if a device starts at perkins, chills in their room
      # for two hours, goes on East, goes back to west, goes to WU.
      if(indexTo - indexFrom > distInte) {
        indexFrom <- match(fromLoc, macsLocs[indexFrom+1:length(macsLocs), ]$location) + indexFrom
        next
      }
      break
    }
    indexTo <- match(toLoc, macsLocs[indexTo+1:length(macsLocs), ]) 
  }
  # macaddr does not visit locations correctly
  if(is.na(indexTo) | is.na(indexFrom)) {
    return(NULL)
  }
  
  # by this point, the device has visited the appropriate locations
  # printing this stuff later/now just to see how many macaddrs were caught by the script for intuition/debugging purposes
  #cat(mac, "from:to", indexFrom, indexTo, "\n")
  
  numOnMap <- numOnMap + 1
  
  # using the time visited, grab the subset of visited locations to viz path from one loc to another
  indexFrom2 <- match(macsLocs$startTimeReal[indexFrom], macsTime$realTime)
  indexTo2 <- match(macsLocs$startTimeReal[indexTo], macsTime$realTime)
  macsTime <- macsTime[indexFrom2:indexTo2, ]
  return(list(timeStayed = timeStayed, orig = macsLocs, macsTime = macsTime, num = numOnMap, indexFrom = indexFrom, indexTo = indexTo))
}

# Returns the macaddrs that do move within the dataset (can later be changed to time interval)
doesMove <- function(df) {
  temp <- df %>% # counts how many times a mac visited a place
    group_by(macaddr,location) %>% 
    summarise(num = n())
  temp <- temp %>% # counts how many different places a mac visited
    group_by(macaddr) %>% 
    summarise(num = n())
  temp <- temp[which(temp$num != 1), ]
  return(temp$macaddr)
}