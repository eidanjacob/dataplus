library(dplyr)
library(lubridate)
library(data.table) # for rleid

# Script that calculates how long a macaddr stayed in each place it visited
# mac is a macaddr (character), macdf is dataframe of events
howLong <- function(mac, macdf) {
  ############
  # Note that this seems to be a better way of calculating timeDiff than the 
  # stuff below does.
  # macdf <- macdf %>% # creating column referencing the next location
  #   group_by(macaddr) %>% 
  #   mutate(nextTime = lead(`_time`, order_by = macaddr), # quick note that this seems to be more efficient than using the howLong function in functions.R 
  #          timeDiff = as.numeric(difftime(nextTime, `_time`, units = "secs"))) %>% 
  #   rename(fromLoc = location.y)
  ############
  if(!"realTime" %in% names(macdf)) {
    macdf <- macdf %>% 
      rename(realTime = `_time`,
             location = location.y) 
  }
  if(!mac %in% unique(macdf$macaddr)) {
    return(NULL)
  }
  
  macLocs <- macdf %>%
    filter(macaddr == mac)
  macLocs <- macLocs[order(macLocs$realTime), ]
  macLocs$realTimeEnd <- lead(macLocs$realTime, 1)
  macLocs <- macLocs %>% 
    mutate(timeDiff = as.numeric(difftime(macLocs$realTimeEnd, macLocs$realTime, units = "secs"))) # timeDiff is in seconds

  # function that adds a column that has numbers that will increment when the location before it changes
  col <- rleid(macLocs$location)
  macLocs$id <- col
  
  # if a macaddr has only visited one/less than one location
  if(length(unique(macLocs$id)) <= 1) {
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
# 2. 1. after it has gone through the filtering
# 3. the shortened dataframe of visited locations used in drawing lines
# 4. the number of devices on the map
# It returns null when the macaddr did not visit the locations properly.
# fromLoc and toLoc are the names of locations.
# See readme for information on the inte variables.
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
  # if someone moves from A to B, and then A to B again in a later time, only the first path is visualized.
  # currently the only way to visualize both paths (not at once) is by changing the time inputs
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
    return(list(timeStayed = timeStayed, orig = macsLocs, macsTime = macsTime[indexFrom:indexTo, ], num = numOnMap))
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
    return(list(timeStayed = timeStayed, orig = macsLocs, macsTime = macsTime[indexFrom:indexTo, ], num = numOnMap))
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
    indexTo <- match(toLoc, macsLocs[indexTo+1:length(macsLocs), ]$location) + indexTo
  }
  # macaddr does not visit locations correctly
  if(is.na(indexTo) | is.na(indexFrom)) {
    return(NULL)
  }
  
  # by this point, the device has visited the appropriate locations
  numOnMap <- numOnMap + 1
  
  # using the time visited, grab the subset of visited locations to viz path from one loc to another
  indexFrom2 <- match(macsLocs$startTimeReal[indexFrom], macsTime$realTime)
  indexTo2 <- match(macsLocs$startTimeReal[indexTo], macsTime$realTime)
  macsTime <- macsTime[indexFrom2:indexTo2, ]
  return(list(timeStayed = timeStayed, orig = macsLocs, macsTime = macsTime, num = numOnMap, indexFrom = indexFrom, indexTo = indexTo))
}

# Returns the macaddrs that move within the dataset during a time interval
# df is newly cleaned splunk data
doesMove <- function(df, inte = interval(min(df$`_time`), max(df$`_time`))) {
  temp <- df %>% # counts how many times a mac visited a place
    filter(`_time` %within% inte) %>% 
    group_by(macaddr,location) %>% 
    summarise(num = n())
  temp <- temp %>% # counts how many different places a mac visited
    group_by(macaddr) %>% 
    summarise(num = n())
  temp <- temp[which(temp$num != 1), ]
  return(temp$macaddr)
}

# Takes filename corresponding to Splunk data and cleans it and links it appropriately. Will also write it to a file if the line is not commented out.
# validLocations is a dataframe that matches aps to locations
# A version of this exists in many of the apps, but is replicated here for future reference.
writeSplunk <- function(splunkFile, ouiFile, validLocations, coord) {
  df <- read_csv(splunkFile)
  
  # match aps to locations, merge for coordinates
  df <- df[!is.na(df$ap),] # remove observations with no ap
  
  # Some aps are in splunk data with name, some with number - code below matches location using whichever is available
  nameMatch = which(validLocations$APname %in% df$ap) # find which aps have their name in the data
  numMatch = which(validLocations$APnum %in% df$ap) # find which aps have their number in the data
  validLocations$ap = c(NA) # new "flexible" column to store either name or number
  validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
  validLocations$ap[numMatch] = validLocations$APnum[numMatch]
  
  validLocations <- merge(coord, validLocations) # link coordinates to locations
  # use the new "flexible" ap variable to merge coordinates onto df
  df <- merge(df, validLocations, by = "ap") # this is the slow step
  # merge with OUI table to identify manufacturers
  df$prefix <- sapply(df$macaddr, function(mac){
    str <- substr(mac, 1, 8)
    return(gsub(":", "-", toupper(str)))
  })
  oui <- read_csv(ouiFile)
  df <- merge(df, oui)
  #write.csv(df, "../mergedData.csv")
  
  return(df)
}