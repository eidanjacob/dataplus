# speedLimit.R
# Reports any MAC addresses moving suspiciously fast
# Requires coordinates on event data

report <- function(index, mac, speeds){
  string <- paste("MAC", mac, "at", speeds[[2]]$location[index], speeds[[2]]$time[index], 
                  "then at", speeds[[2]]$location[index + 1], speeds[[2]]$time[index + 1],
                  "speed:", speeds[[1]][index], "m/s.")
  print(string)
}

speedLimit <- function(eventData, limit){
  source("./speedR.R")
  MACS <- unique(eventData$macaddr)
  sapply(MACS, function(mac){
    speeds <- speedR(mac, eventData)
    if(is.null(speeds)){
      next
    }
    indices <- which(speeds[[1]] == Inf | speeds[[1]] > limit)
    sapply(indices, "report", mac, speeds)
  })
  print("Done.")
}