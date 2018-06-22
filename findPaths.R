# Makes a dataframe of all the locations that a macaddr goes from and to
# This can be used to 
# 1. Get a feel of paths
# 2. Delete paths from East to West to declutter map
# 3. Calculate probabilities
# 
df <- read_csv("dataplus-master/mergedData.csv")
df$`_time` <- force_tz(ymd_hms(df$`_time`), "EST")

# Can later filter df by time to observe how paths change as time changes

uniqMacs <- unique(df$macaddr)
uniqMacs <- as.character(uniqMacs)

paths <- NULL
end <- 250 # small number for faster runtimes

for(i in 1:end) { 

  # select mac to see its from and to locations
  randomMac <- uniqMacs[[i]] 
  newdf <- df %>% 
    filter(macaddr == randomMac)
  
  # collect from, to locations
  from <- newdf[ c(TRUE,FALSE), ]
  to <- newdf[!c(TRUE, FALSE), ]
  
  # for debugging/i can't remember why this is here
  if(length(from$location.y) != length(to$location.y)) {
    print(from)
    next
  }
  
  # filtering out locations that don't change -- eg Perkins to Perkins
  fromL <- from$location.y[which(from$location.y != to$location.y)]
  toL <- to$location.y[which(from$location.y != to$location.y)]
  
  locs <- data.frame(from = fromL, to = toL)
  paths <- rbind(paths, locs)

}

# removing the duplicate rows and adding a column that counts the number of duplicates
numDups <- paths %>%
  group_by(from, to) %>%
  summarise(num = n())

# would also like to combine things like Perkins to Bostock and Bostock to Perkins into one row

View(paths)
