# Mainly to make fun plots.

library(readr)
library(dplyr)
library(ggplot2)
library(data.table)

# source("bytesExtractor.R")
# source("functions.R")

# fname <- "./data-plus-2018_wireless-ap-logs_20180419.csv"
# df0419 <- cleanSplunk(fname)
# bytesData <- read_csv("./bytesData0419.csv")
# mergeDF <- mergeBytesSplunk(bytesData, df0419)

dfssid <- df0419 %>% # seeing if ssid events change by campus
  group_by(ssid, campus) %>% 
  summarise(n = n())
dfVO <- df0419 %>% # seeing if ssid events change by locations off campus
  filter(campus == "Off") %>% 
  group_by(ssid, location.y) %>%
  summarise(n = n())
dfssidsum <- df0419 %>% # seeing what percentage of ssids are na
  group_by(ssid) %>% 
  summarise(n = n())

mainSSID <- c("Dukeblue", "DukeVisitor", "DukeOpen", "eduroam")

ggplot(data = dfssid %>% 
         filter(ssid %in% mainSSID), aes(x=ssid,y=n)) + 
  geom_col(aes(fill = campus)) +
  labs(title = "0419 ssid(truncat) usage by campus") + 
  facet_grid(.~campus)+
  theme_bw()
ggplot(data = dfssid, aes(x=ssid,y=n)) + 
  geom_col(aes(fill = campus)) +
  labs(title = "0419 ssid usage by campus") + 
  facet_grid(.~campus)+
  theme_bw()
ggplot(data = dfVO %>% 
         filter(ssid %in% mainSSID), aes(x=ssid, y= n)) + 
  geom_col(aes(fill = location.y)) +
  facet_grid(.~location.y) + 
  labs(title = "0419 ssid usage by offcampus locations")+
  theme_bw()
ggplot(data = dfssidsum[order(dfssidsum$n, decreasing = TRUE), ][1:5,], aes(x="", y= n, fill = ssid)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start= 0) + 
  labs(title = "0419 percentage of trun ssid events") +
  theme_bw()

dfOrgs <- df0419 %>% 
  group_by(macaddr, org) %>% 
  summatise(n=n())
dfOrgs <- dfOrgs %>%
  group_by(org) %>% 
  summarise(n = n())
dfOrgs <- dfOrgs[order(df0419Orgs$n, decreasing = TRUE), ] # Most common manufacturers.
dforgslot <- df0419 %>% # which manufacturers can only view which slots ?
  group_by(org, slotnum) %>% 
  summarise(n = n()) 
dfapslots <- df0419 %>% # are there aps that are lacking in certain slots?
  group_by(ap, slotnum, campus, location.y) %>% 
  summarise(n = n()) 
dfslots <- df0419 %>% # most common slotnums
  group_by(slotnum) %>% 
  summarise(n = n())

dfapslotsno2 <- dfapslots[dfapslots$slotnum != 2, ] # filtering out slotnum 2 because it's much rarer
dfapslotsno2 <- merge.data.frame(dfapslotsno2[dfapslotsno2$slotnum == 0, ], 
                                 dfapslotsno2[dfapslotsno2$slotnum == 1, ], 
                                 by = "ap")
dfapslotsno2 <- dfapslotsno2 %>% # view the ratio of slot 0 / slot 1 events
  mutate(ratio = n.x / n.y)
mean(dfapslotsno2$ratio) # mean = 1.531007
ggplot(data = dfapslotsno2, aes(x=n.x, y= n.y)) + 
  geom_point(aes(color = campus.x)) + 
  labs(x="num of slot 0 events", y= "num of slot 1 events") + 
  coord_fixed()

dfnaap <- df0419 %>% # want to see if certain aps generate nas more often than others
  group_by(ssid, ap) %>% 
  summarise(n = n())

dfap <- df0419 %>% # seeing top events by location by ap
  group_by(ap, location.y, campus) %>%
  summarise(n = n())
dfaptrun <- dfap %>%
  group_by(location.y) %>%
  top_n(n=5, wt = n)
locs <- dfaptrun %>% 
  group_by(location.y) %>% 
  summarise(n = n())
locs <- locs[locs$n == 5, ]
dfaptrun <- dfaptrun[dfaptrun$location.y %in% locs$location.y, ] # getting rid of locations that don't have at least 5 aps
dfaptrun <- dfaptrun[order(dfaptrun$n,dfaptrun$location.y),]
dfaptrun$ap <- factor(dfaptrun$ap, levels = dfaptrun$ap[order(dfaptrun$n)]) # reordering factors for nicer looking graph
ggplot(dfaptrun %>% filter(campus == "West"), aes(x=ap,y=n)) + 
  geom_col(aes(fill=location.y)) + 
  labs(title = "top events by location by ap") +
  theme_bw() + 
  coord_flip() + facet_wrap(.~location.y,scales = "free")


laptopsOrgs <- c("Microsoft Corporation", "ASUSTek COMPUTER INC.", "Dell Inc.", "Liteon Technology Corporation")
# phonesOrgs <- c("HTC Corporation", "zte corporation")

dflaptops <- df0419 %>% # examining laptops closely by calculating time diff between aps
  filter(org %in% laptopsOrgs)
dflaptops <- dflaptops[order(dflaptops$`_time`), ]
dflaptops <- dflaptops[order(dflaptops$macaddr), ]

macs <- unique(dflaptops$macaddr)
timesBtwnAPs <- NULL
noMoveMacs <- NULL

dflaptops$endTime <- lead(dflaptops$`_time`, 1)
timeInte <- difftime(dflaptops$endTime, dflaptops$`_time`, units = "secs")
dflaptops$timeInte <- timeInte
dflaptops$id <- rleid(dflaptops$ap)


# now going to see when a macaddr stays on the same ap and has more than just an assoc and auth
# # this particular bit is useful for comparing within an ap
# switches <- split.data.frame(dflaptops, dflaptops$id)
# num <- lapply(switches, function(x) {length(x$prefix)})
# num <- which(num > 2)
# switches <- switches[num]
# sink("NUL")
# lapply(switches, function(x) {
#   temp <- x
#   temp$timeInte[length(temp$timeInte)] <- NA
#   timesBtwnAPs <<- rbind(timesBtwnAPs, temp)
# })
# sink()
# timesBtwnAPs$timeInte[timesBtwnAPs$timeInte < 0] <- NA
# ggplot(timesBtwnAPs %>% # seeing the pattern of laptops and their timeInte between ap switches
#          filter(macaddr %in% unique(timesBtwnAPs$macaddr)[1:40]), 
#        aes(x = `_time`, y= as.numeric(timeInte), group = macaddr)) + 
#   geom_line() + 
#   facet_grid(.~macaddr) 

# # this particular bit is useful for comparing across aps within a location
# timeStart <- dflaptops[!rev(duplicated(rev(dflaptops$id))), ]
# timeStart$timeInte[timeStart$timeInte < 0] <- NA
# timeStart <- timeStart[order(timeStart$`_time`), ]
# timeStart <- timeStart[order(timeStart$macaddr), ]
# 
# timeStart$id <- rleid(timeStart$location.y)
# 
# timeStart$idBelow <- lead(timeStart$id, 1)
# temp <- sapply(1:length(timeStart$prefix), function(x) {
#   if(is.na(timeStart$idBelow[x])){
#     return(NA)
#   }
#   if(timeStart$id[x] != timeStart$idBelow[x]) {
#     return(NA)
#   }
#   return(timeStart$timeInte[x])
# })
# timeStart$timeInte <- temp
# 
# avgTimeBeforeSwitch <- timeStart %>% 
#   group_by(location.y, campus) %>% # grouping by location so that large timeInte as a result of changing locations isn't factored in
#   summarise(m = mean(timeInte, na.rm = TRUE), # calculating mean staying time -- if the time is small it implies there's a lot of switching within a location (hopefully)
#             n = n())
# avgTimeBeforeSwitch <- avgTimeBeforeSwitch[which(avgTimeBeforeSwitch$n > 1), ] # getting rid of aps with only one event
# 
# avgTimeBeforeSwitch$location.y <- factor(avgTimeBeforeSwitch$location.y, levels = avgTimeBeforeSwitch$location.y[order(avgTimeBeforeSwitch$m)]) # reordering factors for nicer looking graph
# ggplot(avgTimeBeforeSwitch, aes(x=location.y, y= m)) + 
#   geom_col(aes(fill = campus)) + 
#   theme_bw() + 
#   labs(title = "avg time stayed at each location", x = "location", y = "avg") +
#   facet_grid(.~campus, scale = "free_x")
# 
# avgTimeBeforeSwitch <- avgTimeBeforeSwitch[order(avgTimeBeforeSwitch$m), ]



# Testing for frequent switching of APs with laptops
####################
mergedDF <- rbind(df0419, df0418, df0420)

dfLit <- mergedDF %>% 
  filter(org %in% laptopsOrgs) # filtering for laptops
macs <- (dfLit %>% 
           group_by(macaddr, location.y) %>% 
           summarise(n = n())) %>% 
  group_by(macaddr) %>% 
  summarise(n = n())
# commented below line out to see how accurate this is when macaddrs do change locations
# macs <- macs[macs$n < 2, ] # getting all the macaddrs that do not change location. most likely these are the ones that are actually staying in one place
# macs <- intersect(unique((dfLit %>% filter(location.y %in% c("Fuqua", "Perkins", "Wannamaker")))$macaddr),macs$macaddr) # getting all the macaddrs that were only present in fuqua for less confounding variables, or something

macs <- macs$macaddr

dfLit <- dfLit %>% 
  filter(macaddr %in% macs) %>%
  group_by(macaddr) %>% 
  arrange(`_time`)

timesBtwnAPs <- lapply(unique(macs), function(mac) {
  temp <- dfLit %>% 
    filter(macaddr == mac)
  temp$endTime <- lead(temp$`_time`, 1)
  temp <- temp %>% 
    mutate(timeInte = difftime(endTime, `_time`, units = "secs"))
  return(temp)
})

lengthOfDF <- sapply(timesBtwnAPs, function(x) {return(length(x$prefix))})
# getting outliers -- these outliers represent macaddrs that have more events than the mean
# if they have more events, then that might mean that the mac is assoc and auth etc too much
eligibleDF <- NULL
switchTime <- 60
sink("NUL")
lapply(timesBtwnAPs, function(x) { # Note to self: doesn't work well with macs that change locations, physically, not variable wise
  if(length(x$prefix) > quantile(lengthOfDF)[[2]] + 1.5 * IQR(lengthOfDF) ) { # only doing outliers above since that means there's too many abnormal switches
    x$nextTimeInte <- lead(as.numeric(x$timeInte), 1)
    x <- x %>%
      mutate(lessThanMin = (nextTimeInte - timeInte < switchTime & nextTimeInte + (switchTime/2) >= timeInte)) # returns TRUE if there are too many switches between aps in a time frame
    
    # returns list of row numbers that repeat more than twice in a sequence
    # in this case, returns a list of row numbers that have TRUE occurring more than twice. 
    # twice is okay because typically there is an assoc and then an auth with a small time interval, which triggers
    # the above mutate to assign a TRUE to both of the rows. more than twice implies abnormal switching
    rows <- sapply(1:(length(x$lessThanMin)-1), function(i) {
      if(is.na(x$lessThanMin[[i]]) | is.na(x$lessThanMin[[i+1]])) {
        return(NULL)
      }
      if(x$lessThanMin[[i]] == x$lessThanMin[[i+1]] & x$lessThanMin[[i]]) {
        return(i)
      } else {
        return(NULL)
      }
    })
    rows <- unlist(rows)
    notInSeq <- diff(rows)
    if(length(notInSeq) == 0) {
      return()
    }
    sink("NUL")
    sapply(1:length(notInSeq), function(i) { # the last row of TRUE gets cut off. this is adding it back in
      if(notInSeq[i] != 1) {
        rows <<- c(rows, rows[i] + 1) 
      }
    })
    sink()
    rows <- sort(rows)
    x <- x[rows, ] # reduce df down to problematic rows
    x$id <- rows
    
    eligibleDF <<- rbind(eligibleDF, x)
  }
})
sink()

# want to take out sequences that are less than 2 in length
# since 2 in length is normal (assoc then auth)
thresh <- 2
sequ <- diff(eligibleDF$id) 
sink("NUL")
prev <- sequ[[1]]
count <- 1
seqs <- sapply(2:length(sequ), function(i) {
  ret <- NULL
  if(1 == prev) {
    count <<- count + 1
  }
  else {
    if(count > thresh) {
      ret <- i
    } else {
      ret <- -1
    }
    count <<- 1
  }
  prev <<- sequ[[i]]
  return(ret)
})
sink()

# to get the relevant indices of sequences that are > 2
inds <- NULL
iStart <- 1
for(i in 2:length(seqs)) {
  if(!is.null(seqs[[i]])) {
    if(seqs[[i]] > 0) {
      inds <<- c(inds, iStart:i)
    }
    iStart <<- i + 1
  }
}
eligibleDFforAPs <- eligibleDF[inds, ]

countAPs <- (eligibleDFforAPs %>%
                   group_by(macaddr, ap) %>% 
                   summarise(n = n()) %>% 
                   arrange(desc(n))) %>%
  group_by(ap) %>% 
  summarise(numMacs = n())
erroneousAPs <- eligibleDFforAPs %>% 
  group_by(ap) %>% 
  summarise(numEvents = n())
erroneousAPs <- merge(countAPs, erroneousAPs) %>% 
  mutate(ratio = numMacs / numEvents) %>%
  arrange(desc(numMacs))
head(erroneousAPs)

ggplot(eligibleDFforAPs, aes(x = `_time`, y= ap)) + # maybe a helpful visualization ? like if there was a time where everyone re-connected?
  geom_point(aes(color = asa_code)) +
  labs(subtitle="Each dot represents a registered event for aps identified as erroneous")

####################




