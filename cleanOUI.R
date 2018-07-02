oui <- readLines("./oui.txt") # read in file
oui <- oui[-(1:3)] # delete header
oui <- oui[which(nchar(oui) == 0) + 1] # keep only lines after blank lines
oui <- gsub("*\t", "", oui) # remove "\t"s
oui <- gsub("hex", "", oui) # remove "hex"
oui <- gsub("[()]", "", oui) # remove "()"
ouiDF <- data.frame(prefix = substr(oui, 1, 8), org = substr(oui, 12, nchar(oui)))
write.csv(ouiDF, "ouiDF.txt", row.names = FALSE)