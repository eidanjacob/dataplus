# matchOUI.R
matchOUI <- function(data, lookup){
  sapply(data$macaddr, function(mac){
    MAC <- substr(gsub(":", "-", toupper(mac)), 1, 8)
    return(lookup$org[lookup$prefix == MAC])
  })
}
