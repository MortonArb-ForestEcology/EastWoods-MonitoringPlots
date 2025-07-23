# Extract Daymet for the Arb;
library(daymetr)

path.google <- "~/Google Drive/My Drive/URF REU 2025 - Lizer - Leaf Litter /"
pathOut <- file.path(path.google, "data/daymet")
if(!dir.exists(pathOut)) dir.create(pathOut, recursive=T)

site.id <- "MortonArb-VisitorCenter"
site.lat <- 41.813613
site.lon <- -88.071797
start.date <- "2017"
end.date <- lubridate::year(Sys.Date())-1


metAll <- download_daymet(site=site.id, lat=site.lat, lon=site.lon, start=start.date, end=end.date)
metAll <- metAll$data
metAll$date <- as.Date(paste(metAll$year, metAll$yday, sep="-"), format="%Y-%j")
metAll$month <- lubridate::month(metAll$date)
metAll$week <- lubridate::week(metAll$date)

# Using Teton's equation to calculate saturated VP so we can get VPD; note using 610.78 bc we have VP in PA; not kPA
metAll$vp.sat.max <- 610.78*exp(17.27*metAll$tmax..deg.c./(metAll$tmax..deg.c.+237.73))
metAll$vp.sat.min <- 610.78*exp(17.27*metAll$tmin..deg.c./(metAll$tmin..deg.c.+237.73))
metAll$VPD.tmax <- metAll$vp.sat.max - metAll$vp..Pa.
metAll$VPD.tmin <- metAll$vp.sat.min - metAll$vp..Pa.
metAll$VPD.tmin[metAll$VPD.tmin<0] <- 0 # VPD can't be negative in reality, so make 0
metAll$VPD.avg <- apply(metAll[,c("VPD.tmax", "VPD.tmin")], 1, mean)
summary(metAll)

write.csv(metAll, file.path(pathOut, "daymet_raw_2017-2023.csv"), row.names=F)

# Do some growing season summaries
# Subsetting to June-July-August, which is the typical summer window
metJJA <- metAll[metAll$month %in% c(6:8),]
summary(metJJA)
# Note: SWE = snow-water equivalent; so it *should* be 0 for summer

metJJAyr <- aggregate(cbind(prcp..mm.day., srad..W.m.2., swe..kg.m.2., tmax..deg.c., tmin..deg.c., vp..Pa., VPD.tmax, VPD.tmin, VPD.avg) ~ year, data=metJJA, FUN=mean)
summary(metJJAyr)

for(i in 1:nrow(metJJAyr)){
  YR = metJJAyr$year[i]
  metYR <- metJJA[metJJA$year==YR,]
  metJJAyr[i,"n.Rainless"] <- length(which(metYR$prcp..mm.day.==0))
  metJJAyr[i,"n.Tmax32"] <- length(which(metYR$tmax..deg.c.>32)) # days with high >90 F
  metJJAyr[i,"n.Tmax35"] <- length(which(metYR$tmax..deg.c.>35)) # days with high >95 F
  
  # Finding consecutive rainless days & temp during that period
  metYR$consecRainless <- NA 
  metYR$consecRainless[1] <- ifelse(metYR$prcp..mm.day.[1]==0, 1, 0)
  for(j in 2:nrow(metYR)){
    if(metYR$prcp..mm.day.[j]==0){
      metYR$consecRainless[j] <- metYR$consecRainless[j-1] + 1
    } else {
      metYR$consecRainless[j] <- 0
    }
  }
  indMax <- which(metYR$consecRainless==max(metYR$consecRainless))
  maxRainless <- metYR$consecRainless[indMax]
  
  metJJAyr[i,"RainlessConsec.max"] <- maxRainless
  metJJAyr[i,"RainlessConsec.Tmax.mean"] <- mean(metYR$tmax..deg.c.[(indMax - maxRainless+1):indMax])
}

metJJAyr
write.csv(metJJAyr, file.path(pathOut, "daymet_June-July-August_summaries_2017-2023.csv"), row.names=F)
