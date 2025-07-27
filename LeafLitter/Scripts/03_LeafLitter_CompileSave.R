# Exploring the leaf litter data to identify peak times of deposition

library(ggplot2)
library(tidyverse)


# Set up file paths etc. --> this should also indicate where you can find these files!
path.google <- "~/Google Drive/My Drive"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.figs <- file.path(path.litter, "figures") # where we shoudl save some figures
path.save <- file.path(path.litter, "LeafLitterData_Clean_forArchiving") # Where we should save the data

# Setting a consistent color scheme across all graphs
plotOrder <- c("B-127", "U-134", "N-115", "HH-115")
ewPlotColors <- c("#1B9E77","#D95F02", "#7570B3", "#E7298A")
names(ewPlotColors) = plotOrder
ewPlotColors


dir(path.save)
#combine into one dataset
all_cleaned_files <- list.files(path.save, pattern = "\\.csv$", full.names = TRUE)
if (length(all_cleaned_files) == 0) { stop(...) }
datLitter <- map_df(all_cleaned_files, read_csv)
datLitter$plot <- factor(datLitter$plot, levels = plotOrder)
summary(datLitter)

datLitter$sci_name <- paste(datLitter$genus, datLitter$species)

datLitter$year <- lubridate::year(datLitter$date_collection)
datLitter$yday <- lubridate::yday(datLitter$date_collection)
datLitter$week <- lubridate::week(datLitter$date_collection)
summary(datLitter)

datLitter <- datLitter[datLitter$year<2024,]
summary(datLitter)


# There is no HH-115 NE, NW, SE, SW --> it has a weird layout
datLitter[datLitter$plot=="HH-115" & datLitter$trap_ID %in% c("NE", "NW", "SE", "SW"), "trap_ID"] <- NA

datLitter <- datLitter[!is.na(datLitter$trap_ID),]

# Finding bags that that are empty or missing
unique(datLitter$tissue)

datMissing <- datLitter[datLitter$tissue == "MISSING DATA",]
dim(datMissing)
summary(datMissing)


datNone <- datLitter[datLitter$tissue == "EMPTY BAG",]
dim(datNone)
summary(datNone)


# Figuring out how to get species-stuff (finally; I've been putting this off for years) ----
# What we need to do for each collection (trap per date) is:
# 1. if a trap is MISSING, put NA for all the speices
# 2. if a bag is empty, put 0 for all the species
# 3. if a species wasn't found in a bag, put 0 for it
# Note: we'll leave the people & weighed columns blank for this
datLeaf <- datLitter[datLitter$tissue %in% c("leaf", "MISSING DATA", "EMPTY BAG"),!grepl("fruit", names(datLitter))]
sppUniqueLeaf <- aggregate(mass_g ~ sci_name, data=datLeaf, FUN=sum, na.rm=T)$sci_name
dim(datLeaf)

datAdd <- data.frame()
for(PLT in unique(datLeaf$plot)){
  datPLT <- datLeaf[datLeaf$plot==PLT,]
  print(PLT)
  for(TRP in unique(datPLT$trap_ID)){
    datTRP <- datPLT[datPLT$trap_ID==TRP,]
    
    print(paste("---", TRP))
    
    for(CDATE in unique(datTRP$date_collection)){
      datNOW <- datTRP[datTRP$date_collection==CDATE,]
      # data.frame(datNOW)
      CDATE <- as.Date(CDATE)
      
      sppMissing <- sppUniqueLeaf[!sppUniqueLeaf %in% unique(datNOW$sci_name)]
      sppArray <- data.frame(matrix(unlist(str_split(sppMissing, " ")), ncol=2, byrow = T))
      names(sppArray) <- c("genus", "species")
      sppArray$sci_name <- sppMissing
      
      missingNOW <- data.frame(sorter=NA, weigher=NA, date_weighed=NA, data_entry=NA, notes=NA,
                               date_collection=CDATE, year=lubridate::year(CDATE), yday=lubridate::yday(CDATE), week=lubridate::week(CDATE),
                               plot=PLT, trap_ID=TRP, 
                               genus=sppArray$genus, species=sppArray$species, sci_name=sppArray$sci_name,
                               tissue="leaf"
      )
      
      # if the bag is missing, make rows for all species with NA
      if(nrow(datNOW)==1 & datNOW$tissue[1]=="MISSING DATA"){
        missingNOW$mass_g <- NA
      } else {
        missingNOW$mass_g <- 0
      }
      missingNow <- missingNOW[,names(datLeaf)] # Make sure everything is in the right order
      
      datAdd <- rbind(datAdd, missingNOW) # This will be cumbersome, but *should* work
    } # End Colleciton Date
  } # end trap
} # end plot
summary(datAdd)
datLeaf <- rbind(datLeaf[datLeaf$tissue=="leaf",], datAdd)


# Summing to the trap level
aggLeafTrap <- aggregate(mass_g ~ year + week + yday + date_collection + plot + trap_ID, data=datLeaf, FUN=sum, na.rm=T, drop=T)
# summary(aggLeafTrap)

# Make sure to add missing back in; doign drop=F above results in too many combos
aggLeafTrap <- rbind(aggLeafTrap, datMissing[,c("year", "week", "yday", "date_collection", "plot", "trap_ID", "mass_g")])
summary(aggLeafTrap)

# Now going through each plot & trap get the *rate* of deposition
allCollect = NULL
# plotsCore <- c("N", "S", "E", "W")
# plotExtra <- c("NW", "SW", "NE", "SE")
for(PLT in unique(aggLeafTrap$plot)){
  datPLT <- aggLeafTrap[aggLeafTrap$plot==PLT,]
  datPLT <- droplevels(datPLT)
  
  datePLT <- unique(datPLT$date_collection)
  datePLT <- sort(datePLT)
  daysPLT <- diff(datePLT)
  # Making a data frame with all collections dates and traps for the plot
  # That way we can add in plots as missing if they're not there
  dfCollection <- data.frame(plot=PLT, trap_ID = rep(unique(datPLT$trap_ID), each=length(datePLT)),
                             date_collection=datePLT, days_collection=c(NA, daysPLT))
  
  
  if(is.null(allCollect)){
    allCollect <- dfCollection
  } else {
    allCollect <- rbind(allCollect, dfCollection)
  }
}
summary(allCollect)

# Merging the number of days in each collection period with the actual data
summary(allCollect)
hist(allCollect$days_collection)


# aggLeafTrap[aggLeafTrap$year==2022 & aggLeafTrap$date_collection<as.Date("2022-08-15")  & aggLeafTrap$plot=="B-127",]
dim(aggLeafTrap)
aggLeafTrap <- merge(aggLeafTrap, allCollect, all=T)
# aggLeafTrap$year <- lubridate::year(aggLeafTrap$date_collection)
# aggLeafTrap$week <- lubridate::week(aggLeafTrap$date_collection)
summary(aggLeafTrap)

# This added in a lot of NAs because we added half our traps a couple years in
for(PLT in unique(aggLeafTrap$plot)){
  for(TRP in unique(aggLeafTrap$trap_ID[aggLeafTrap$plot==PLT])){
    minTrap <- min(aggLeafTrap$date_collection[aggLeafTrap$plot==PLT & aggLeafTrap$trap_ID==TRP & !is.na(aggLeafTrap$mass_g)] ,na.rm=T)
    
    # test <- aggLeafTrap[aggLeafTrap$plot==PLT & aggLeafTrap==TRP & aggLeafTrap$date_collection>minTrap,]
    
    aggLeafTrap <- aggLeafTrap[aggLeafTrap$plot!=PLT | (aggLeafTrap$plot==PLT & aggLeafTrap$trap_ID!=TRP) | 
                                 (aggLeafTrap$plot==PLT & aggLeafTrap$trap_ID==TRP & aggLeafTrap$date_collection>minTrap),]
  }
}


dim(aggLeafTrap)
summary(aggLeafTrap)

aggLeafTrap$mass_g_day <- aggLeafTrap$mass_g/aggLeafTrap$days_collection
summary(aggLeafTrap)

# Lets calculate the proportion of leaf fall at any given point in time
leafTrapTotal <- aggregate(mass_g ~ year + plot + trap_ID, data=aggLeafTrap, FUN=sum)
names(leafTrapTotal)[names(leafTrapTotal)=="mass_g"] <- "totalMass_year"
summary(leafTrapTotal)


# Merging our totals into the weekly sums so we can get proportion
aggLeafTrap <- merge(aggLeafTrap, leafTrapTotal, all=T)
aggLeafTrap <- aggLeafTrap[!is.na(aggLeafTrap$trap_ID),]
aggLeafTrap$mass_prop <- aggLeafTrap$mass_g/aggLeafTrap$totalMass_year
summary(aggLeafTrap)
aggLeafTrap[is.na(aggLeafTrap$totalMass_year),]

write.csv(aggLeafTrap, file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter ", "LeafLitter_byTrap_latest.csv"), row.names=F)

# Getting some plot-level summary stats
aggLeafPlot <- aggregate(cbind(mass_g, mass_g_day, mass_prop, totalMass_year)~ year + plot + week + date_collection, data=aggLeafTrap, FUN=mean)
summary(aggLeafPlot)

weekPeak <- data.frame(year=rep(unique(aggLeafTrap$year), each=length(unique(aggLeafPlot$plot))),
                       plot=rep(unique(aggLeafPlot$plot)),
                       week=NA,
                       date=NA,
                       propPeak = NA,
                       prop9Wk = NA,
                       prop5Wk = NA,
                       prop3Wk = NA)
# weekPeak <- weekPeak[weekPeak$year<max(weekPeak$year),]

for(YR in unique(weekPeak$year)){
  datYr <- aggLeafPlot[aggLeafPlot$year==YR,]
  for(PLT in unique(datYr$plot)){
    indNow <- which(weekPeak$year==YR & weekPeak$plot==PLT)
    datPlot <- datYr[datYr$plot==PLT,]
    
    indPeak <- which(datPlot$mass_g_day==max(datPlot$mass_g_day))
    wkPeak <- datPlot$week[indPeak]
    
    # Recalculating proportion/weidghts based on rates; it's slightly different, but not dramatically
    datPlot$mass_g_day_Prop <- datPlot$mass_g_day/sum(datPlot$mass_g_day, na.rm=T)
    wkPeakWeight <- sum(datPlot$week*datPlot$mass_g_day_Prop)
    # wkPeakWeight2 <- sum(datPlot$week*datPlot$mass_prop)
    
    weekPeak$week[indNow] <- wkPeak
    weekPeak$weekPeakWt[indNow] <- wkPeakWeight
    weekPeak$date[indNow] <- as.character(datPlot$date_collection[indPeak])
    weekPeak$propPeak[indNow] <- datPlot$mass_prop[indPeak]
    weekPeak$prop9Wk[indNow] <- sum(datPlot$mass_prop[datPlot$week %in% (wkPeak-4):(wkPeak + 4)])
    weekPeak$prop5Wk[indNow] <- sum(datPlot$mass_prop[datPlot$week %in% (wkPeak-2):(wkPeak + 2)])
    weekPeak$prop3Wk[indNow] <- sum(datPlot$mass_prop[datPlot$week %in% (wkPeak-1):(wkPeak + 1)])
  }
}
weekPeak$date <- as.Date(weekPeak$date)
summary(weekPeak)
weekPeak[weekPeak$prop5Wk<0.5,]


write.csv(weekPeak, file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter ", "PeakLeafDates_byPlot.csv"), row.names=F)


summary(leafTrapTotal)
for(YR in unique(aggLeafTrap$year)){
  print(YR)
  datYr <- aggLeafTrap[aggLeafTrap$year==YR,]
  for(PLT in unique(datYr$plot)){
    print(PLT)
    datPlot <- datYr[datYr$plot==PLT,]
    for(TRP in unique(datPlot$trap_ID)){
      indNow <- which(leafTrapTotal$year==YR & leafTrapTotal$plot==PLT & leafTrapTotal$trap_ID==TRP)
      datTrp <- datPlot[datPlot$trap_ID==TRP,]
      
      # print(paste(TRP, "-", indNow))
      indPeak <- which(datTrp$mass_g_day==max(datTrp$mass_g_day, na.rm=T))
      wkPeak <- datTrp$week[indPeak]
      
      # Recalculating proportion/weidghts based on rates; it's slightly different, but not dramatically
      datTrp$mass_g_day_Prop <- datTrp$mass_g_day/sum(datTrp$mass_g_day, na.rm=T)
      wkPeakWeight <- sum(datTrp$week*datTrp$mass_g_day_Prop, na.rm=T)
      
      
      # tmpPleafTrapTotaleak <- data.frame(year = YR, plot=PLT, trap_ID=TRP, week=NA, date=NA, propPeak=NA)
      leafTrapTotal[indNow,"weekPeak"] <- wkPeak
      leafTrapTotal[indNow, "weekPeakWt"] <- wkPeakWeight
      leafTrapTotal[indNow,"datePeak"] <- as.character(datTrp$date_collection[indPeak])
      leafTrapTotal[indNow,"propPeak"] <- datTrp$mass_prop[indPeak]
      # tmpPeak[1,"prop9Wk"] <- sum(datTrp$mass_prop[datTrp$week %in% (wkPeak-4):(wkPeak + 4)])
      # tmpPeak[1,"prop5Wk"] <- sum(datTrp$mass_prop[datTrp$week %in% (wkPeak-2):(wkPeak + 2)])
      # tmpPeak[1,"prop3Wk"] <- sum(datTrp$mass_prop[datTrp$week %in% (wkPeak-1):(wkPeak + 1)])
    }
  }
}
# weekPeakTrap$date <- as.Date(weekPeakTrap$date)
summary(leafTrapTotal)
# summary(leafTrapTotal[is.na(leafTrapTotal$weekPeak),])
# weekPeak[weekPeak$prop5Wk<0.5,]
head(leafTrapTotal)

write.csv(leafTrapTotal, file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter ", "LeafLitter_Peak_byTrap.csv"), row.names=F)




