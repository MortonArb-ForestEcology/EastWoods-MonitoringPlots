# Exploring the leaf litter data to identify peak times of deposition

library(ggplot2)
library(tidyverse)
library(nlme);  # Does the mixed effects model
library(emmeans) # will et us do a multi-comparisons test


# Set up file paths etc. --> this should also indicate where you can find these files!
path.google <- "~/Google Drive/My Drive"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.figs <- file.path(path.litter, "figures") # where we shoudl save some figures
path.save <- file.path(path.litter, "LeafLitterData_Clean_forArchiving") # Where we should save the data


# Using a formatting theme consistent with what Meghan has done
theme_base <-   theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_rect(fill="white", colour = "black", linewidth=0.7),
                      axis.title.x = element_text(margin = margin(t = 10, b=5), size=14),
                      axis.title.y = element_text(margin = margin(l = 5, r=5), size=14),
                      axis.text.x= element_text(margin = margin(t = 10), size=12),
                      axis.text.y=element_text(margin = margin(r = 10), size=12),
                      axis.ticks.length=unit(-0.3, "cm"),
                      # axis.ticks.margin=unit(0.5, "cm"),
                      axis.ticks = element_line(colour = "black", linewidth = 0.4))


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

datLitter$year <- lubridate::year(datLitter$date_collection)
datLitter$yday <- lubridate::yday(datLitter$date_collection)
datLitter$week <- lubridate::week(datLitter$date_collection)
summary(datLitter)

datLitter <- datLitter[datLitter$year<2024,]
summary(datLitter)

metSummer <- read.csv("~/Google Drive/My Drive/URF REU 2025 - Lizer - Leaf Litter /data/daymet/daymet_June-July-August_summaries_2017-2023.csv")
summary(metSummer)

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

# Summing to the trap level
aggLeafTrap <- aggregate(mass_g ~ year + week + date_collection + plot + trap_ID, data=datLitter[datLitter$tissue=="leaf",], FUN=sum)
summary(aggLeafTrap)

aggLeafTrap <- rbind(aggLeafTrap, datNone[,c("year", "week", "date_collection", "plot", "trap_ID", "mass_g")], datMissing[,c("year", "week", "date_collection", "plot", "trap_ID", "mass_g")])
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
aggLeafTrap$year <- lubridate::year(aggLeafTrap$date_collection)
aggLeafTrap$week <- lubridate::week(aggLeafTrap$date_collection)
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

ggplot(data=aggLeafTrap) +
  facet_grid(year~plot) +
  # facet_wrap(~tissue, scales="free_y") +
  # geom_boxplot(aes(x=as.factor(week), y=mass_g, color=plot)) +
  geom_point(aes(x=week, y=mass_g_day, color=plot)) +
  stat_summary(geom="line", aes(x=week, y=mass_g_day), fun="mean") +
  labs(x="week", y="mass (g)") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_bw()

# Lets calculate the proportion of leaf fall at any given point in time
leafTrapTotal <- aggregate(mass_g ~ year + plot + trap_ID, data=aggLeafTrap, FUN=sum)
names(leafTrapTotal)[names(leafTrapTotal)=="mass_g"] <- "totalMass_year"
summary(leafTrapTotal)
hist(leafTrapTotal$totalMass_year)
hist(leafTrapTotal$totalMass_year[leafTrapTotal$year<2024])
summary(leafTrapTotal[leafTrapTotal$year<2024,])

# Just doing a check for weirdo names again
leafTrapTotal2 <- aggregate(mass_g ~ plot + trap_ID, data=aggLeafTrap, FUN=sum)
summary(leafTrapTotal2)




# Merging our totals into the weekly sums so we can get proportion
aggLeafTrap <- merge(aggLeafTrap, leafTrapTotal, all=T)
aggLeafTrap <- aggLeafTrap[!is.na(aggLeafTrap$trap_ID),]
aggLeafTrap$mass_prop <- aggLeafTrap$mass_g/aggLeafTrap$totalMass_year
summary(aggLeafTrap)
aggLeafTrap[is.na(aggLeafTrap$totalMass_year),]


ggplot(data=aggLeafTrap) +
  facet_grid(year~plot) +
  # facet_wrap(~tissue, scales="free_y") +
  # geom_boxplot(aes(x=as.factor(week), y=mass_g, color=plot)) +
  geom_point(aes(x=week, y=mass_prop, color=plot)) +
  stat_summary(geom="line", aes(x=week, y=mass_prop), fun="mean") +
  labs(x="week", y="mass (g)") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_bw()

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
    weekPeak$week[indNow] <- wkPeak
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


# weekPeakTrap <- data.frame(year=rep(unique(aggLeafTrap$year), each=length(unique(aggLeafPlot$plot))*length(unique(aggLeaf))),
#                        plot=rep(unique(aggLeafPlot$plot)),
#                        week=NA,
#                        date=NA,
#                        propPeak = NA,
#                        prop9Wk = NA,
#                        prop5Wk = NA,
#                        prop3Wk = NA)
# weekPeak <- weekPeak[weekPeak$year<max(weekPeak$year),]
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
  
      print(paste(TRP, "-", indNow))
      indPeak <- which(datTrp$mass_g_day==max(datTrp$mass_g_day, na.rm=T))
      wkPeak <- datTrp$week[indPeak]
      # tmpPleafTrapTotaleak <- data.frame(year = YR, plot=PLT, trap_ID=TRP, week=NA, date=NA, propPeak=NA)
      leafTrapTotal[indNow,"weekPeak"] <- wkPeak
      leafTrapTotal[indNow,"date"] <- as.character(datTrp$date_collection[indPeak])
      leafTrapTotal[indNow,"propPeak"] <- datTrp$mass_prop[indPeak]
      # tmpPeak[1,"prop9Wk"] <- sum(datTrp$mass_prop[datTrp$week %in% (wkPeak-4):(wkPeak + 4)])
      # tmpPeak[1,"prop5Wk"] <- sum(datTrp$mass_prop[datTrp$week %in% (wkPeak-2):(wkPeak + 2)])
      # tmpPeak[1,"prop3Wk"] <- sum(datTrp$mass_prop[datTrp$week %in% (wkPeak-1):(wkPeak + 1)])
    }
  }
}
# weekPeakTrap$date <- as.Date(weekPeakTrap$date)
summary(leafTrapTotal)
summary(leafTrapTotal[is.na(leafTrapTotal$weekPeak),])
# weekPeak[weekPeak$prop5Wk<0.5,]

leafTrapTotal <- merge(leafTrapTotal, metSummer, all.x=T)
summary(leafTrapTotal)

ggplot(data=leafTrapTotal) +
  geom_boxplot(aes(x=as.factor(year), y=weekPeak)) +
  geom_point(aes(x=as.factor(year), y=weekPeak, color=plot), position=position_jitter(0.1))


ggplot(data=leafTrapTotal) +
  # geom_boxplot(aes(x=as.factor(year), y=weekPeak)) +
  geom_point(aes(x=prcp..mm.day., y=weekPeak, color=plot)) +
  stat_smooth(aes(x=prcp..mm.day., y=weekPeak), method="lm")

lmYrs <- lm(weekPeak ~ as.factor(year), data=leafTrapTotal)
summary(lmYrs)
anova(lmYrs)

lmRainless <- lm(weekPeak ~ n.Rainless, data=leafTrapTotal)
summary(lmRainless)
anova(lmRainless)


lmeYrs <- lme(weekPeak ~ as.factor(year), random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeYrs)
anova(lmeYrs)

yrsComp <- emmeans(lmeYrs, ~year)
pairs(yrsComp, adjust="tukey")

lmeYrsInt <- lme(weekPeak ~ as.factor(year)-1, random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeYrsInt)

mean(leafTrapTotal$weekPeak[leafTrapTotal$year==2021])
mean(leafTrapTotal$weekPeak[leafTrapTotal$year==2018])
mean(leafTrapTotal$weekPeak[leafTrapTotal$year==2022])

ggplot(data=leafTrapTotal) +
  # geom_boxplot(aes(x=as.factor(year), y=weekPeak)) +
  geom_point(aes(x=n.Rainless, y=weekPeak, color=plot)) +
  stat_smooth(aes(x=n.Rainless, y=weekPeak), method="lm")


lmeRainless <- lme(weekPeak ~ n.Rainless, random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeRainless)
anova(lmeRainless)

mean(leafTrapTotal$n.Rainless[leafTrapTotal$year==2021])
mean(leafTrapTotal$n.Rainless[leafTrapTotal$year==2018])
mean(leafTrapTotal$n.Rainless[leafTrapTotal$year==2022])


lmeRainlessConsec <- lme(weekPeak ~ RainlessConsec.max , random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeRainlessConsec)
anova(lmeRainlessConsec)

mean(leafTrapTotal$RainlessConsec.max[leafTrapTotal$year==2021])
mean(leafTrapTotal$RainlessConsec.max[leafTrapTotal$year==2018])
mean(leafTrapTotal$RainlessConsec.max[leafTrapTotal$year==2022])

ggplot(data=leafTrapTotal) +
  # geom_boxplot(aes(x=as.factor(year), y=weekPeak)) +
  geom_point(aes(x=RainlessConsec.max, y=weekPeak, color=plot)) +
  stat_smooth(aes(x=RainlessConsec.max, y=weekPeak), method="lm")


lmePrcp <- lme(weekPeak ~ prcp..mm.day., random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmePrcp)
anova(lmePrcp)

ggplot(data=leafTrapTotal) +
  # geom_boxplot(aes(x=as.factor(year), y=weekPeak)) +
  geom_point(aes(x=tmax..deg.c., y=weekPeak, color=plot)) +
  stat_smooth(aes(x=tmax..deg.c., y=weekPeak), method="lm")


lmeTmax <- lme(weekPeak ~ tmax..deg.c., random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeTmax)
anova(lmeTmax)

mean(leafTrapTotal$tmax..deg.c.[leafTrapTotal$year==2021])
mean(leafTrapTotal$tmax..deg.c.[leafTrapTotal$year==2018])
mean(leafTrapTotal$tmax..deg.c.[leafTrapTotal$year==2022])


lmeTmaxRainless <- lme(weekPeak ~ RainlessConsec.Tmax.mean , random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeTmaxRainless)
anova(lmeTmaxRainless)

ggplot(data=leafTrapTotal) +
  # geom_boxplot(aes(x=as.factor(year), y=weekPeak)) +
  geom_point(aes(x=vp..Pa., y=weekPeak, color=plot)) +
  stat_smooth(aes(x=vp..Pa., y=weekPeak), method="lm")

lmeVP <- lme(weekPeak ~ vp..Pa., random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeVP)
anova(lmeVP)

mean(leafTrapTotal$vp..Pa.[leafTrapTotal$year==2021])
mean(leafTrapTotal$vp..Pa.[leafTrapTotal$year==2018])
mean(leafTrapTotal$vp..Pa.[leafTrapTotal$year==2022])

lmeYrs2021 <- lme(weekPeak ~ relevel(as.factor(year), "2021"), random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeYrs2021)
anova(lmeYrs2021)

# Aggregating Leaves to the species level
aggLeafSpp <- aggregate(cbind(mass_g, mass_g_day) ~ year + week + plot + trap_ID + genus + species, data=datLitter[datLitter$tissue=="leaf",], FUN=sum)
aggLeafSpp$sci_name <- as.factor(paste(aggLeafSpp$genus, aggLeafSpp$species))
summary(aggLeafSpp)

# png(file.path(path.figs, "LeafMass_byTrap_byWeek_latest.png"), height=6, width=8, units="in", res=220)
ggplot(data=aggLeafSpp[aggLeafSpp$genus %in% c("Quercus", "Acer") & aggLeafSpp$species %in% c("alba", "rubra", "saccharum"),]) +
  facet_grid(year~sci_name) +
  # facet_wrap(~tissue, scales="free_y") +
  geom_boxplot(aes(x=as.factor(week), y=mass_g_day)) +
  # stat_summary(geom="line", aes(x=week, y=mass_g), fun="mean") +
  labs(x="week", y="mass (g)") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_bw()
# dev.off()


aggLeafSppTot <- aggregate(mass_g ~ year + plot + trap_ID + genus + species, data=datLitter[datLitter$tissue=="leaf",], FUN=sum, na.rm=T)
aggLeafSppTot$sci_name <- as.factor(paste(aggLeafSppTot$genus, aggLeafSppTot$species))
summary(aggLeafSppTot)


summary(aggLeafSppTot)
for(YR in unique(aggLeafSppTot$year)){
  # print(YR)
  datYr <- aggLeafSpp[aggLeafSpp$year==YR,]
  for(PLT in unique(datYr$plot)){
    # print(PLT)
    datPlot <- datYr[datYr$plot==PLT,]
    for(TRP in unique(datPlot$trap_ID)){
      datTrp <- datPlot[datPlot$trap_ID==TRP,]
      
      for(SPP in unique(datTrp$sci_name)){
        indNow <- which(aggLeafSppTot$year==YR & aggLeafSppTot$plot==PLT & aggLeafSppTot$trap_ID==TRP & aggLeafSppTot$sci_name==SPP)
        
        datSpp <- datTrp[datTrp$sci_name==SPP,]
        # print(paste(TRP, "-", indNow))
        indPeak <- which(datSpp$mass_g==max(datSpp$mass_g, na.rm=T))
        if(length(indPeak)>1){
          wkPeak <- median(datSpp$week[indPeak])
          indPeak <- indPeak[1]
        } else {
          wkPeak <- datSpp$week[indPeak]
        }
        # tmpPleafTrapTotaleak <- data.frame(year = YR, plot=PLT, trap_ID=TRP, week=NA, date=NA, propPeak=NA)
        aggLeafSppTot[indNow,"weekPeak"] <- wkPeak
        # leafTrapTotal[indNow,"date"] <- as.character(datSpp$date_collection[indPeak])
        aggLeafSppTot[indNow,"propPeak"] <- datSpp$mass_g[indPeak]/sum(datSpp$mass_g)
      }
    }
  }
}
# weekPeakTrap$date <- as.Date(weekPeakTrap$date)
summary(aggLeafSppTot)
aggLeafSppTot <- aggLeafSppTot[aggLeafSppTot$year < 2023,]
# summary(aggLeafSppTot[is.na(aggLeafSppTot$weekPeak),])


ggplot(data=aggLeafSppTot[aggLeafSppTot$sci_name %in% c("Acer saccharum", "Quercus alba", "Quercus rubra") & aggLeafSppTot$year < 2023,]) +
  facet_wrap(~sci_name, ncol=2) +
  geom_point(aes(x=as.factor(year), y=weekPeak, color=plot), position=position_jitter(0.1)) +
  geom_boxplot(aes(x=as.factor(year), y=weekPeak)) 


lmeYrsSpp <- lme(weekPeak ~ as.factor(year)*sci_name, random=list(plot=~1, trap_ID=~1), data=aggLeafSppTot[aggLeafSppTot$sci_name %in% c("Acer saccharum", "Quercus alba", "Quercus rubra"),])
summary(lmeYrsSpp)
anova(lmeYrsSpp)


lmeYrsQURU <- lme(weekPeak ~ as.factor(year), random=list(plot=~1, trap_ID=~1), data=aggLeafSppTot[aggLeafSppTot$sci_name %in% c("Quercus rubra"),])
summary(lmeYrsQURU)
anova(lmeYrsQURU)

quruComp <- emmeans(lmeYrsQURU, ~year)
pairs(quruComp, adjust="tukey")


lmeYrsQUAL <- lme(weekPeak ~ as.factor(year), random=list(plot=~1, trap_ID=~1), data=aggLeafSppTot[aggLeafSppTot$sci_name %in% c("Quercus alba"),])
summary(lmeYrsQUAL)
anova(lmeYrsQUAL)

qualComp <- emmeans(lmeYrsQUAL, ~year)
pairs(qualComp, adjust="tukey")


lmeYrsACSA <- lme(weekPeak ~ as.factor(year), random=list(plot=~1, trap_ID=~1), data=aggLeafSppTot[aggLeafSppTot$sci_name %in% c("Acer saccharum"),])
summary(lmeYrsACSA)
anova(lmeYrsACSA)

acsaComp <- emmeans(lmeYrsACSA, ~year)
pairs(acsaComp, adjust="tukey")

# Just out of Christy's curiosity
lmeYrsTIAM <- lme(weekPeak ~ as.factor(year), random=list(plot=~1, trap_ID=~1), data=aggLeafSppTot[aggLeafSppTot$sci_name %in% c("Tilia americana"),])
summary(lmeYrsTIAM)
anova(lmeYrsTIAM)

tiamComp <- emmeans(lmeYrsTIAM, ~year)
pairs(tiamComp, adjust="tukey")
