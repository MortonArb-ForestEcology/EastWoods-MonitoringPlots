# Exploring the leaf litter data to identify peak times of deposition

library(ggplot2)
library(tidyverse)

# Set up file paths etc. --> this should also indicate where you can find these files!
path.google <- "~/Google Drive/My Drive"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.figs <- file.path(path.litter, "figures") # where we shoudl save some figures
path.save <- file.path(path.litter, "LeafLitterData_Clean_forArchiving") # Where we shoudl save the data


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

# There is no HH-115 NE, NW, SE, SW --> it has a weird layout
datLitter[datLitter$plot=="HH-115" & datLitter$trap_ID %in% c("NE", "NW", "SE", "SW"), "trap_ID"] <- NA

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

ggplot(data=aggLeafTrap) +
  facet_grid(year~plot) +
  # facet_wrap(~tissue, scales="free_y") +
  # geom_boxplot(aes(x=as.factor(week), y=mass_g, color=plot)) +
  geom_point(aes(x=week, y=mass_g, color=plot)) +
  stat_summary(geom="line", aes(x=week, y=mass_g), fun="mean") +
  labs(x="week", y="mass (g)") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_bw()

# Lets calculate the proportion of leaf fall at any given point in time
leafTrapTotal <- aggregate(mass_g ~ year + plot + trap_ID, data=aggLeafTrap, FUN=sum)
names(leafTrapTotal)[names(leafTrapTotal)=="mass_g"] <- "totalMass_year"
summary(leafTrapTotal)
hist(leafTrapTotal$totalMass_year)
hist(leafTrapTotal$totalMass_year[leafTrapTotal$year<2023])
summary(leafTrapTotal[leafTrapTotal$year<2023,])

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
aggLeafPlot <- aggregate(cbind(mass_g, mass_prop, totalMass_year)~ year + plot + week + date_collection, data=aggLeafTrap, FUN=mean)
summary(aggLeafPlot)

weekPeak <- data.frame(year=rep(unique(aggLeafTrap$year), each=length(unique(aggLeafPlot$plot))),
                       plot=rep(unique(aggLeafPlot$plot)),
                       week=NA,
                       date=NA,
                       propPeak = NA,
                       prop9Wk = NA,
                       prop5Wk = NA,
                       prop3Wk = NA)
weekPeak <- weekPeak[weekPeak$year<max(weekPeak$year),]

for(YR in unique(weekPeak$year)){
  datYr <- aggLeafPlot[aggLeafPlot$year==YR,]
  for(PLT in unique(datYr$plot)){
    indNow <- which(weekPeak$year==YR & weekPeak$plot==PLT)
    datPlot <- datYr[datYr$plot==PLT,]
    
    indPeak <- which(datPlot$mass_prop==max(datPlot$mass_prop))
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
      indPeak <- which(datTrp$mass_prop==max(datTrp$mass_prop, na.rm=T))
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
weekPeak[weekPeak$prop5Wk<0.5,]

ggplot(data=leafTrapTotal) +
  geom_boxplot(aes(x=as.factor(year), y=weekPeak)) +
  geom_point(aes(x=as.factor(year), y=weekPeak, color=plot), position=position_jitter(0.1))

lmYrs <- lm(week ~ as.factor(year), data=weekPeakTrap)
summary(lmYrs)
anova(lmYrs)

library(nlme);  # Does the mixed effects model
library(emmeans) # will et us do a multi-comparisons test
lmeYrs <- lme(weekPeak ~ as.factor(year), random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeYrs)
anova(lmeYrs)

lmeYrs2021 <- lme(weekPeak ~ relevel(as.factor(year), "2021"), random=list(plot=~1, trap_ID=~1), data=leafTrapTotal)
summary(lmeYrs2021)
anova(lmeYrs2021)

yrsComp <- emmeans(lmeYrs, ~year)
pairs(yrsComp, adjust="tukey")

# Aggregating Leaves to the species level
aggLeafSpp <- aggregate(mass_g ~ year + week + plot + trap_ID + genus + species, data=datLitter[datLitter$tissue=="leaf",], FUN=sum)
aggLeafSpp$sci_name <- as.factor(paste(aggLeafSpp$genus, aggLeafSpp$species))
summary(aggLeafSpp)

# png(file.path(path.figs, "LeafMass_byTrap_byWeek_latest.png"), height=6, width=8, units="in", res=220)
ggplot(data=aggLeafSpp[aggLeafSpp$genus %in% c("Quercus", "Acer") & aggLeafSpp$species %in% c("alba", "rubra", "saccharum"),]) +
  facet_grid(year~plot) +
  # facet_wrap(~tissue, scales="free_y") +
  geom_boxplot(aes(x=as.factor(week), y=mass_g, color=plot)) +
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
