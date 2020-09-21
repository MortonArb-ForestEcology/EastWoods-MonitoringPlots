# Loading, formatting, and exploratory graphing of the Understory community data
library(ggplot2)
source("0_Format_Understory_Data.R")

path.google <- "/Volumes/GoogleDrive/My Drive/East Woods/Rollinson_Monitoring/Data/Understory_Vegetation/"
path.figs <- file.path(path.google, "figures")
dir.create(path.figs, recursive = T, showWarnings = F)

# Load the 2019 Understory plot data from 
dat.2019 <- get.understory(YEAR=2019)
dat.2019 <- dat.2019[dat.2019$Name.Common!="NOTHING" & dat.2019$Cover>0 & !is.na(dat.2019$Cover),]
summary(dat.2019)
dim(dat.2019)
unique(dat.2019$Name.Common)

dat.2020 <- get.understory(YEAR=2020)
dat.2020 <- dat.2020[dat.2020$Name.Common!="NOTHING" & dat.2020$Cover>0 & !is.na(dat.2020$Cover),]
summary(dat.2020)
dim(dat.2020)

# Adding in missing data for 2020
plots.ew <- data.frame(Plot=rep(unique(dat.2020$Plot), each=length(unique(dat.2020$Subplot))), 
                       Subplot=rep(unique(dat.2020$Subplot), length(unique(dat.2020$Plot))))
dat.missing <- data.frame(Obs.Date=seq.Date(from=as.Date("2020-03-27"), to=as.Date("2020-05-31"), by=7 ))
dat.missing <- merge(dat.missing, plots.ew)
dat.missing$Year <- lubridate::year(dat.missing$Obs.Date)
dat.missing$Yday <- lubridate::yday(dat.missing$Obs.Date)
summary(dat.missing)

# dat.2020 <- merge(dat.2020, dat.missing, all=T)


dat.ew <- rbind(dat.2019, dat.2020)
dat.ew$Year <- lubridate::year(dat.ew$Obs.Date)
dat.ew$Yday <- lubridate::yday(dat.ew$Obs.Date)

plot.summary <- aggregate(Cover ~ Plot + Subplot + Obs.Date + Year + Yday, data=dat.ew, FUN=sum)
plot.summary$Richness <- aggregate(Cover ~ Plot + Subplot + Obs.Date + Year + Yday, data=dat.ew, FUN=length)$Cover
summary(plot.summary)

# Add in missing 2020 dates
plot.summary <- merge(plot.summary, dat.missing[,], all=T)
summary(plot.summary)

plot.summary$Plot <- factor(plot.summary$Plot, levels=c("B-127", "U-134", "N-115", "HH-115"))

png(file.path(path.figs, "UnderstoryVegetation_TotalCover.png"), height=6, width=6, units="in", res=120)
ggplot(data=plot.summary[plot.summary$Yday>max(dat.missing$Yday),], aes(x=Yday, y=Cover)) +
  facet_wrap(~Plot) +
  geom_ribbon(aes(fill=as.factor(Year)), stat="summary", fun.ymin=min, fun.ymax=max, alpha=0.5) +
  geom_line(aes(color=as.factor(Year)), stat="summary", fun.y=mean)
dev.off()

png(file.path(path.figs, "UnderstoryVegetation_SpeciesRichness.png"), height=6, width=6, units="in", res=120)
ggplot(data=plot.summary[plot.summary$Yday>max(dat.missing$Yday),], aes(x=Yday, y=Richness)) +
  facet_wrap(~Plot) +
  geom_ribbon(aes(fill=as.factor(Year)), stat="summary", fun.ymin=min, fun.ymax=max, alpha=0.5) +
  geom_line(aes(color=as.factor(Year)), stat="summary", fun.y=mean)
dev.off()



# get the data from a particular sheet



# Load the 2020 Understory plot data