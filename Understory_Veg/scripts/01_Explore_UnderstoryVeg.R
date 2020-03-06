# Loading, formatting, and exploratory graphing of the Understory community data
library(ggplot2)
source("0_Format_Understory_Data.R")

path.google <- "/Volumes/GoogleDrive/My Drive/East Woods/Rollinson_Monitoring/Data/Understory_Vegetation/"
path.figs <- file.path(path.google, "figures")
dir.create(path.figs, recursive = T, showWarnings = F)

# Load the 2019 Understory plot data from 
dat.2019 <- get.understory(YEAR=2019)
dat.2019 <- dat.2019[!dat.2019$Name.Common=="NOTHING",]
summary(dat.2019)
unique(dat.2019$Name.Common)

plot.summary <- aggregate(Cover ~ Plot + Subplot + Obs.Date, data=dat.2019, FUN=sum)
plot.summary$Richness <- aggregate(Cover ~ Plot + Subplot + Obs.Date, data=dat.2019, FUN=length)$Cover
summary(plot.summary)

png(file.path(path.figs, "UnderstoryVegetation_2019_TotalCover.png"), height=6, width=6, units="in", res=120)
ggplot(data=plot.summary) +
  facet_wrap(~Plot) +
  geom_boxplot(aes(x=Obs.Date, y=Cover, group=Obs.Date))
dev.off()

png(file.path(path.figs, "UnderstoryVegetation_2019_SpeciesRichness.png"), height=6, width=6, units="in", res=120)
ggplot(data=plot.summary) +
  facet_wrap(~Plot) +
  geom_boxplot(aes(x=Obs.Date, y=Richness, group=Obs.Date))
dev.off()



# get the data from a particular sheet



# Load the 2020 Understory plot data