library(ggplot2)
library(tidyverse)
library(nlme);  # Does the mixed effects model
library(emmeans) # will et us do a multi-comparisons test


# Set up file paths etc. --> this should also indicate where you can find these files!
path.google <- "~/Google Drive/My Drive"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.figs <- file.path(path.litter, "figures") # where we shoudl save some figures
path.save <- file.path(path.litter, "LeafLitterData_Clean_forArchiving") # Where we should save the data
path.REU <- file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter ")


# Read in Data
aggLeafTrap <- read.csv(file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter ", "LeafLitter_byTrap_latest.csv"))
weekPeakTotal <- read.csv(file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter ", "LeafLitter_Peak_byTrap.csv"))

weekPeakSpp <- read.csv(file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter ", "PeakLeafDates_byPlot_bySpp.csv"))
weekPeakSpp <- weekPeakSpp[!is.na(weekPeakSpp$sci_name) & weekPeakSpp$sci_name %in% c("Quercus alba", "Quercus rubra", "Acer saccharum"),]
summary(weekPeakSpp)

datSpp <- read.csv(file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter ", "LeafLitter_byTrap_bySpecies_latest.csv"))
datSpp$date_collection <- as.Date(datSpp$date_collection)
datSpp <- datSpp[!is.na(datSpp$sci_name) & datSpp$sci_name %in% c("Quercus alba", "Quercus rubra", "Acer saccharum"),]
summary(datSpp)


datLLN <- read.csv(file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter ", "LeafLitter-Nitrogen_bySpecies_combined.csv"))
datLLN <- datLLN[!datLLN$sci_name %in% c("Tilia americana"),]
datLLN$date_collection <- as.Date(datLLN$date_collection)
datLLN$year <- lubridate::year(datLLN$date_collection)
datLLN$week <- lubridate::week(datLLN$date_collection)
datLLN$yday <- lubridate::yday(datLLN$date_collection)

metSummer <- read.csv(file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter /data/daymet/daymet_June-July-August_summaries_2017-2023.csv"))


summary(aggLeafTrap)
summary(leafTrapTotal)
summary(weekPeakSpp)
summary(datLLN)
summary(datSpp)
summary(metSummer)

# If we want to do a weighted-average of leaf N for our key species, we need to merge that in
aggLeafPlotSpp <- aggregate(cbind(mass_g, mass_g_day)~ year + plot + sci_name + week + date_collection, data=datSpp, FUN=mean, na.rm=T)
summary(aggLeafPlotSpp)


summary(datLLN)
datLLNmerge <- merge(datLLN, aggLeafPlotSpp[,c("year", "sci_name", "date_collection", "plot", "mass_g", "mass_g_day")], all.x=T, all.y=F)
datLLNmerge$mass_g_day_Prop <- NA
summary(datLLNmerge)

# Calculating the weights for our C:N data
for(PLT in unique(datLLNmerge$plot)){
  for(SPP in unique(datLLNmerge$sci_name[datLLNmerge$plot==PLT])){
    for(YR in unique(datLLNmerge$year[datLLNmerge$plot==PLT & datLLNmerge$sci_name==SPP])){
      indNow <- which(datLLNmerge$plot==PLT & datLLNmerge$sci_name==SPP & datLLNmerge$year==YR)
      
      datLLNmerge$mass_g_day_Prop[indNow] <- datLLNmerge$mass_g_day[indNow]/sum(datLLNmerge$mass_g_day[indNow], na.rm=T)
    }
  }
}
datLLNmerge$C.N.weighted <- datLLNmerge$C.N * datLLNmerge$mass_g_day_Prop
datLLNmerge$perN.weighted <- datLLNmerge$X.N * datLLNmerge$mass_g_day_Prop
datLLNmerge$perC.weighted <- datLLNmerge$X.C * datLLNmerge$mass_g_day_Prop
summary(datLLNmerge)

datLLNpeak <- aggregate(cbind(C.N.weighted, perN.weighted, perC.weighted) ~ plot + sci_name + year, data=datLLNmerge, FUN=sum)
summary(datLLNpeak)

# Merging summer met in with our key datasets
datLLNpeak <- merge(datLLNpeak, metSummer, all.x=T, all.y=F)
summary(datLLNpeak)

# Just a quick visual inspection!
ggplot(datLLNpeak, aes(x=prcp..mm.day., y=perN.weighted, color=sci_name, fill=sci_name)) +
  geom_point() +
  stat_smooth(method="lm")

weekPeakSpp <- merge(weekPeakSpp, metSummer, all.x=T, all.y=F)
summary(weekPeakSpp)

ggplot(weekPeakSpp, aes(x=prcp..mm.day., y=weekPeakWt, color=sci_name, fill=sci_name)) +
  geom_point() +
  stat_smooth(method="lm")


weekPeakTotal <- merge(weekPeakTotal, metSummer, all.x=T, all.y=F)
summary(weekPeakTotal)

ggplot(weekPeakTotal, aes(x=prcp..mm.day., y=weekPeakWt)) +
  geom_point() +
  stat_smooth(method="lm")

# Following the progression of analyses in Cierra's presentation 


