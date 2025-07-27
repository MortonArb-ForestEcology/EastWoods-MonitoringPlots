library(ggplot2)
library(tidyverse)
library(nlme);  # Does the mixed effects model
library(emmeans) # will et us do a multi-comparisons test
library(MuMIn)

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
varsDrought <- c("prcp..mm.day.", "VPD.tmax", "tmax..deg.c.", "n.Rainless")

summary(weekPeakTotal)
weekPeakStack <- stack(weekPeakTotal[,varsDrought])
weekPeakStack[,c("year", "plot", "trap_ID", "weekPeakWt", "totalMass_year")] <- weekPeakTotal[,c("year", "plot", "trap_ID", "weekPeakWt", "totalMass_year")]
summary(weekPeakStack)

ggplot(data=weekPeakStack, aes(x=values, y=weekPeakWt)) +
  facet_wrap(~ind, scales="free_x") +
  geom_point() +
  stat_smooth(method="lm")

summary(weekPeakSpp)
weekPeakStackSpp <- stack(weekPeakSpp[,varsDrought])
weekPeakStackSpp[,c("year", "plot", "sci_name", "weekPeakWt", "totalMass_year")] <- weekPeakSpp[,c("year", "plot", "sci_name", "weekPeakWt")]
summary(weekPeakStackSpp)

ggplot(data=weekPeakStackSpp, aes(x=values, y=weekPeakWt, color=sci_name, fill=sci_name)) +
  facet_wrap(~ind, scales="free_x") +
  geom_point() +
  stat_smooth(method="lm")

summary(datLLNpeak)
llnStackSpp <- stack(datLLNpeak[,varsDrought])
llnStackSpp[,c("year", "plot", "sci_name", "C.N.weighted", "perN.weighted", "perC.weighted")] <- datLLNpeak[,c("year", "plot", "sci_name", "C.N.weighted", "perN.weighted", "perC.weighted")]
summary(llnStackSpp)

ggplot(data=llnStackSpp, aes(x=values, y=perN.weighted, color=sci_name, fill=sci_name)) +
  facet_wrap(~ind, scales="free_x") +
  geom_point() +
  stat_smooth(method="lm")


# Based off of the above exploratory figures, lets roll with precip & n.Rainless as our vars
summary(weekPeakTotal)
totDropPrecip <- lme(weekPeakWt ~ prcp..mm.day., random=list(plot=~1, trap_ID=~1), data=weekPeakTotal)
summary(totDropPrecip)
r.squaredGLMM(totDropPrecip)

ggplot(weekPeakTotal, aes(x=prcp..mm.day., y=weekPeakWt)) +
  geom_point() +
  stat_smooth(method="lm")

totDropRainless <- lme(weekPeakWt ~ n.Rainless, random=list(plot=~1, trap_ID=~1), data=weekPeakTotal)
summary(totDropRainless)
r.squaredGLMM(totDropRainless)

ggplot(weekPeakTotal, aes(x=n.Rainless, y=weekPeakWt)) +
  geom_point() +
  stat_smooth(method="lm")

# Working at the plot level to make it comparable to our nitrogen data
sppDropPrecip <- lme(weekPeakWt ~ prcp..mm.day.*sci_name, random=list(plot=~1), data=weekPeakSpp)
summary(sppDropPrecip)
anova(sppDropPrecip)
r.squaredGLMM(sppDropPrecip)

sppDropPrecip2 <- lme(weekPeakWt ~ prcp..mm.day., random=list(plot=~1, sci_name=~1), data=weekPeakSpp)
summary(sppDropPrecip2)
anova(sppDropPrecip2)
r.squaredGLMM(sppDropPrecip2)

ggplot(weekPeakSpp, aes(x=prcp..mm.day., y=weekPeakWt, color=sci_name, fill=sci_name)) +
  geom_point() +
  stat_smooth(method="lm")


# Now looking at leaf percent Nitrogen
sppNPrecip <- lme(perN.weighted ~ prcp..mm.day.*sci_name, random=list(plot=~1), data=datLLNpeak)
summary(sppNPrecip)
anova(sppNPrecip)
r.squaredGLMM(sppNPrecip)

sppNPrecip2 <- lme(perN.weighted ~ prcp..mm.day., random=list(plot=~1, sci_name=~1), data=datLLNpeak)
summary(sppNPrecip2)
anova(sppNPrecip2)
r.squaredGLMM(sppNPrecip2)

ggplot(datLLNpeak, aes(x=prcp..mm.day., y=perN.weighted, color=sci_name, fill=sci_name)) +
  geom_point() +
  stat_smooth(method="lm")


summary(datLLNpeak)
summary(weekPeakSpp)
datLLNpeak <- merge(datLLNpeak, weekPeakSpp[,c("year", "plot", "sci_name", "weekPeakWt", "propPeak", "prop3Wk")], all.x=T, all.y=F)
summary(datLLNpeak)

ggplot(datLLNpeak, aes(x=weekPeakWt, y=perN.weighted, color=sci_name, fill=sci_name)) +
  geom_point() +
  stat_smooth(method="lm")

peakVn <- lme(perN.weighted ~ weekPeakWt, random=list(plot=~1, sci_name=~1), data=datLLNpeak)
summary(peakVn)
anova(peakVn)
r.squaredGLMM(peakVn)
