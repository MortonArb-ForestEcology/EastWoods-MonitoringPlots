# Loading, formatting, and exploratory graphing of the Understory community data
library(ggplot2)
source("0_Format_Understory_Data.R")

path.google <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Understory_Vegetation/"
path.figs <- file.path(path.google, "figures")
path.save <- file.path(path.google, "UnderstoryData_clean_forArchiving")
dir.create(path.figs, recursive = T, showWarnings = F)

# Using a formatting theme consistent with what Meghan has done
theme.meghan <-   theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_rect(fill=NA, colour = "black", size=.7),
                        axis.title.x = element_text(margin = margin(t = 10, b=5), size=14),
                        axis.title.y = element_text(margin = margin(l = 5, r=5), size=14),
                        axis.text.x= element_text(margin = margin(t = 10), size=12),
                        axis.text.y=element_text(margin = margin(r = 10), size=12),
                        axis.ticks.length=unit(-0.3, "cm"),
                        # axis.ticks.margin=unit(0.5, "cm"),
                        axis.ticks = element_line(colour = "black", linewidth = 0.4))

plotOrder <- c("B-127", "U-134", "N-115", "HH-115")
ewPlotColors <- c("#1B9E77","#D95F02", "#7570B3", "#E7298A")
names(ewPlotColors) = plotOrder
ewPlotColors

yrNow=lubridate::year(Sys.Date())
ydayLabs <- data.frame(Date=seq.Date(from=as.Date(paste0(yrNow, "-03-01")), to=as.Date(paste0(yrNow, "-12-01")), by=14))
ydayLabs$yday <- lubridate::yday(ydayLabs$Date)
ydayLabs$moName <- lubridate::month(ydayLabs$Date, label=T)
ydayLabs$day <- lubridate::day(ydayLabs$Date)

# # # # # # # # # # # # # # # # # # # # # 
# load past years data ---- 
# # # # # # # # # # # # # # # # # # # # # 
fpast <- dir(path.save)
dat.veg <- data.frame()
for(i in seq_along(fpast)){
  datNow <- read.csv(file.path(path.save, fpast[i]))
  
  dat.veg <- rbind(dat.veg, datNow)
}
summary(dat.veg)
dat.veg$Plot <- as.factor(dat.veg$Plot)
dat.veg$Subplot <- as.factor(dat.veg$Subplot)
dat.veg$Obs.Date <- as.Date(dat.veg$Obs.Date)
dat.veg$year <- lubridate::year(dat.veg$Obs.Date)
dat.veg$yday <- lubridate::yday(dat.veg$Obs.Date)
dat.veg$month <- lubridate::month(dat.veg$Obs.Date)
dat.veg$week <- lubridate::week(dat.veg$Obs.Date)
dat.veg$Genus <- as.factor(dat.veg$Genus)
dat.veg$Species <- as.factor(dat.veg$Species)
dat.veg$GenusSpecies <-as.factor( paste(dat.veg$Genus, dat.veg$Species))
summary(dat.veg)

length(unique(dat.veg$Genus[!grepl("UNKNOWN", dat.veg$Genus)]))
length(unique(dat.veg$GenusSpecies[!grepl("UNKNOWN", dat.veg$GenusSpecies)]))

# # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # 
# Get, save, merge current data ----
# # # # # # # # # # # # # # # # # # # # # 
# Load the understory data for this year
datNow <- get.understory(YEAR=lubridate::year(Sys.Date()))
datNow <- datNow[datNow$Name.Common!="NOTHING" & datNow$Cover>0 & !is.na(datNow$Phenophase.Codes),]
summary(datNow)

# Save an "Up to" file with the archive
#  # Code currently pirated from the met station format
fileSave <- paste0("MortonArb_EastWoods_Understory_Vegetation_", max(lubridate::year(datNow$Obs.Date)), "_up_to_" , max(datNow$Obs.Date), ".csv")
write.csv(datNow[,], file.path(path.save, fileSave), row.names = FALSE)

# # If we now have >1 "up_to" file for the year, delete it
fWorking <- dir(path.save, "_up_to_")
if(length(fWorking)>1){
  for(i in 1:(length(fWorking)-1)){
    file.remove(file.path(path.save, fWorking[i]))
  }
}

# Add to past data
datNow$year <- lubridate::year(datNow$Obs.Date)
datNow$yday <- lubridate::yday(datNow$Obs.Date)
datNow$month <- lubridate::month(datNow$Obs.Date)
datNow$week <- lubridate::week(datNow$Obs.Date)
datNow$Genus <- as.factor(datNow$Genus)
datNow$Species <- as.factor(datNow$Species)
datNow$GenusSpecies <-as.factor( paste(datNow$Genus, datNow$Species))
summary(datNow)

dat.veg <- rbind(dat.veg, datNow)
dat.veg$Plot <- factor(dat.veg$Plot, levels=plotOrder)
# # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # 
# Make some figures! ----
# # # # # # # # # # # # # # # # # # # # # 
# 1. what is out and/or flowering -- pull anything typically active in a 30-day window of today(?)
#.   - Flowering, Present
#.   -- summarize by plot --> x=axis = DOY, dots for present; gray for past years; color A for current year, color B for last year; don't distinguish subplots (not going for averages, just presence & somewhat frequency)
#.   - for leaves, *could* consider color = intensity, but that could get too messy
yrNow <- lubridate::year(max(dat.veg$Obs.Date))
ydayNow <- lubridate::yday(max(dat.veg$Obs.Date))

# Subsetting to just flower observations in a 60-day window around our last obs
sppFlowerYday60 <- unique(dat.veg$GenusSpecies[dat.veg$yday>=ydayNow-30 & dat.veg$yday<=ydayNow+30 & dat.veg$Flowers.Open])

plotFlower <- ggplot(data=dat.veg[dat.veg$GenusSpecies %in% sppFlowerYday60,]) +
  ggtitle(paste("Open Flowers:", max(dat.veg$Obs.Date))) +
  facet_wrap(~Plot) +
  geom_point(data=dat.veg[dat.veg$GenusSpecies %in% sppFlowerYday60 & dat.veg$year==yrNow & !dat.veg$Flowers.Open,], aes(x=yday, y=GenusSpecies, color="No Open Flowers - This Year"), size=3.5) + 
  geom_point(data=dat.veg[dat.veg$GenusSpecies %in% sppFlowerYday60 &  !dat.veg$Flowers.Open,], aes(x=yday, y=GenusSpecies, color="No Open Flowers")) +
  geom_point(data=dat.veg[dat.veg$GenusSpecies %in% sppFlowerYday60 & dat.veg$year==yrNow & dat.veg$Flowers.Open,], aes(x=yday, y=GenusSpecies, color="Open Flowers - This Year"), size=3) + 
  geom_point(data=dat.veg[dat.veg$GenusSpecies %in% sppFlowerYday60 & dat.veg$Flowers.Open,], aes(x=yday, y=GenusSpecies, color="Open Flowers - Past")) +
  scale_x_continuous(name="Date", limits=c(ydayNow-30, ydayNow+30), breaks=ydayLabs$yday, labels = paste(ydayLabs$moName, ydayLabs$day)) +
  scale_color_manual(values=c("No Open Flowers"="gray80", "Open Flowers - Past" = "plum2", "No Open Flowers - This Year"="gray50", "Open Flowers - This Year"="purple3")) +
  theme_bw() +
  theme(legend.position = "right")

png(file.path(path.figs, "Understory_Flowering_Window-60day_latest.png"), height=6, width=10, units="in", res=240)
plotFlower
dev.off()


sppLeafYday60 <- unique(dat.veg$GenusSpecies[dat.veg$yday>=ydayNow-30 & dat.veg$yday<=ydayNow+30 & dat.veg$Leaves.Present])


plotLeaf <- ggplot(data=dat.veg[dat.veg$GenusSpecies %in% sppLeafYday60,]) +
  ggtitle(paste("Leaves Present:", max(dat.veg$Obs.Date))) +
  facet_wrap(~Plot, scales="free_y") +
  geom_point(data=dat.veg[dat.veg$GenusSpecies %in% sppFlowerYday60 & dat.veg$year==yrNow & !dat.veg$Leaves.Present,], aes(x=yday, y=GenusSpecies, color="No Leaves - This Year"), size=2.5) +
  geom_point(data=dat.veg[dat.veg$GenusSpecies %in% sppLeafYday60 &  !dat.veg$Leaves.Present,], aes(x=yday, y=GenusSpecies, color="No Leaves")) +
  geom_point(data=dat.veg[dat.veg$GenusSpecies %in% sppLeafYday60 & dat.veg$year==yrNow & dat.veg$Leaves.Present,], aes(x=yday, y=GenusSpecies, color="Leaves - This Year"), size=3) + 
  geom_point(data=dat.veg[dat.veg$GenusSpecies %in% sppLeafYday60 &  dat.veg$Leaves.Present,], aes(x=yday, y=GenusSpecies, color="Leaves - Past")) +
  scale_x_continuous(name="Date", limits=c(ydayNow-30, ydayNow+30), breaks=ydayLabs$yday, labels = paste(ydayLabs$moName, ydayLabs$day)) +
  theme_bw() +
  scale_color_manual(values=c("No Leaves"="gray80", "Leaves - Past" = "palegreen3", "No Leaves - This Year"="gray50", "Leaves - This Year"="darkolivegreen")) +
  theme(legend.position = "right")

png(file.path(path.figs, "Understory_Leaves_Window-60day_latest.png"), height=12, width=12, units="in", res=320)
plotLeaf
dev.off()

# # # # # # # # # # # # # # # # # # # # # 
