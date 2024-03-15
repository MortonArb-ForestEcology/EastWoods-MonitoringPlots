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
# filename <- paste0(PLOT,"_", max(yrsFile), "_up_to_" , max(plotAll$Date), ".csv")
# write.csv(plotAll[,], file.path(path.clean, PLOT,  file = filename), row.names = FALSE)
# 
# # # Adding a check to make sure the timestamp is working
# # TEST <- read.csv(file.path(path.clean, PLOT,  file = filename))
# # TEST$Timestamp <- as.POSIXct(TEST$Timestamp, tz="Etc/GMT+6")
# # TEST$Date <- as.Date(TEST$Date)
# # summary(TEST)
# # head(TEST)
# # tail(TEST)
# 
# # If we now have >1 "up_to" file for the year, delete it
# fWorking <- dir(file.path(path.clean, PLOT), paste0(PLOT,"_", max(yrsFile), "_up_to_"))
# if(length(fWorking)>1){
#   for(i in 1:(length(fWorking)-1)){
#     file.remove(file.path(path.clean, PLOT, fWorking[i]))
#   }
# }

# Add to past data
# # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # 
# Make some figures! ----
# # # # # # # # # # # # # # # # # # # # # 
# 1. what is out and/or flowering -- pull anything typically active in a 30-day window of today(?)
#.   - Flowering, Present
#.   -- summarize by plot --> x=axis = DOY, dots for present; gray for past years; color A for current year, color B for last year; don't distinguish subplots (not going for averages, just presence & somewhat frequency)
#.   - for leaves, *could* consider color = intensity, but that could get too messy
ydayNow <- lubridate::yday()

# # # # # # # # # # # # # # # # # # # # # 
