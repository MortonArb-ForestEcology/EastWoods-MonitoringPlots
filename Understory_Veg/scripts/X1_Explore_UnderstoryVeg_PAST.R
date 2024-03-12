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

# Load the understory data
# # NOTE: I haven't done any species harmonization, so lets not do community comparisions yet
yrs.ew <- 2019:2024
dat.veg <- list()
for(YR in yrs.ew){
  dat.yr <- get.understory(YEAR=YR)
  dat.yr <- dat.yr[dat.yr$Name.Common!="NOTHING" & dat.yr$Cover>0 & !is.na(dat.yr$Phenophase.Codes),]
  # dat.yr$GenusSpecies <- paste(dat.yr$Genus, dat.yr$Species)
  
  dat.veg[[paste(YR)]] <- dat.yr
}

# summary(dat.veg[["2019"]])
# summary(dat.veg[["2020"]])
# summary(dat.veg[["2021"]])
# summary(dat.veg[["2022"]])
# summary(dat.veg[["2023"]])
summary(dat.veg[["2024"]])

# summary(dat.veg[["2019"]][is.na(dat.veg$`2019`$Species),])
# summary(dat.veg[["2020"]][is.na(dat.veg$`2020`$Species),])
# summary(dat.veg[["2021"]][is.na(dat.veg$`2021`$Species),])
# summary(dat.veg[["2022"]][is.na(dat.veg$`2022`$Species),])
# summary(dat.veg[["2023"]][is.na(dat.veg$`2023`$Species),])
summary(dat.veg[["2024"]][is.na(dat.veg$`2024`$Species),])

# summary(as.factor(dat.veg[["2019"]]$GenusSpecies))
# summary(as.factor(dat.veg[["2020"]]$GenusSpecies))
# summary(as.factor(dat.veg[["2021"]]$GenusSpecies))
# summary(as.factor(dat.veg[["2022"]]$GenusSpecies))
# summary(as.factor(dat.veg[["2023"]]$GenusSpecies))
summary(as.factor(dat.veg[["2024"]]$GenusSpecies))


for(YR in names(dat.veg)){
  write.csv(dat.veg[[YR]], file.path(path.save, paste0("MortonArb_EastWoods_Understory_Vegetation_", YR, ".csv")), row.names=F)
}


# test <- read.csv("MortonArb_EastWoods_Understory_Vegetation_2019.csv")
# head(test)


########################
# Needs to be updated!
########################
files.past <- dir(path.save)

dat.veg <- data.frame()
for(i in seq_along(files.past)){
  datNow <- read.csv(file.path(path.save, files.past[i]))
  
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

sort(unique(dat.veg$GenusSpecies[!grepl("UNKNOWN", dat.veg$GenusSpecies)]))

veg.summary <- aggregate(Cover ~ Plot + Subplot + Obs.Date + year + yday, data=dat.veg, FUN=sum)
veg.summary$Richness <- aggregate(Cover ~ Plot + Subplot + Obs.Date + year + yday, data=dat.veg, FUN=length)$Cover
summary(veg.summary)

# Add in missing 2020 dates

veg.summary$Plot <- factor(veg.summary$Plot, levels=plotOrder)

veg.graph <- veg.summary$year!=2020 | (veg.summary$year==2020 & veg.summary$Obs.Date>as.Date("2020-05-01"))

plot.cover <- ggplot(data=veg.summary[veg.graph,], aes(x=Obs.Date, y=Cover)) +
  # facet_grid(.~Plot) +
  geom_ribbon(data=veg.summary[veg.summary$year<2020,], aes(fill=Plot), stat="summary", fun.ymin=min, fun.ymax=max, alpha=0.5) +
  geom_line(data=veg.summary[veg.summary$year<2020 ,], aes(color=Plot), stat="summary", fun.y=mean) +
geom_ribbon(data=veg.summary[veg.summary$year==2020 & veg.summary$Obs.Date>as.Date("2020-05-01"),], aes(fill=Plot), stat="summary", fun.ymin=min, fun.ymax=max, alpha=0.5) +
  geom_line(data=veg.summary[veg.summary$year==2020 & veg.summary$Obs.Date>as.Date("2020-05-01"),], aes(color=Plot), stat="summary", fun.y=mean) +
  geom_ribbon(data=veg.summary[veg.summary$year>2020,], aes(fill=Plot), stat="summary", fun.ymin=min, fun.ymax=max, alpha=0.5) +
  geom_line(data=veg.summary[veg.summary$year>2020 ,], aes(color=Plot), stat="summary", fun.y=mean) +
  labs(x="Observation Date", y="Total Veg Cover") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_linedraw() + theme.meghan + theme(legend.position="right")

plot.richness <- ggplot(data=veg.summary[veg.graph,], aes(x=Obs.Date, y=Richness)) +
  # facet_wrap(~Plot) +
  geom_ribbon(data=veg.summary[veg.summary$year<2020,], aes(fill=Plot), stat="summary", fun.ymin=min, fun.ymax=max, alpha=0.5) +
  geom_line(data=veg.summary[veg.summary$year<2020 ,], aes(color=Plot), stat="summary", fun.y=mean) +
  geom_ribbon(data=veg.summary[veg.summary$year==2020 & veg.summary$Obs.Date>as.Date("2020-05-01"),], aes(fill=Plot), stat="summary", fun.ymin=min, fun.ymax=max, alpha=0.5) +
  geom_line(data=veg.summary[veg.summary$year==2020 & veg.summary$Obs.Date>as.Date("2020-05-01"),], aes(color=Plot), stat="summary", fun.y=mean) +
  geom_ribbon(data=veg.summary[veg.summary$year>2020,], aes(fill=Plot), stat="summary", fun.ymin=min, fun.ymax=max, alpha=0.5) +
  geom_line(data=veg.summary[veg.summary$year>2020 ,], aes(color=Plot), stat="summary", fun.y=mean) +
  labs(x="Observation Date", y="Species Richness") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_linedraw() + theme.meghan + theme(legend.position="right")



png(file.path(path.figs, "UnderstoryVegetation_Cover_Richness.png"), height=6, width=8, units="in", res=220)
cowplot::plot_grid(plot.cover, plot.richness, ncol=1)
dev.off()

####################################
# Do a quick biodiveristy analysis
####################################
spp.yr <- aggregate(Cover ~ Plot + Subplot + Genus + Species + year, data=dat.veg, FUN=max)
summary(spp.yr)

# Calculating the relative proportion based on cover to do Shannon Diveristy
for(PLOT in unique(spp.yr$Plot)){
  for(SUB in unique(spp.yr$Subplot)){
    for(YR in unique(spp.yr$year)){
      rows.now <- which(spp.yr$Plot==PLOT & spp.yr$Subplot==SUB & spp.yr$year==YR)
      spp.yr[rows.now, "prop"] <- spp.yr[rows.now, "Cover"]/sum(spp.yr[rows.now, "Cover"])
    }
  }
}
# spp.yr$neg.p.lnp <- -spp.yr$prop*log(spp.yr$prop)
summary(spp.yr)

bd.yr <- aggregate(Cover ~ Plot + Subplot + year, data=spp.yr, FUN=length)
names(bd.yr)[names(bd.yr)=="Cover"] <- "Richness"
bd.yr$Hprime <- aggregate(prop ~ Plot + Subplot + year, data=spp.yr, FUN=function(x){-sum(x*log(x))})$prop
summary(bd.yr)


bd.yr$Plot <- factor(bd.yr$Plot, levels=plotOrder)

yr.hprime <- ggplot(data=bd.yr) +
  geom_boxplot(aes(x=as.factor(year), y=Hprime, fill=Plot)) +
  labs(x="Year", y="Shannon BD (H')") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_linedraw() + theme.meghan


yr.richness <- ggplot(data=bd.yr) +
  geom_boxplot(aes(x=as.factor(year), y=Richness, fill=Plot)) +
  labs(x="Year", y="Species Richness") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_linedraw() + theme.meghan


png(file.path(path.figs, "UnderstoryVegetation_Cover_Richness_v2.png"), height=6, width=12, units="in", res=220)
cowplot::plot_grid(plot.cover, yr.hprime, plot.richness, yr.richness, ncol=2, rel_widths = c(0.6, 0.4))
dev.off()

####################################
# Begin playing with the phenology data, but nothing really clear
####################################3

veg.long <- stack(dat.veg[,c("Initial.Growth", "Leaves.Present", "Leaves.Colored", "Flowers.Buds", "Flowers.Open", "Fruits.Present", "Fruits.Ripe", "Fruits.Drop")])
names(veg.long) <- c("Pheno.Status","Phenophase")
veg.long[,c("Plot", "Subplot", "Genus", "Species", "Obs.Date", "year", "month",  "week", "yday")] <- dat.veg[,c("Plot", "Subplot", "Genus", "Species", "Obs.Date", "year", "month", "week", "yday")]
summary(veg.long)

# Aggregating to species in the plot level first; this will get us the number of plots witha  species in a given phenophase for each plot
pheno.spp <- aggregate(Pheno.Status ~ Phenophase + Plot + Subplot + Obs.Date + year + month + yday + Genus + Species, data=veg.long[veg.long$Pheno.Status,], FUN=length)

pheno.spp.plot <- aggregate(Subplot ~ Phenophase + Plot + Obs.Date + year + month + yday + Genus + Species, data=pheno.spp, FUN=length)

pheno.spp.plot.mo <- aggregate(Subplot ~ Phenophase + Plot + year + month+ Genus + Species, data=pheno.spp, FUN=length)


ggplot(data=pheno.spp.plot[,]) +
  facet_grid(year~Plot) +
  geom_boxplot(aes(x=yday, fill=Phenophase), position="dodge") #+
  # scale_fill_manual(palette = "Dark2") +
  # scale_color_manual(palette = "Dark2")

ggplot(data=pheno.spp.plot.mo[,]) +
  facet_grid(year~Plot) +
  geom_histogram(aes(x=month, fill=Phenophase), position="dodge", binwidth=1) #+

# ggplot(data=veg.long) +
#   facet_grid(Plot~Phenophase) +
#   geom_boxplot(aes(x=yday, y=as.factor(year), fill=Plot))
# 
# ggplot(data=veg.long) +
#   facet_grid(Phenophase~.) +
#   geom_boxplot(aes(x=yday, y=Plot, fill=Plot))
# 
# ggplot(data=veg.long) +
#   facet_grid(Phenophase~Plot) +
#   geom_boxplot(aes(x=yday, y=as.factor(year), fill=Plot))
# 
# ggplot(data=veg.long) +
#   facet_grid(Phenophase~.) +
#   geom_boxplot(aes(x=yday, y=as.factor(year), fill=Plot))
# 
# ggplot(data=veg.long) +
#   facet_grid(Phenophase~.) +
#   geom_boxplot(aes(x=yday, y=Plot, group=as.factor(year)))
# 
# 
# pheno.subplot <- aggregate(Pheno.Status ~ Phenophase + Plot + Subplot + year + month, data=pheno.spp, FUN=length)
# 
# ggplot(data=pheno.subplot) +
#   facet_grid(Phenophase~year) +
#   geom_boxplot(aes(x=month, y=Subplot, fill=Plot))
# 
# 
# 
