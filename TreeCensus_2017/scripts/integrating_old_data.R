# --------------------------------------
# Summary of plot and tree data from the East Woods survey in 2011?
# Sierra Lopezalles, slopezal@caltech.edu
# June 27, 2017
# --------------------------------------
# --------------------------------------

# Setting a working directory

setwd("~/GitHub/EastWoods-MonitoringPlots/TreeCensus_2017/")

# Data taken from Fahey_shres EW Survey data in the East Woods folder.
# Tree data is from the tree datasheet page
# Plot data is from the plot datasheet page

oldplot.data <- read.csv("data/Old EW Survey Data/Old EW Plot Data.csv", na.strings="")

oldtree.data <- read.csv("data/Old EW Survey Data/Old EW Tree Data.csv", na.strings="")

# New data taken from 2017 plot monitoring
# General survey data is seperate from tree heights 

tree.data <-read.csv("data/TreeData-raw_data.csv", na.strings="")

heights.data <- read.csv("data/TreeHeights-raw_data.csv", na.strings="")

# Candidate plot data was taken from Christy's plot selection folder 

plots1.data <- read.csv("data/CandidatePlots.csv", na.strings="")

# --------------------------------------
# Data wrangling to make everything nice
# --------------------------------------

library(car)

# We don't want all the weird columns

oldplot.data <- oldplot.data[!is.na(oldplot.data[,1]),1:11]
names(oldplot.data) <- c("Plot", "Shrub.Cover", "Shrub.Ht", "Herb.Cover", "Ground.Cover", "X", "Y", "Management.Unit",
                         "Burn.Year", "Thin.Year", "Notes")

oldtree.data <- oldtree.data[!is.na(oldtree.data[,1]),1:6]
names(oldtree.data)[1] <- "Plot"

# Removing spaces in species codes

tree.data$Sp_code <- as.factor(substr(tree.data$Sp_code, 1, 4))
oldtree.data$Species <- as.factor(substr(oldtree.data$Species, 1, 4))

# Fixing a typo

oldtree.data$Species <- recode(oldtree.data$Species, " 'FR'='FRAM'; 'UL'='ULRU'; 'UL?'='ULRU'; 'QUEL'='QUAL'; 'ACNE' = 'ACNI' ")
                            
# Ordering canopy levels logically

tree.data$Canopy <- factor(tree.data$Canopy, levels=c("D", "CD", "I", "U", NA))

#Removing extra columns

heights2.data <- heights.data[c(3, 8)]

# Combining the tree height data into the general survey data

alltree.data <- merge(tree.data, heights2.data, by = "Tag")

# --------------------------------------
# Begining calculations
# --------------------------------------

# Adding Basal Area to alltree data

alltree.data$BA <- pi*(alltree.data$DBH/200)^2 # Units = m2

# --------------------------------------

# Filter plots in old data by those that were candidate plots for the 2017 monitoring

plots.data <- plots1.data

plots.data$CORNER <- paste0(substr(plots1.data$CORNER, 1, nchar(paste(plots1.data$CORNER))-4), substr(plots1.data$CORNER, nchar(paste(plots1.data$CORNER))-2,nchar(paste(plots1.data$CORNER))))

plot.list <- c("N115", "U134", "HH115", "B127")

oldtree.data.subset <- oldtree.data[oldtree.data$Plot %in% unique(plots.data$CORNER),]

oldtree.data.subset2 <- oldtree.data[oldtree.data$Plot %in% plot.list,]

# Creating stand data

stand.data <- plots.data[3:4]
names(stand.data)[1] <- "Plot"
names(stand.data)[2] <- "Stand"

# --------------------------------------

# Summing basal area by plot and then by each species in the plot

oldba.data <- aggregate(as.numeric(as.character(oldtree.data[,"BA"])),
                          oldtree.data[,c("Plot", "Species")],
                          sum)
oldba.data.subset <- aggregate(as.numeric(as.character(oldtree.data.subset[,"BA"])),
                                    oldtree.data.subset[,c("Plot", "Species")],
                                    sum)

newba.data <- aggregate(as.numeric(as.character(alltree.data[,"BA"])),
                          alltree.data[,c("Plot", "Sp_code")],
                          sum)

names(oldba.data)[3] <- "BA.tot"
names(oldba.data.subset)[3] <- "BA.tot"
names(newba.data)[3] <- "BA.tot"

# Actually converting to density
# Area for old plots was .025 ha, based on IMLS report sampling protocal
# Area for new plots is 400 m2, .04 ha

oldba.data$Density <- oldba.data$BA.tot/250 # Units m2/ha

oldba.data.subset$Density <- oldba.data.subset$BA.tot/250 # Units m2/ha
  
newba.data$Density <- newba.data$BA.tot/400 # Units m2/ha

# Summing number of stems per plot per species

olddensity.data <- aggregate(oldtree.data[,"BA"],
                              oldtree.data[,c("Plot", "Species")],
                              length)

olddensity.data.subset <- aggregate(oldtree.data.subset[,"BA"],
                                     oldtree.data.subset[,c("Plot", "Species")],
                                     length)

newdensity.data <- aggregate(tree.data["DBH"],
                          tree.data[,c("Plot", "Sp_code")],
                          length)

names(olddensity.data)[3] <- "Stem.count"
names(olddensity.data.subset)[3] <- "Stem.count"
names(newdensity.data)[3] <- "Stem.count"

# -------------------------------------

# Subsetting the 4 2017 plots

oldba.data.subset2 <- oldba.data[oldba.data$Plot %in% plot.list,]
olddensity.data.subset2 <-olddensity.data[olddensity.data$Plot %in% plot.list,]

# --------------------------------------

# Recoding plots to reflect the appropraite corner

newba.data$Plot <- recode(newba.data$Plot, " 'A1'='N115'; 'B5'='U134'; 'C6'='HH115'; 'D1'='B127' ")
newdensity.data$Plot <- recode(newdensity.data$Plot, " 'A1'='N115'; 'B5'='U134'; 'C6'='HH115'; 'D1'='B127' ")


newba.data$Year <- 2017
oldba.data.subset2$Year <- 2011

names(newba.data)[2] <- "Species"
names(newdensity.data)[2] <- "Species"

# Adding stand letter

oldba.data.subset.stand <- merge(oldba.data.subset, stand.data, by = "Plot")
newba.data.stand <- merge(newba.data, stand.data, by = "Plot")

oldba.data.subset.stand$Year <- 2011

old.density <- merge(olddensity.data.subset, stand.data, by = "Plot")
new.density <- merge(newdensity.data, stand.data, by = "Plot")

old.density$Year <- 2011
new.density$Year <- 2017

# Combining the basal area data 

ba2.all <- rbind(newba.data, oldba.data.subset2)
ba.all <- rbind(newba.data.stand, oldba.data.subset.stand)

# Combining density data

density.all <- rbind(old.density, new.density)

# -------------------------------------

# Calculating things needed for error bars

ba.error2 <- aggregate(ba.all$Density,
                           ba.all[,c("Stand", "Species", "Year")],
                           mean)
names(ba.error2)[4] <- "Density"

ba.error3 <- aggregate(ba.all$Density,
                            ba.all[,c("Stand", "Species", "Year")],
                            sd)
names(ba.error3)[4] <- "St.dev"

ba.error <- merge(ba.error2, ba.error3)

ba.error$Ymin <- ba.error$Density - ba.error$St.dev
ba.error$Ymax <- ba.error$Density + ba.error$St.dev

# --------------------------------------
# Pretty graphs
# --------------------------------------

library(ggplot2)

colors <- c(ACSA = "#F8766D",
            FRAM = "#AEA200", 
            JUNI = "#FF63B6", 
            OSVI = "#00BD5C", 
            PRSE = "#B385FF", 
            QUAL = "#00BADE", 
            QUMA = "#00A6FF", 
            QURU = "#00C1A7", 
            TIAM = "#EF67EB", 
            ULRU = "lightpink2", 
            ACNI = "#53B400", 
            CACO = "#AEA200", 
            CR = "#DB8E00", 
            POTR = "lightseagreen", 
            UNKN = "lightpink4", 
            unkn = "lightpink4")

# Don't like

# Canopy Distributions based on plot

ggplot(tree.data) +
  geom_bar(aes(Canopy, fill=Sp_code)) +
  facet_wrap(~ Plot) +
  scale_fill_manual(values = colors)

# Species distribution based on plot

ggplot(tree.data) +
  geom_bar(aes(Sp_code, fill=Sp_code)) +
  facet_wrap(~ Plot) +
  scale_fill_manual(values = colors)

ggplot(tree.data) +
  geom_bar(aes(Plot, fill=Sp_code)) +
  scale_fill_manual(values = colors)

# Tree distribution based on plot

ggplot(alltree.data) +
  geom_point(aes(x=X, y=Y, color=Sp_code, size=DBH)) +
  facet_wrap(~ Plot)

ggplot(alltree.data) +
  geom_point(aes(x=X, y=Y, color=Sp_code, size=BA)) +
  facet_wrap(~ Plot) 

ggplot(alltree.data) +
  geom_point(aes(x=X, y=Y, color="blue", size=BA)) +
  geom_point(aes(x=X, y=Y, color="red", size=DBH)) +
  facet_wrap(~ Plot)

# Correlation between diameter and height

ggplot(alltree.data, aes(x=DBH, y=Height)) +
  geom_point(aes(color=Sp_code)) 

# Species basal area per plot from old and new data

ggplot(oldba.data.subset2, aes(fill=Species)) +
  geom_col(aes(Species, Density)) +
  facet_wrap(~ Plot) +
  scale_fill_manual(values = colors)

ggplot(newba.data, aes(fill=Species)) +
  geom_col(aes(Species, Density)) +
  facet_wrap(~ Plot) +
  scale_fill_manual(values = colors)

# --------------------------------------
# Checking that the 2017 plots are a good representation of each stand
# --------------------------------------

# Comparing species basal area in the four stands between years

ggplot(ba.all, aes(fill=Species)) +
  geom_col(aes(Species, Density)) +
  facet_grid(Year ~ Stand, scales="free") +
  scale_fill_manual(values = colors)

# Graphing error bars for the density in 2011 stands

ggplot(ba.error, aes(fill=Species)) +
  facet_grid(Year ~ Stand, scales="free_x") +
  geom_col(aes(Species, Density)) +
  geom_errorbar(aes(x=Species, ymin=Ymin, ymax=Ymax), na.rm = TRUE)

# Showing only top half of the error bars, looks better, less accurate

ggplot(ba.error, aes(fill=Species)) +
  geom_errorbar(aes(x=Species, ymin=Density, ymax=Ymax), na.rm = TRUE) +
  facet_grid(Year ~ Stand, scales="free_x") +
  geom_col(aes(Species, Density)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_manual(values = colors)

# Combined all oak species

ba.error.test <- ba.error
ba.error.test$Species <- as.factor(substr(ba.error.test$Species, 1, 2))

ggplot(ba.error.test, aes(fill=Species)) +
  facet_grid(Year ~ Stand, scales="free_x") +
  geom_col(aes(Species, Density)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Using a boxplot rather than error bars

ggplot(ba.all, aes(fill=Species)) +
  geom_boxplot(data=ba.all[ba.all$Year==2011,],aes(x=Species, y=Density)) +
  geom_point(data=ba.all[ba.all$Year==2017,],aes(x=Species, y=Density), color="yellow") +
  facet_grid(. ~ Stand, scales="free_x") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_manual(values = colors)

# Boxplot of stem counts in old plots by stand

ggplot(old.density, aes(fill=Species)) +
  geom_boxplot(aes(x=Species, y=Stem.count)) +
  facet_grid(. ~ Stand, scales="free_x") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Comparing stem count per plot per year with boxplots

ggplot(density.all, aes(fill=Species)) +
  geom_boxplot(data=density.all[density.all$Year==2011,],aes(x=Species, y=Stem.count, alpha = .2)) +
  geom_point(data=density.all[density.all$Year==2017,],aes(x=Species, y=Stem.count), color="black") +
  facet_grid(. ~ Stand, scales="free_x") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# --------------------------------------
# Finding more plots to collect more oak data
# --------------------------------------

# Using at oak basal area as a family rather than by species

oldba.sub <- oldba.data.subset.stand
oldba.sub$Species <- as.factor(substr(oldba.sub$Species, 1, 2))

oldba.oak <- aggregate(oldba.sub["Density"],
                       oldba.sub[, c("Species", "Plot", "Stand")],
                       sum)

old.density.sub <- old.density
old.density.sub$Species <- as.factor(substr(old.density.sub$Species, 1, 2))

old.density.oak <- aggregate(old.density.sub["Stem.count"],
                       old.density.sub[, c("Species", "Plot", "Stand")],
                       sum)

# --------------------------------------

# Stand A, Plot: O109, O121, R109; R113, N113,, O108, O105 (N115)

ggplot(oldba.oak, aes(fill=Species)) +
  geom_boxplot(data=oldba.oak[oldba.oak$Stand=="A",], aes(x=Species, y=Density, alpha = .5)) +
  geom_point(data=oldba.oak[oldba.oak$Stand=="A",], aes(x=Species, y=Density, color=Plot), position=position_jitter(width=.2, height=0)) +
  guides(fill=FALSE)

ggplot(old.density.oak, aes(fill=Species)) +
  geom_boxplot(data=old.density.oak[old.density.oak$Stand=="A",], aes(x=Species, y=Stem.count, alpha = .5)) +
  geom_point(data=old.density.oak[old.density.oak$Stand=="A",], aes(x=Species, y=Stem.count, color=Plot), position=position_jitter(width=.2, height=0)) +
  guides(fill=FALSE)

# --------------------------------------

# Stand B, Plot: M134, T132, O136 (U134)

ggplot(oldba.oak, aes(fill=Species)) +
  geom_boxplot(data=oldba.oak[oldba.oak$Stand=="B",], aes(x=Species, y=Density, alpha = .5)) +
  geom_point(data=oldba.oak[oldba.oak$Stand=="B",], aes(x=Species, y=Density, color=Plot), position=position_jitter(width=.2, height=0)) +
  guides(fill=FALSE)

ggplot(old.density.oak, aes(fill=Species)) +
  geom_boxplot(data=old.density.oak[old.density.oak$Stand=="B",], aes(x=Species, y=Stem.count, alpha = .5)) +
  geom_point(data=old.density.oak[old.density.oak$Stand=="B",], aes(x=Species, y=Stem.count, color=Plot), position=position_jitter(width=.2, height=0)) +
  guides(fill=FALSE)

# All have one oak, probably should core more plots

# --------------------------------------

# Stand C, Plot: HH112, MM114, LL114; SS113,, FF117, HH116, II118 (HH115)

ggplot(oldba.oak, aes(fill=Species)) +
  geom_boxplot(data=oldba.oak[oldba.oak$Stand=="C",], aes(x=Species, y=Density, alpha = .5)) +
  geom_point(data=oldba.oak[oldba.oak$Stand=="C",], aes(x=Species, y=Density, color=Plot), position=position_jitter(width=.2, height=0)) +
  guides(fill=FALSE)

ggplot(old.density.oak, aes(fill=Species)) +
  geom_boxplot(data=old.density.oak[old.density.oak$Stand=="C",], aes(x=Species, y=Stem.count, alpha = .5)) +
  geom_point(data=old.density.oak[old.density.oak$Stand=="C",], aes(x=Species, y=Stem.count, color=Plot), position=position_jitter(width=.2, height=0)) +
  guides(fill=FALSE)

# --------------------------------------

# Stand D, Plot: D123, A128, C123; C120,, F118 (B127)

ggplot(oldba.oak, aes(fill=Species)) +
  geom_boxplot(data=oldba.oak[oldba.oak$Stand=="D",], aes(x=Species, y=Density, alpha = .5)) +
  geom_point(data=oldba.oak[oldba.oak$Stand=="D",], aes(x=Species, y=Density, color=Plot), position=position_jitter(width=.2, height=0)) +
  guides(fill=FALSE)

ggplot(old.density.oak, aes(fill=Species)) +
  geom_boxplot(data=old.density.oak[old.density.oak$Stand=="D",], aes(x=Species, y=Stem.count, alpha = .5)) +
  geom_point(data=old.density.oak[old.density.oak$Stand=="D",], aes(x=Species, y=Stem.count, color=Plot), position=position_jitter(width=.2, height=0)) +
  guides(fill=FALSE)

# --------------------------------------

# For testing plot points

ggplot(oldba.oak, aes(fill=Species)) +
  geom_boxplot(data=oldba.oak[oldba.oak$Stand=="D",], aes(x=Species, y=Density, alpha = .5)) +
  geom_point(data=oldba.oak[oldba.oak$Plot=="D123",], aes(x=Species, y=Density, color=Plot), position=position_jitter(width=.2, height=0)) +
  guides(fill=FALSE)






# Choose plots that had average oak basal area for their stand
# What about higher stem density?

# Idenstify 2 or 3 more plots per stand
# how, number of oaks, which plots

# Choose colors




