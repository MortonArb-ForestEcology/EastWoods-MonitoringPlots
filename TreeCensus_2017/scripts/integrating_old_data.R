# --------------------------------------
# Summary of plot and tree data from the East Woods survey in 2011?
# Sierra Lopezalles, slopezal@caltech.edu
# June 27, 2017
# --------------------------------------

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

# Ordering canopy levels logically

tree.data$Canopy <- factor(tree.data$Canopy, levels=c("D", "CD", "I", "U", NA))

#Removing extra columns

heights2.data <- heights.data[c(3, 8)]

# Combining the tree height data into the general survey data

alltree.data <- merge(tree.data, heights2.data, by = "Tag")

# --------------------------------------

# Adding Basal Area to alltree data

alltree.data$BA <- pi*(alltree.data$DBH/2)^2 # Units = cm2

# --------------------------------------

# Filter plots in old data by those that were candidate plots for the 2017 monitoring

# Need to change naming of candidate plots

plots.data <- plots1.data

plots.data$CORNER <- paste0(substr(plots1.data$CORNER, 1, nchar(paste(plots1.data$CORNER))-4), substr(plots1.data$CORNER, nchar(paste(plots1.data$CORNER))-2,nchar(paste(plots1.data$CORNER))))

plot.list <- c("N115", "U134", "HH115", "B127")

oldtree.data.subset <- oldtree.data[oldtree.data$Plot %in% unique(plots.data$CORNER),]

oldtree.data.subset2 <- oldtree.data[oldtree.data$Plot %in% plot.list,]

# --------------------------------------

# Summing basal area by plot and then by each species in the plot

olddensity.data <- aggregate(as.numeric(oldtree.data[,"BA"]),
                          oldtree.data[,c("Plot", "Species")],
                          sum)
olddensity.data.subset <- aggregate(as.numeric(oldtree.data.subset[,"BA"]),
                                    oldtree.data.subset[,c("Plot", "Species")],
                                    sum)

density.data <- aggregate(as.numeric(alltree.data[,"BA"]),
                          alltree.data[,c("Plot", "Sp_code")],
                          sum)

names(olddensity.data)[3] <- "BA.tot"
names(olddensity.data.subset)[3] <- "BA.tot"
names(density.data)[3] <- "BA.tot"

# Actually converting to density
# Area for old plots was .025 ha, based on IMLS report sampling protocal
# Area for new plots is 400 m2, .04 ha

olddensity.data$Density <- olddensity.data$BA.tot/250 # Units m2/ha

olddensity.data.subset$Density <- olddensity.data.subset$BA.tot/250 # Units m2/ha
  
density.data$Density <- density.data$BA.tot/400 # Units m2/ha

# Summing number of stems per plot per species

olddensity2.data <- aggregate(oldtree.data[,"BA"],
                              oldtree.data[,c("Plot", "Species")],
                              length)

olddensity2.data.subset <- aggregate(oldtree.data.subset[,"BA"],
                                     oldtree.data.subset[,c("Plot", "Species")],
                                     length)

density2.data <- aggregate(tree.data["DBH"],
                          tree.data[,c("Plot", "Sp_code")],
                          length)

names(olddensity2.data)[3] <- "Stem.count"
names(olddensity2.data.subset)[3] <- "Stem.count"
names(density2.data)[3] <- "Stem.count"

olddensity.data.subset2 <- olddensity.data[olddensity.data$Plot %in% plot.list,]
olddensity2.data.subset2 <-olddensity2.data[olddensity2.data$Plot %in% plot.list,]

# --------------------------------------

# Combining the density data 

# Recoding plots to reflect the appropraite corner
density.data$Plot <- recode(density.data$Plot, " 'A1'='N115'; 'B5'='U134'; 'C6'='HH115'; 'D1'='B127' ")

density.data$Year <- 2017
olddensity.data.subset2$Year <- 2011

names(density.data)[2] <- "Species"

density.all <- rbind(density.data, olddensity.data.subset2)

# --------------------------------------
# Pretty graphs

library(ggplot2)

# Canopy Distributions based on plot

ggplot(tree.data) +
  geom_bar(aes(Canopy, fill=Sp_code)) +
  facet_wrap(~ Plot)

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

# Species density per plot from old and new data

ggplot(olddensity.data.subset2, aes(fill=Species)) +
  geom_col(aes(Species, Density)) +
  facet_wrap(~ Plot)

ggplot(density.data, aes(fill=Sp_code)) +
  geom_col(aes(Sp_code, Density)) +
  facet_wrap(~ Plot)

# Comparing species density in the four plots between years

ggplot(data=density.all, aes(fill=Species)) +
  geom_col(aes(Species, Density)) +
  facet_grid(Year ~ Plot, scales="free")

# Want to look at stand for 2011 rather than plot




