#Script for summarizing leaf litter data. Largely cannibalized from Bethany
library(googlesheets)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lubridate)

#grabbing the file from google drive and making it a workable data frame
litter.df <- gs_title("Leaf_Litter_Data")
dat.lit <- data.frame(gs_read(litter.df, ws="raw_data"))

#adding a sqaured mass column and month column for different explorations
dat.lit <- dat.lit %>% mutate(mass.sqrt = sqrt(mass_g),
                              month = format(date_collection, format="%m/%d"))

#removing NA values created by fact that volunteers record plot and date in advance of acquiring data
dat.lit <- dat.lit[!is.na(dat.lit$trap_ID),]

#Getting rid of huge outlier of heavy chunk of ash bark
library(data.table)
outlierReplace = function(dat.lit, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dat.lit, rows, cols, newValue)
  }
}
outlierReplace(dat.lit, "mass_g", which(dat.lit$mass_g > 50), NA)

#Mean, SD, and ANOVA (will need to change ANOVA) for mass across plots
tapply(dat.lit$mass_g, dat.lit$plot, mean)
tapply(dat.lit$mass_g, dat.lit$plot, sd)
res.aov <- aov(mass_g ~ plot, data = dat.lit)
summary(res.aov)

#--------------------------#
#pie table to visualize composition. Probabaly to be removed as it isnt useful
pie <- ggplot(dat.lit, aes(x="", fill = factor(taxon)))+
  geom_bar(width = 1)+
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust=0.5))+
  labs(fill="class", x=NULL,y=NULL)

pie + coord_polar(theta = "y", start=0)

#bar graph for frequency of taxon
freqtable <- table(dat.lit$taxon)
dat.freq <- as.data.frame.table(freqtable)
dat.freq <- dat.freq[order(dat.freq$Freq),]
dat.freq$Var1 <- factor(dat.freq$Var1, levels = dat.freq$Var1)

#ordered bar chart for frequency of taxa
ggplot(dat.freq, aes(Var1, Freq))+
  geom_bar(stat="identity", width = 0.5, , fill = "tomato2")+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))

#bar chart of frequency of tissue type by taxa
ggplot(dat.lit, aes(taxon))+
  geom_bar(aes(fill=tissue), width=0.5)+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))

#bar chart of frequency of taxa at plot
ggplot(dat.lit, aes(plot))+
  geom_bar(aes(fill=taxon), width=0.5)


#boxplot to visualize differences
ggboxplot(dat.lit, x = "taxon", y = "mass_g", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Mass (g)", xlab = "Plot", 
          title = "(Raw) Total Mass of All Tissue Sep2018-Aug2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "free")

#box plot with mass squared to look at outliers
ggboxplot(dat.lit, x = "taxon", y = "mass.sqrt", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Sqaure root of Mass (g)", xlab = "Plot", 
          title = "(Raw) Sqaure root of Mass of All Tissue Sep2018-Aug2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "fixed")


#Plot for mass of tissue type in each plot by taxon
ggplot(dat.lit, aes(x=plot, y=tissue, alpha=mass_g))+
  facet_wrap(~taxon, scales = "fixed")+
  geom_tile()+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mass_g of species by plot and tissue type")

#Plot for tissue presence of each taxon by plot
ggplot(dat.lit, aes(x=plot, y=tissue))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_count(aes(color=tissue))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("Tissue type for taxon by plot")

#Plot for the taxons tissue type by plot
ggplot(dat.lit, aes(x=taxon, y=tissue))+
  facet_wrap(~plot, scales = "free_y")+
  geom_jitter(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("Taxon tissue type by plot")

#Plots tissue by species over time for prevalence
ggplot(dat.lit, aes(x=month, y=tissue, alpha=mass_g))+
  facet_wrap(~taxon, scales = "fixed")+
  geom_tile()+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("tissue by species over time")

#----------------------------#
#working with fruits
dat.fruit <- dat.lit[!is.na(dat.lit$num_fruit),]

#removing instances of characters in num_fruit. Will need to discuss how to deal with otherwise
dat.fruit <- dat.fruit[!(dat.fruit$num_fruit == "multiple"),]

dat.fruit <- dat.fruit %>% transform(num_fruit = as.numeric(dat.fruit$num_fruit),
                                     num_mature_fruit = as.numeric(dat.fruit$num_mature_fruit))

#Plot of fruitmass of taxa by plot
ggplot(dat.fruit, aes(x=taxon, y=mass_g))+
  facet_wrap(~plot, scales = "fixed")+
  geom_bar(stat="identity")+
  theme(axis.text.x= element_text(angle=65, vjust=0.6))

ggplot(dat.fruit, aes(mass.sqrt))+
  geom_density(aes(fill=factor(genus)), alpha=0.8)


#Graphs themselves
ggplot(dat.fruit, aes(x=taxon, y=num_fruit))+
  facet_wrap(~plot)+
  geom_jitter(aes(color=taxon))

ggplot(dat.fruit, aes(taxon, mass.sqrt))+
  geom_violin()+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "free_y")

#----------------------------#
#Working with just oaks
dat.oaks <- subset(dat.lit, genus == "Quercus")

dat.oaksum <- aggregate(mass_g~date_collection+genus+species+taxon+plot+tissue, data=dat.oaks, sum)
dat.oaksum <- dat.oaksum %>% mutate(mass.sqrt = sqrt(mass_g),
                              month = format(date_collection, format="%m/%d"))

ggplot(dat.oaksum, aes(x=month, y=mass.sqrt))+
  facet_wrap(~taxon)+
  geom_jitter(aes(color=tissue))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))

#plot for presence of oak tissue types at different plots
ggplot(dat.oaksum, aes(x=plot, y=tissue, alpha=mass_g))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_tile()+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mass_g of oak species by plot and tissue type")

#Plot for the taxons tissue type by plot
ggplot(dat.oaks, aes(x=taxon, y=tissue))+
  facet_wrap(~plot, scales = "free_y")+
  geom_jitter(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("Oak tissue type by plot")

#Plots tissue by species over time for prevalence
ggplot(dat.oaks, aes(x=month, y=tissue, alpha=mass_g))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_tile()+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("Oak tissue by species over time")

#Plots tissue by species over time for frequency
ggplot(dat.oaks, aes(x=month, y=tissue))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_jitter(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("Oak tissue by species over time")

#------------------------------#
#data frame where trap_id is excluded and mass is summed by date, taxon, plot, and tissue
dat.sum <- aggregate(mass_g~date_collection+genus+species+taxon+plot+tissue, data=dat.lit, sum)
dat.sum <- dat.sum %>% mutate(mass.sqrt = sqrt(mass_g),
                              month = format(date_collection, format="%m/%d"))

#Plot for seeing change in mass totals over time
ggplot(dat.sum, aes(x=month, y=mass.sqrt))+
  facet_wrap(~tissue)+
  geom_point(aes(color=genus))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))

#Plot for seeing changes in mass totals per species over time
ggplot(dat.sum, aes(x=month, y=mass.sqrt))+
  facet_wrap(~taxon)+
  geom_jitter(aes(color=tissue))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))

#Plot for seeing mass by genus per plot
ggplot(dat.sum, aes(x=genus, y=mass.sqrt))+
  facet_wrap(~plot)+
  geom_point(aes(color=tissue))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))

#-------------#
#mass is summed by taxon exclusively for divergence calculation
dat.taxmass <- aggregate(mass_g~taxon, data=dat.lit, mean)
dat.taxmass$mass_z <- round((dat.taxmass$mass_g-mean(dat.taxmass$mass_g))/sd(dat.taxmass$mass_g),3)
dat.taxmass <- dat.taxmass[order(dat.taxmass$mass_z),]
dat.taxmass$taxon <- factor(dat.taxmass$taxon, levels = dat.taxmass$taxon)

ggplot(dat.taxmass, aes(x=taxon, y=mass_z, label=mass_z))+
  geom_point(stat='identity', fill="black", size=8)+
  geom_segment(aes(y=0, x = taxon, yend=mass_z, xend=taxon))+
  geom_text(color="white", size=2)+
  coord_flip()

#----------------------------------------#
#mass is summed by plot exclusively for divergence calculation
dat.plotmass <- aggregate(mass_g~plot, data=dat.lit, mean)
dat.plotmass$mass_z <- round((dat.plotmass$mass_g-mean(dat.plotmass$mass_g))/sd(dat.plotmass$mass_g),3)


ggplot(dat.plotmass, aes(x=plot, y=mass_z, label=mass_z))+
  geom_point(stat='identity', fill="black", size=8)+
  geom_segment(aes(y=0, x = plot, yend=mass_z, xend=plot))+
  geom_text(color="white", size=2)+
  coord_flip()

#------------------------------------#
#mass is summed by date exclusively for divergence calculation
dat.datemass <- aggregate(mass_g~date_collection, data=dat.lit, mean)
dat.datemass$mass_z <- round((dat.datemass$mass_g-mean(dat.datemass$mass_g))/sd(dat.datemass$mass_g),3)


ggplot(dat.datemass, aes(x=date_collection, y=mass_z, label=mass_z))+
  geom_point(stat='identity', fill="black", size=8)+
  geom_segment(aes(y=0, x = date_collection, yend=mass_z, xend=date_collection))+
  geom_text(color="white", size=2)


