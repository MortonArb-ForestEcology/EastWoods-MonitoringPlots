#Script for summarizing leaf litter data. Largely cannibalized from Bethany
library(googlesheets)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lubridate)

#grabbing the file from google drive and making it a workable data frame
litter.df <- gs_title("Leaf_Litter_Data")
dat.lit <- data.frame(gs_read(litter.df, ws="raw_data"))

for(i in 1:ncol(dat.lit)){
  if(class(dat.lit[,i])=="character") dat.lit[,i] <- as.factor(dat.lit[,i])
}

str(dat.lit)

#adding a sqaured mass column and month column for different explorations
dat.lit <- dat.lit %>% mutate(mass.sqrt = sqrt(mass_g),
                              month = format(date_collection, format="%m/%d"))
dat.lit$month <- as.Date(dat.lit$month, format="%m/%d")

#removing NA values created by fact that volunteers record plot and date in advance of acquiring data
dat.lit <- dat.lit[!is.na(dat.lit$trap_ID),]

#--------------------------#
ggboxplot(dat.lit, x = "plot", y = "mass_g", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Mass (g)", xlab = "Plot", 
          title = "(Raw) Total Mass of All Tissue Sep2018-Aug2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "free")

#box plot with mass squared to look at outliers


ggboxplot(dat.lit, x = "plot", y = "mass.sqrt", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Sqaure root of Mass (g)", xlab = "Plot", 
          title = "(Raw) Sqaure root of Mass of All Tissue Sep2018-Aug2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "free")

#Mean, SD, and ANOVA (will need to change ANOVA) for mass across plots
tapply(dat.lit$mass_g, dat.lit$plot, mean)
tapply(dat.lit$mass_g, dat.lit$plot, sd)
res.aov <- aov(mass_g ~ plot, data = dat.lit)
summary(res.aov)

#Getting rid of huge outlier of heavy chunk of ash bark
library(data.table)
outlierReplace = function(dat.lit, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dat.lit, rows, cols, newValue)
  }
}
outlierReplace(dat.lit, "mass_g", which(dat.lit$mass_g > 50), NA)

#Total mass of all tissue by facet by species

for(i in 1:ncol(dat.lit)){
  if(class(dat.lit[,i])=="factor") dat.lit[,i] <- as.character(dat.lit[,i])
}

dat.stack <- stack(dat.lit[,c("mass_g", "tissue", "mass.sqrt")])
names(dat.stack) <- c("values", "var")
dat.stack[,c("plot","taxon","date_collection", "month")] <- dat.lit[,c("plot","taxon","date_collection")]
summary(dat.stack)

ggplot(dat.stack, aes(x=plot, y = values))+
  facet_wrap(~var, scales = "free_y")+
  geom_jitter(aes(color=taxon))

#Plot for tissue presence of each taxon by plot
ggplot(dat.lit, aes(x=plot, y=tissue))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_jitter(aes(color=tissue))+
  theme(axis.text.x = element_text(size=8, angle=60))+
  ggtitle("Tissue type for taxon by plot")

#Plot for the taxons tissue type by plot
ggplot(dat.lit, aes(x=taxon, y=tissue))+
  facet_wrap(~plot, scales = "free_y")+
  geom_jitter(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60))+
  ggtitle("Taxon tissue type by plot")

#Plots tissue by species over time for prevalence
ggplot(dat.lit, aes(x=month, y=tissue))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_point(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60))+
  ggtitle("Tissue by species over time")

#----------------------------#
#working with fruits
dat.fruit <- dat.lit[!is.na(dat.lit$num_fruit),]

#removing instances of characters in num_fruit. Will need to discuss how to deal with otherwise
dat.fruit <- dat.fruit[!(dat.fruit$num_fruit == "multiple"),]

dat.fruit <- dat.fruit %>% transform(num_fruit = as.numeric(dat.fruit$num_fruit),
                                     num_mature_fruit = as.numeric(dat.fruit$num_mature_fruit))

#Graphs themselves
ggplot(dat.fruit, aes(x=genus, y=num_fruit))+
  geom_jitter(aes(color=plot))

ggboxplot(dat.fruit, x = "month", y = "num_fruit", 
          color = "plot",
          facet.by = "taxon",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~taxon, scales = "free")

#----------------------------#
#Working with just oaks
dat.oaks <- subset(dat.lit, genus == "Quercus")

dat.oakstack <- stack(dat.oaks[,c("mass_g", "tissue", "mass.sqrt")])
names(dat.oakstack) <- c("values", "var")
dat.oakstack[,c("plot","taxon","date_collection", "month")] <- dat.oaks[,c("plot","taxon","date_collection", "month")]
summary(dat.oakstack)


#Plot for the taxons tissue type by plot
ggplot(dat.oaks, aes(x=taxon, y=tissue))+
  facet_wrap(~plot, scales = "free_y")+
  geom_jitter(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60))+
  ggtitle("Oak tissue type by plot")

#Plots tissue by species over time for prevalence
ggplot(dat.oaks, aes(x=month, y=tissue))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_point(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60))+
  ggtitle("Oak tissue by species over time")

#Plots tissue by species over time for frequency
ggplot(dat.oaks, aes(x=month, y=tissue))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_jitter(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60))+
  ggtitle("Oak tissue by species over time")


        
