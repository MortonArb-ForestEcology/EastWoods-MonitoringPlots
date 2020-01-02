#Script for summarizing leaf litter data. Largely cannibalized from Bethany
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lubridate)

path.l <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Leaf_litter_data/"

setwd(path.l)

#grabbing the file from google drive and making it a workable data frame
litter.df <- sheets_find("Leaf_Litter_Data")
dat.lit <- data.frame(sheets_read(litter.df, range='raw_data'))

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
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("Frequency of taxa across plots")

#bar chart of frequency of tissue type by taxa
ggplot(dat.lit, aes(taxon))+
  geom_bar(aes(fill=tissue), width=0.5)+
  theme(axis.text.x = element_text(size=8, angle=75, vjust=0.6))+
  ggtitle("Freqeuncy of taxa filled by tissue type")

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
  facet_wrap(~plot, scales = "fixed")

#box plot with mass squared to look at outliers
ggboxplot(dat.lit, x = "taxon", y = "mass.sqrt", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Sqaure root of Mass (g)", xlab = "Plot", 
          title = "(Raw) Sqaure root of Mass of All Tissue Sep2018-Sep2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "fixed")

#Plot for seeing mass by genus per plot
ggplot(dat.lit, aes(x=taxon, y=mass.sqrt))+
  facet_wrap(~plot)+
  geom_jitter(aes(color=taxon))+
  ggtitle("Squared mass of taxa per plot")+
  theme(axis.text.x = element_text(size=8, angle=70, vjust=0.6))

#Plot for mass of tissue type in each plot by taxon
ggplot(dat.lit, aes(x=plot, y=tissue, alpha=mass_g))+
  facet_wrap(~taxon, scales = "fixed")+
  geom_tile(aes(color=tissue))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mass_g of species by plot and tissue type")

#Plot for tissue presence of each taxon by plot
ggplot(dat.lit, aes(x=plot, y=tissue))+
  facet_wrap(~taxon, scales = "fixed")+
  geom_count(aes(color=tissue))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("Tissue type for taxon by plot")

#Count Plot for the taxons tissue type by plot
ggplot(dat.lit, aes(x=taxon, y=tissue))+
  facet_wrap(~plot, scales = "fixed")+
  geom_count(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=75, vjust=0.6))+
  ggtitle("Taxon tissue type by plot")

#Tile Plot for the taxons tissue type by plot
ggplot(dat.lit, aes(x=taxon, y=tissue, alpha=mass_g))+
  facet_wrap(~plot, scales = "fixed")+
  geom_tile(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=75, vjust=0.6))+
  ggtitle("mass_g of Taxa by tissue type and plot")

#Tile Plot tissue by species over time for prevalence
ggplot(dat.lit, aes(x=month, y=taxon, alpha=mass_g))+
  geom_tile(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mass_g of tissue by species over time")

#-------------------------------#
#Graphs that use a mean value
#conversion for tile based on mean
dat.mean <- aggregate(mass_g~date_collection+taxon+plot+tissue, data=dat.lit, mean)
dat.mean <- dat.mean %>% mutate(mass_mean.sqrt = sqrt(mass_g),
                                month = format(date_collection, format="%m/%d"))
colnames(dat.mean)[colnames(dat.mean)=='mass_g'] <- 'mass_mean'

#Tile Plot for the mean of taxons by tissue and plot
ggplot(dat.mean, aes(x=taxon, y=tissue, alpha=mass_mean))+
  facet_wrap(~plot, scales = "fixed")+
  geom_tile(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=75, vjust=0.6))+
  ggtitle("Mean taxon tissue type by plot")


#Mean tile plots tissue by species over time for prevalence
ggplot(dat.mean, aes(x=month, y=tissue, alpha=mass_mean))+
  facet_wrap(~taxon, scales = "fixed")+
  geom_tile()+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mean tissue by species over time")

#------------------------------#
#data frame where trap_id is excluded and mass is summed by date, taxon, plot, and tissue
dat.sum <- aggregate(mass_g~date_collection+genus+species+taxon+plot+tissue, data=dat.lit, sum)
dat.sum <- dat.sum %>% mutate(mass_sum.sqrt = sqrt(mass_g),
                              month = format(date_collection, format="%m/%d"))
colnames(dat.sum)[colnames(dat.sum)=='mass_g'] <- 'mass_sum'

#Plot for seeing change in mass totals over time
ggplot(dat.sum, aes(x=month, y=mass_sum.sqrt))+
  facet_wrap(~tissue)+
  geom_point(aes(color=taxon))+
  ggtitle("Squared mass of tissue over time")+
  xlab("date collected")+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))

#Plot for seeing changes in mass totals per species over time
ggplot(dat.sum, aes(x=month, y=mass_sum.sqrt))+
  facet_wrap(~taxon)+
  geom_point(aes(color=tissue))+
  ggtitle("Sqaured mass of taxa over time")+
  xlab("date collected")+
  theme(axis.text.x = element_text(size=8, angle=70, vjust=0.6))

#----------------------------#
#working with fruits
dat.fruit <- dat.lit[!is.na(dat.lit$num_fruit),]

#Converting instances of characters into an NA
dat.fruit[,9][dat.fruit[, 9] =="multiple"] <- NA

dat.fruit <- dat.fruit %>% transform(num_fruit = as.numeric(dat.fruit$num_fruit),
                                     num_mature_fruit = as.numeric(dat.fruit$num_mature_fruit))

#rows=1
#for(i in rows:nrow(dat.fruit)){
#  if(!is.na(dat.fruit[rows, 'num_fruit'])){
#    dat.fruit[rows, 'num_immature_fruit'] = ifelse(is.na(dat.fruit[rows, 'num_immature_fruit']),0,dat.fruit[rows, 'num_immature_fruit'])
#    dat.fruit[rows, 'num_mature_fruit'] = ifelse(is.na(dat.fruit[rows, 'num_mature_fruit']),0,dat.fruit[rows, 'num_mature_fruit'])
#  }
#  rows=rows+1
#}

#Plot of fruitmass of taxa by plot
ggplot(dat.fruit, aes(x=taxon, y=mass_g))+
  facet_wrap(~plot, scales = "fixed")+
  geom_bar(stat="identity")+
  theme(axis.text.x= element_text(angle=75, vjust=0.6))

ggplot(dat.fruit, aes(mass.sqrt))+
  geom_density(aes(fill=factor(genus)), alpha=0.8)

#Graphs 
ggplot(dat.fruit, aes(x=taxon, y=num_fruit))+
  facet_wrap(~plot)+
  geom_count(aes(color=taxon))+
  theme(axis.text.x= element_text(angle=75, vjust=0.6))

ggplot(dat.fruit, aes(taxon, mass.sqrt))+
  geom_violin()+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "free_y")

#----------------------------#
#working with just leaf tissue
dat.leaf <- dat.lit[dat.lit$tissue=="leaf",]
dat.leaf <- dat.leaf[!is.na(dat.leaf$trap_ID),]

dat.leafmean <- aggregate(mass_g~date_collection+taxon+plot, data=dat.leaf, mean)
dat.leafmean <- dat.leafmean %>% mutate(mass_mean.sqrt = sqrt(mass_g),
                                      month = format(date_collection, format="%m/%d"))

dat.leafmass <- aggregate(mass_g~taxon+plot, data=dat.leaf, mean)
dat.leafmass$mass_z <- round((dat.leafmass$mass_g-mean(dat.leafmass$mass_g))/sd(dat.leafmass$mass_g),3)


ggplot(dat.leaf, aes(x=taxon, y=mass_g))+
  facet_wrap(~plot, scales = "fixed")+
  geom_bar(stat="identity", aes(color=taxon))+
  theme(axis.text.x= element_text(angle=75, vjust=0.6))

ggplot(dat.leaf, aes(x=month, y=mass_g))+
  facet_wrap(~plot, scales = "fixed")+
  geom_bar(stat="identity", aes(color=taxon))+
  xlab("date collected")+
  ggtitle("leaf mass of date collected by plot")+
  theme(axis.text.x= element_text(angle=60, vjust=0.6))

ggboxplot(dat.leaf, x = "taxon", y = "mass.sqrt", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Sqaure root of Mass (g)", xlab = "Plot", 
          title = "(Raw) Sqaure root of Mass of Leaf Tissue Sep2018-Sep2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "fixed")

ggplot(dat.leafmass, aes(x=taxon, y=mass_z, label=mass_z))+
  facet_wrap(~plot)+
  geom_point(stat='identity', aes(color=taxon), size=8)+
  geom_segment(aes(y=0, x = taxon, yend=mass_z, xend=taxon))+
  geom_text(color="white", size=2)+
  ggtitle("Divergence of mean leaf mass by taxa per plot")+
  coord_flip()


#----------------------------#
#Working with just oaks
dat.oaks <- subset(dat.lit, genus == "Quercus")

dat.oaksum <- aggregate(mass_g~date_collection+genus+species+taxon+plot+tissue, data=dat.oaks, sum)
dat.oaksum <- dat.oaksum %>% mutate(mass.sqrt = sqrt(mass_g),
                              month = format(date_collection, format="%m/%d"))

dat.oakmean <- aggregate(mass_g~date_collection+taxon+tissue, data=dat.oaks, mean)
dat.oakmean <- dat.oakmean %>% mutate(mass_mean.sqrt = sqrt(mass_g),
                                month = format(date_collection, format="%m/%d"))
colnames(dat.oakmean)[colnames(dat.oakmean)=='mass_g'] <- 'mass_mean'

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
ggplot(dat.oaks, aes(x=taxon, y=tissue, alpha=mass_g))+
  facet_wrap(~plot, scales = "fixed")+
  geom_tile(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=75, vjust=0.6))+
  ggtitle("Oak tissue type by plot")

#Plots tissue by species over time for prevalence
ggplot(dat.oaks, aes(x=month, y=tissue, alpha=mass_g))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_tile(aes(color=tissue))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("Oak tissue by species over time")

#Plots tissue by species over time for frequency
ggplot(dat.oaks, aes(x=month, y=tissue))+
  facet_wrap(~taxon, scales = "free_y")+
  geom_count(aes(color=taxon))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("Oak tissue by species over time")+
  xlab("date collected")

#-----------------------------------#
#working with Trap_id for the plots
dat.trapmean <- aggregate(mass_g~trap_ID+plot, data=dat.lit, mean)
dat.trapmean <- dat.trapmean %>% mutate(mass_mean.sqrt = sqrt(mass_g))
dat.trapmean$mass_z <- round((dat.trapmean$mass_g-mean(dat.trapmean$mass_g))/sd(dat.trapmean$mass_g),3)

ggplot(dat.trapmean, aes(x=trap_ID, y=mass_z, label=mass_z))+
  facet_wrap(~plot, scales = "free_x")+
  geom_point(stat='identity', fill="black", size=8)+
  geom_segment(aes(y=0, x = trap_ID, yend=mass_z, xend=trap_ID))+
  geom_text(color="white", size=2)+
  ggtitle("Divergence of mean mass per trap per plot")

dat.traptax <- aggregate(mass_g~trap_ID+plot+taxon, data=dat.lit, mean)

ggplot(dat.traptax, aes(x=trap_ID))+
  facet_wrap(~plot, scales="free_x")+
  geom_bar(aes(fill=taxon))



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
  ggtitle("Divergence in mean mass per taxa")+
  coord_flip()

#----------------------------------------#
#mass is summed by plot exclusively for divergence calculation
dat.plotmass <- aggregate(mass_g~plot, data=dat.lit, mean)
dat.plotmass$mass_z <- round((dat.plotmass$mass_g-mean(dat.plotmass$mass_g))/sd(dat.plotmass$mass_g),3)


ggplot(dat.plotmass, aes(x=plot, y=mass_z, label=mass_z))+
  geom_point(stat='identity', fill="black", size=8)+
  geom_segment(aes(y=0, x = plot, yend=mass_z, xend=plot))+
  geom_text(color="white", size=2)+
  ggtitle("Divergence of mean mass per plot")
  coord_flip()

#------------------------------------#
#mass is summed by date exclusively for divergence calculation
dat.datemass <- aggregate(mass_g~date_collection+month, data=dat.lit, mean)
dat.datemass$mass_z <- round((dat.datemass$mass_g-mean(dat.datemass$mass_g))/sd(dat.datemass$mass_g),3)


ggplot(dat.datemass, aes(x=date_collection, y=mass_z, label=mass_z))+
  geom_point(stat='identity', fill="black", size=8)+
  geom_segment(aes(y=0, x = date_collection, yend=mass_z, xend=date_collection))+
  ggtitle("Divergence of mean mass per date collected")+
  geom_text(color="white", size=2)

ggplot(dat.datemass, aes(x=month, y=mass_z, label=mass_z))+
  geom_point(stat='identity', fill="black", size=8)+
  geom_segment(aes(y=0, x = month, yend=mass_z, xend=month))+
  geom_text(color="white", size=2)

#--------------------------------------------------#
#orders by date collection
dat.leaf <- dat.leaf[order(dat.leaf$date_collection),]
dat.leafdate <- aggregate(mass_g~date_collection+taxon, data=dat.leaf, mean)

dat.leafdate$date_comp <- 0
#loop to create value of the differences between dates
for(i in 1:nrow(dat.leafdate)){
  if(dat.leafdate[i, "taxon"]!= "unknown"){
    if(dat.leafdate[i,"taxon"] == dat.leafdate[i+1, "taxon"]){
      n <- (dat.leafdate[i+1,"date_collection"]-dat.leafdate[i,"date_collection"])
    } else{n=0}
    dat.leafdate$date_comp[i+1] <- n
  } else {break}
}

#removing values casued by first measurement
dat.leafdate <- dat.leafdate %>% transform(date_comp = ifelse(date_comp==0, NA, date_comp))
dat.leafdate$mass_per_day <- 0

for(i in 1:nrow(dat.leafdate)){
  if(!is.na(dat.leafdate[i, "date_comp"])){
  n <- (dat.leafdate[i,"mass_g"]/dat.leafdate[i,"date_comp"])
  }else{n=NA}
  dat.leafdate$mass_per_day[i] <- n
}

#determining the midpoint between measured dates so that the width can be centered here
dat.leafdate$mid_date <- (dat.leafdate$date_collection - (dat.leafdate$date_comp/2))


View(dat.leafdate)
str(dat.leafdate)


dat.leafdate <- select(filter(dat.leafdate, taxon!= "unknown"),c(date_collection, taxon, mass_g, date_comp, mass_per_day, mid_date))
dat.leafdate <- select(filter(dat.leafdate, taxon!= "Quercus"),c(date_collection, taxon, mass_g, date_comp, mass_per_day, mid_date))

ggplot(dat.leafdate, aes(x=mid_date, y=taxon, fill=mass_per_day, width=date_comp))+
  geom_tile(color='white', aes(color=mid_date))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mass/day over time periods by species")+
  scale_x_date(date_breaks="1 month", date_labels = "%b-%y")
  

