#Script for summarizing leaf litter data. Largely cannibalized from Bethany
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lubridate)

path.l <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Leaf_litter_data/"

#grabbing the file from google drive and making it a workable data frame
#litter.df <- gs4_find("Leaf_Litter_Data")
#dat.lit <- data.frame(sheets_read(litter.df, range='raw_data'))

dat.lit <- read.csv("../GSHEET.csv")

setwd(path.l)

dat.lit$taxon <- paste0(substr(dat.lit$genus, 1, 3), ".", " ", dat.lit$species)
dat.lit <- dat.lit[dat.lit$taxon != ". ",]

dat.lit$taxon <- ifelse(grepl("unknown", dat.lit$taxon, fixed = TRUE), "unknown", dat.lit$taxon )
#adding a sqaured mass column and month column for different explorations
dat.lit <- dat.lit %>% mutate(mass.sqrt = sqrt(mass_g),
                              month = format(date_collection, format="%m/%d"))

#removing NA values created by fact that volunteers record plot and date in advance of acquiring data
dat.lit <- dat.lit[!is.na(dat.lit$trap_ID),]

#--------------------------------------------------#
#orders by date collection
#working with just leaf tissue
dat.leaf <- dat.lit[dat.lit$tissue=="leaf",]
dat.leaf <- dat.leaf[!is.na(dat.leaf$trap_ID),]
dat.leaf$date_collection <- as.Date(dat.leaf$date_collection, "%m/%d/%Y")
dat.leaf <- dat.leaf[order(dat.leaf$date_collection),]
dat.leafdate <- aggregate(mass_g~date_collection+plot+taxon, data=dat.leaf, mean)



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
dat.leafdate$mid_date <- (as.Date(dat.leafdate$date_collection) - (dat.leafdate$date_comp/2))

#removes unknown leaf litter and general "Quercus" id's. Optional part to be run
#dat.leafdate <- select(filter(dat.leafdate, taxon!= "Quercus"),c(date_collection, taxon, mass_g, date_comp, mass_per_day, mid_date))
dat.leafdate <- dat.leafdate[dat.leafdate$taxon != "unknown",]

#-------------------------------------------------------#
#Visualizations
#-------------------------------------------------------#

png("figures/Square_root_of_mass_by_species")
ggboxplot(dat.leaf, x = "taxon", y = "mass.sqrt", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Square root of Mass (g)", xlab = "Taxon", 
          title = "(Raw) Sqaure root of Mass of Leaf Tissue",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "fixed")
dev.off()

png("figures/Square_root_of_mass_by_plot")
ggboxplot(dat.leaf, x = "plot", y = "mass.sqrt", 
          color = "plot",
          ylab = "Square root of Mass (g)", xlab = "Plot", 
          title = "(Raw) Sqaure root of Mass of Leaf Tissue",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))
dev.off()

dat.leafdate$month <- month.abb[month(dat.leafdate$date_collection)]

dat.leafdate$month <- factor(dat.leafdate$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

png("figures/mass_per_day by month")
ggplot(dat.leafdate)+
  #facet_wrap(~plot)+
  geom_boxplot(aes(x=month, y = mass_per_day, color = plot))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mass/day by month")
#scale_x_date(date_breaks="1 month", date_labels = "%b-%y")
dev.off()


ggplot(dat.leafdate, aes(x=mid_date, y=taxon, fill=mass_per_day, width=date_comp))+
  geom_tile(color='white', aes(color=mid_date))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mass/day over time periods by species")+
  scale_x_date(date_breaks="1 month", date_labels = "%b-%y")

ggplot(dat.leafdate)+
  facet_wrap(~plot)+
  geom_line(aes(x=date_collection, y = mass_per_day, color = taxon))+
  geom_point(aes(x=date_collection, y = mass_per_day, color = taxon))+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mass/day over time periods by species")+
  scale_x_date(date_breaks="1 month", date_labels = "%b-%y")


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

#bar chart of frequency of taxa at plot
ggplot(dat.lit, aes(plot))+
  geom_bar(aes(fill=taxon), width=0.5)


#boxplot to visualize differences
ggboxplot(dat.lit[dat.lit$taxon != "unknown",], x = "taxon", y = "mass_g", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Mass (g)", xlab = "Plot", 
          title = "(Raw) Total Mass of All Tissue Sep2018-Aug2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "fixed")

#Plot for seeing mass by genus per plot
ggplot(dat.lit, aes(x=taxon, y=mass.sqrt))+
  facet_wrap(~plot)+
  geom_jitter(aes(color=taxon))+
  ggtitle("Squared mass of taxa per plot")+
  theme(axis.text.x = element_text(size=8, angle=70, vjust=0.6))

#-------------------------------#
#Graphs that use a mean value
#conversion for tile based on mean
dat.mean <- aggregate(mass_g~date_collection+taxon+plot+tissue, data=dat.lit, mean)
dat.mean <- dat.mean %>% mutate(mass_mean.sqrt = sqrt(mass_g),
                                month = format(date_collection, format="%m/%d"))
colnames(dat.mean)[colnames(dat.mean)=='mass_g'] <- 'mass_mean'

#------------------------------#
#data frame where trap_id is excluded and mass is summed by date, taxon, plot, and tissue
dat.sum <- aggregate(mass_g~date_collection+genus+species+taxon+plot+tissue, data=dat.lit, sum)
dat.sum <- dat.sum %>% mutate(mass_sum.sqrt = sqrt(mass_g),
                              month = format(date_collection, format="%m/%d"))
colnames(dat.sum)[colnames(dat.sum)=='mass_g'] <- 'mass_sum'

#----------------------------#

dat.leafmean <- aggregate(mass_g~date_collection+taxon+plot, data=dat.leaf, mean)
dat.leafmean <- dat.leafmean %>% mutate(mass_mean.sqrt = sqrt(mass_g),
                                      month = format(date_collection, format="%m/%d"))

dat.leafmass <- aggregate(mass_g~taxon+plot, data=dat.leaf, mean)
dat.leafmass$mass_z <- round((dat.leafmass$mass_g-mean(dat.leafmass$mass_g))/sd(dat.leafmass$mass_g),3)


ggplot(dat.leaf, aes(x=taxon, y=mass_g))+
  facet_wrap(~plot, scales = "fixed")+
  geom_bar(stat="identity", aes(color=taxon))+
  theme(axis.text.x= element_text(angle=75, vjust=0.6))

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


#Getting rid of huge outlier of heavy chunk of ash bark
#library(data.table)
#outlierReplace = function(dat.lit, cols, rows, newValue = NA) {
#  if (any(rows)) {
#    set(dat.lit, rows, cols, newValue)
#  }
#}
#outlierReplace(dat.lit, "mass_g", which(dat.lit$mass_g > 50), NA)

#Mean, SD, and ANOVA (will need to change ANOVA) for mass across plots
tapply(dat.lit$mass_g, dat.lit$plot, mean)
tapply(dat.lit$mass_g, dat.lit$plot, sd)
res.aov <- aov(mass_g ~ plot, data = dat.lit)
summary(res.aov)
