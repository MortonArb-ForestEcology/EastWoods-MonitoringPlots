#Script for visualizing leaf litter data
library(ggplot2)
library(tidyr)
library(ggpubr)
library(lubridate)

path.l <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Leaf_litter_data/"

setwd(path.l)

#grabbing the file from google drive and making it a workable data frame
litter.df <- googlesheets4::sheets_find("Leaf_Litter_Data")
dat.lit <- data.frame(googlesheets4::sheets_read(litter.df, range='raw_data'))

#removing NA values created by fact that volunteers record plot and date in advance of acquiring data
dat.leaf <- dat.lit[dat.lit$tissue=="leaf",]
dat.leaf <- dat.leaf[!is.na(dat.leaf$trap_ID),]
dat.leaf <- subset(dat.leaf, select=-c(1,7,9:13,15:16,18))

dat.agg <- aggregate(mass_g~date_collection+taxon+plot+tissue+genus+species, data=dat.leaf, mean)

#reading CN output file
CN.dat <-readbulk::read_bulk(directory = "CN_Runs", extension = ".csv", header = FALSE, skip=1,)
CN.dat <- CN.dat[,1:4]
colnames(CN.dat) <- c("Name", "%N", "%C", "C.N")

CN.dat$Name <- as.character(CN.dat$Name)

#I was testing different methods here which is why they are differnet syntaxes
#Creating a new column with PlotID 
CN.dat$PlotID <- sapply(strsplit(CN.dat$Name, "-"), "[", 1)

#Creating a new column with Species
CN.dat$Taxon <- substr(CN.dat$Name, nchar(CN.dat$Name)-1, nchar(CN.dat$Name))

#Creating a new column with Date
CN.dat$Date <- substr(CN.dat$Name, 3, 10)
CN.dat$Date <- paste("20", CN.dat$Date, sep="")
CN.dat$Date <- as.Date(CN.dat$Date, "%Y-%m-%d")

#Renaming plots to match google sheet
CN.dat$PlotID <- gsub("B", "B127", CN.dat$PlotID)
CN.dat$PlotID <- gsub("H", "HH115", CN.dat$PlotID)
CN.dat$PlotID <- gsub("U", "U134", CN.dat$PlotID)
CN.dat$PlotID <- gsub("N", "N115", CN.dat$PlotID)

#Renaming species to match googe sheet
CN.dat$Taxon <- gsub("QA", "Que. alba", CN.dat$Taxon)
CN.dat$Taxon <- gsub("TA", "Til. americana", CN.dat$Taxon)
CN.dat$Taxon <- gsub("AS", "Ace. saccharum", CN.dat$Taxon)


#Creating a column with the difference between dates to create a rate of mass column
#This is stolen from the leaf litter code could have a direct work flow but the other script needs plot aggregated. Might fix later

#Combining our CN frame with our leaf litter datatframe
leaf.comb <- merge(dat.agg, CN.dat, by.x=c("date_collection", "plot", "taxon"), by.y=c("Date", "PlotID", "Taxon"))

#Creating a new data frame to add in the amount of days in between measurements
date.df <- data.frame(unique(leaf.comb$date_collection))
colnames(date.df) <- c("Date")
date.df$date_comp <- 0

for(i in 1:nrow(date.df)){
  n <- (date.df[i+1,1] - date.df[i,1])
  date.df$date_comp[i+1] <- n
}

leaf.final <- merge(leaf.comb, date.df ,by.x=c("date_collection"), by.y = c("Date"))

#addes value for first column based on knowledge of last date. Could maybe automate later but unsure if thats really more efficient
leaf.final <- leaf.final %>% transform(date_comp = ifelse(date_comp==0, 21, date_comp))

#Using the date difference to calculate mass per day
leaf.final$mass_per_day <- 0

for(i in 1:nrow(leaf.final)){
    n <- (leaf.final[i,"mass_g"]/leaf.final[i,"date_comp"])
  leaf.final$mass_per_day[i] <- n
}

#Adding middate value for grpahical representation
leaf.final$date_collection <- as.Date(leaf.final$date_collection)
leaf.final$mid_date <- as.Date((leaf.final$date_collection) - (leaf.final$date_comp/2))

#Creating proportions. This is real messy but it works
leaf.prop <- leaf.final %>%
              group_by(plot, date_collection) %>%
              mutate(plot_mean = mean(C.N))


leaf.prop <- leaf.prop %>%
              group_by(plot, date_collection) %>%
              mutate(sd = sd(C.N))

leaf.prop <- leaf.prop %>%
              group_by(plot, date_collection) %>%
              mutate(taxon_count = length(unique(taxon)))

leaf.prop <- leaf.prop %>% mutate(taxon_prop = C.N / taxon_count)



ggplot(leaf.final, aes(x=mid_date, y=taxon, fill=mass_per_day, width=date_comp))+
  facet_wrap(~plot)+
  geom_tile(color='white', aes(color=mid_date))+
  scale_x_date(labels = leaf.final$date_collection, 
               breaks = leaf.final$date_collection) +
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("mass_per_day over time periods by species")


#Visualizations
ggplot(leaf.final, aes(x=date_collection, y=C.N))+
  facet_wrap(~plot)+
  geom_line(aes(color=taxon))+
  geom_point(aes(color = taxon))+
  scale_x_date(labels = leaf.final$date_collection, 
               breaks = leaf.final$date_collection) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5))+
  ggtitle("C:N ratio over time")


ggplot(leaf.final, aes(x=mass_per_day, y = C.N))+
  geom_jitter()+
  ggtitle("Mass_per_day vs. C:N")


ggplot(leaf.final, aes(x=date_collection, y =C.N))+
  facet_wrap(~plot)+
  ylim(-10,80)+
  geom_point(aes(color = taxon))+
  geom_smooth(aes(color = taxon))+
  scale_x_date(labels = leaf.final$date_collection, 
               breaks = leaf.final$date_collection) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5))+
  ggtitle("General trend of C:N ratio over time by plot")

ggplot(leaf.final, aes(x=taxon))+
  geom_bar(aes(fill=plot))+
  ggtitle("taoxn per day")
  
ggplot(leaf.final, aes(x=plot, y=C.N))+
  facet_wrap(~date_collection)+
  geom_bar(aes(fill=taxon), stat = "identity")+
  ggtitle("C.N ratio of each species in a plot")

ggplot(leaf.prop, aes(x=date_collection, y=taxon_prop))+
  facet_wrap(~plot)+
  geom_line(aes(color=taxon))+
  scale_x_date(labels = leaf.final$date_collection, 
               breaks = leaf.final$date_collection) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5))+
  ggtitle("C.N ratio of each species in a plot")
  


ggplot(leaf.final, aes(x=date_collection, y=mass_per_day))+
  facet_wrap(~plot, scales = 'free_y')+
  geom_point(aes(color = taxon))+
  geom_line(aes(color = taxon))+
  scale_x_date(labels = leaf.final$date_collection, 
               breaks = leaf.final$date_collection) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5))+
  ggtitle("mass_per_day")


ggplot(leaf.final, aes(x=date_collection, y=C.N))+
  facet_wrap(~taxon)+
  geom_line(aes(color=plot))+
  geom_point(aes(color=plot))+
  scale_x_date(labels = leaf.final$date_collection, 
               breaks = leaf.final$date_collection) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5))+
  ggtitle("CN analysis")

ggplot(leaf.final, aes(x=date_collection, y=mass_per_day))+
  facet_wrap(~taxon)+
  geom_line(aes(color=plot))+
  geom_point(aes(color=plot))+
  scale_x_date(labels = leaf.final$date_collection, 
               breaks = leaf.final$date_collection) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5))+
  ggtitle("CN analysis")








