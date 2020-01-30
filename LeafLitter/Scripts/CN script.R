#Script for visualizing leaf litter data
library(googlesheets4)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(lubridate)

path.l <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Leaf_litter_data/"

setwd(path.l)

#grabbing the file from google drive and making it a workable data frame
litter.df <- sheets_find("Leaf_Litter_Data")
dat.lit <- data.frame(sheets_read(litter.df, range='raw_data'))

#removing NA values created by fact that volunteers record plot and date in advance of acquiring data
dat.lit <- dat.lit[!is.na(dat.lit$trap_ID),]
dat.leaf <- dat.lit[dat.lit$tissue=="leaf",]
dat.leaf <- dat.leaf[!is.na(dat.leaf$trap_ID),]

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


#pull out discrete dates, order them, do the difference, then add them to the matchign columns, then do mass/day



#Combining our CN frame with our leaf litter datatframe
leaf.comb <- merge(dat.leaf, CN.dat, by.x=c("date_collection", "plot", "taxon"), by.y=c("Date", "PlotID", "Taxon"))
leaf.comb <- subset(leaf.comb, select=-c(4,5,8:14,16,17,19))

date.df <- data.frame(unique(leaf.comb$date_collection))
colnames(date.df) <- c("Date")
date.df$date_comp <- 0

rows <- 1
for(i in rows:nrow(date.df$Date)){
      n <- (date.df[i+1,]-date.df[i,])
      date.df$date_comp[i+1] <- n
}

str(leaf.comb)

#removing values casued by first measurement
dat.leaf <- dat.leaf %>% transform(date_comp = ifelse(date_comp==0, NA, date_comp))
dat.leaf$mass_per_day <- 0

for(i in 1:nrow(dat.leaf)){
  if(!is.na(dat.leaf[i, "date_comp"])){
    n <- (dat.leaf[i,"mass_g"]/dat.leaf[i,"date_comp"])
  }else{n=NA}
  dat.leaf$mass_per_day[i] <- n
}

ggplot(leaf.comb, aes(x=date_collection, y=C.N))+
  facet_wrap(~plot)+
  geom_line(aes(color=taxon))+
  ggtitle("CN analysis")

ggplot(leaf.comb, aes(x=date_collection, y=mass_g))+
  facet_wrap(~plot)+
  geom_line(aes(color=taxon))+
  ggtitle("CN analysis")


#Visualizing
ggplot(CN.dat, aes(x=Date, y=C.N))+
  geom_smooth(aes(color=Species))+
  ggtitle("CN analysis")

ggplot(CN.dat, aes(x=Date, y=C.N))+
  facet_wrap(~PlotID)+
  geom_line(aes(color=Species))+
  geom_point(aes(color=Species))+
  ggtitle("CN analysis")

ggplot(CN.dat, aes(x=Date, y=C.N))+
  geom_point(aes(color=Species))+
  ggtitle("CN analysis")

ggplot(CN.dat, aes(x=Date, y=C.N))+
  geom_smooth(aes(color=PlotID))+
  ggtitle("CN analysis")








