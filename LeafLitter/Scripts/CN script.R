#Script for visualizing leaf litter data
library(tidyr)
library(ggplot2)

#setting directory
path.out <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Leaf_litter_data/"
setwd(path.out)

#reading file
ll.dat <- read.csv("Leaf_Litter_CN_Run1.csv")

ll.dat$Name <- as.character(ll.dat$Name)

#I was testing different methods here which is why they are differnet syntaxes
#Creating a new column with PlotID 
ll.dat$PlotID <- sapply(strsplit(ll.dat$Name, "-"), "[", 1)

#Creating a new column with Species
ll.dat$Species <- substr(ll.dat$Name, nchar(ll.dat$Name)-1, nchar(ll.dat$Name))

#Creating a new column with Date
ll.dat$Date <- substr(ll.dat$Name, 3, 10)
ll.dat$Date <- paste("20", ll.dat$Date, sep="")
ll.dat$Date <- as.Date(ll.dat$Date, "%Y-%m-%d")

#Visualizing
ggplot(ll.dat, aes(x=Date, y=C.N))+
  geom_smooth(aes(color=Species))+
  ggtitle("CN analysis")

ggplot(ll.dat, aes(x=Date, y=C.N))+
  facet_wrap(~PlotID)+
  geom_line(aes(color=Species))+
  geom_point(aes(color=Species))+
  ggtitle("CN analysis")

ggplot(ll.dat, aes(x=Date, y=C.N))+
  geom_point(aes(color=Species))+
  ggtitle("CN analysis")

ggplot(ll.dat, aes(x=Date, y=C.N))+
  geom_smooth(aes(color=PlotID))+
  ggtitle("CN analysis")








