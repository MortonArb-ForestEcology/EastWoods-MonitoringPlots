# Loading, formatting, and exploratory graphing of the Understory community data
library(ggplot2)
source("0_Format_Understory_Data.R")

# Load the 2019 Understory plot data from 
dat.2019 <- get.understory(YEAR=2019)
dat.2019 <- dat.2019[!dat.2019$Name.Common=="NOTHING",]
summary(dat.2019)
unique(dat.2019$Name.Common)

ggplot(data=dat.2019) +
  facet_wrap(~Plot) +
  geom_histogram(aes(x=Obs.Date, fill=Subplot), stat="count", binwidth=8) +
  scale_y_continuous(name="Number Species")

ggplot(data=dat.2019) +
  facet_wrap(~Plot) +
  geom_histogram(aes(x=Obs.Date, y=Cover/4, fill=Subplot), stat="sum", binwidth=8) +
  scale_y_continuous(name="Total Cover")


# get the data from a particular sheet



# Load the 2020 Understory plot data