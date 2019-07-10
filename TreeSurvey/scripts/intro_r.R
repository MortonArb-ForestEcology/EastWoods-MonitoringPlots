#setting a working directory

setwd("~/GitHub/EastWoods-MonitoringPlots/TreeCensus_2017/")

#importing data

tree.data <-read.csv("data/TreeData-raw_data.csv", na.strings="")

#basic stats about the data

class(tree.data)
summary(tree.data)

#making a histogram of diameters

hist(tree.data$DBH)

#a way to look at the DBHs of just plot A1

hist(tree.data[tree.data$Plot=="A1","DBH"])
dim(tree.data)

#checking data for the largest tree

size.max <- max(tree.data$DBH)
tree.data$DBH==size.max
tree.data[tree.data$DBH==size.max,]

#graph package, shouldn't need to install again

install.packages("ggplot2")

#importing package

library(ggplot2)

#basic stats with functions

mean(tree.data[tree.data$Sp_code=="QUAL","DBH"])

summary(tree.data$Sp_code)
sd(tree.data[tree.data$Sp_code=="QUAL","DBH"])

mean(tree.data[tree.data$Sp_code=="ACSA","DBH"])

sd(tree.data[tree.data$Sp_code=="ACSA","DBH"])



  
  
  
  