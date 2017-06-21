#setting a working directory

setwd("~/GitHub/EastWoods-MonitoringPlots/TreeCensus_2017/")

tree.data <-read.csv("data/TreeData-raw_data.csv", na.strings="")
class(tree.data)
summary(tree.data)

#this is a comment

#making a histogram of diameters

hist(tree.data$DBH)

#another way 

hist(tree.data[tree.data$Plot=="A1","DBH"])
dim(tree.data)

#checking data for the largest tree

size.max <- max(tree.data$DBH)
tree.data$DBH==size.max
tree.data[tree.data$DBH==size.max,]


install.packages("ggplot2")

library(ggplot2)
mean(tree.data[tree.data$Sp_code=="QUAL","DBH"])

summary(tree.data$Sp_code)
sd(tree.data[tree.data$Sp_code=="QUAL","DBH"])

mean(tree.data[tree.data$Sp_code=="ACSA","DBH"])

sd(tree.data[tree.data$Sp_code=="ACSA","DBH"])