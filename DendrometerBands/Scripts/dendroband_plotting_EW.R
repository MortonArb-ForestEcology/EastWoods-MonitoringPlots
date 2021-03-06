library(ggplot2) #Load necessary libraries
library(scales)
library(dplyr)
library("ggpubr")

#Reading in data and formatting dates
setwd("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Raw_data")
dendroband <- read.csv("DendrobandObservations_EastWoods.csv", na.strings=c("", "negative", "9999.00", "999.00","9999", "999")) #read in data file
dendroband$date_observed <- as.Date(dendroband$date_observed, format="%m/%d/%Y") #Format dates as dates

#Changing species names for easier graphing
dendroband$taxon <- dendroband$species
dendroband$taxon <- recode(dendroband$species, alba = "Q. alba", 
              americana = "T. americana", 
              macrocarpa = "Q. macrocarpa",
              nigra = "J. nigra",
              rubra = "Q. rubra",
              saccharum = "A. saccharum",
              serotina = "P. serotina",
              virginiana = "O. virginiana")

#summarizing data and data types
summary(dendroband) 
str(dendroband)

#Order by date observed
dendroband <- dendroband[order(dendroband$date_observed),] #sort dataframe by date of measurement

#Diameter
dendroband$dist_from_collar <- dendroband$dist_from_collar / pi #convert circumference to diameter in the dataframe



#subtract minimum for each tree to offset bands settling
for(id in unique(dendroband$id)){ #for each tree
  dendroband[which(dendroband$id == id),]$dist_from_collar <- 
    dendroband[which(dendroband$id == id),]$dist_from_collar -
    min(dendroband[which(dendroband$id == id),]$dist_from_collar, na.rm = TRUE) #subtract the minimum from each measurement
}

#Basal area (mm^2) 
dendroband$dist_from_collar <- pi * ((dendroband$dist_from_collar / 2) ^2)

#Convert diameter/area to growth (mm/day) - use difftime to get days between each measurement
growth_df1 <- data.frame() #create empty df for rbinding

for (id in unique(dendroband$id)){ #for each tree
  
  dates <- dendroband[which(dendroband$id == id),]$date_observed #get observation dates
  obs <- dendroband[which(dendroband$id == id),]$dist_from_collar #get measurements
  seq_1 <- seq(2, length(dates)) #create sequence for selecting dates to subtract
  seq_2 <- seq_1 + 1 #create sequence for selecting dates to subtract from
  
  id_df <- dendroband[which(dendroband$id==id),][3:(length(dates)),][,1:which(colnames(dendroband) == "id")] #create df with metadata
  
  date_diff <- list() #create empty list for number of days
  obs_diff <- list() #create empty list for mm grown
  
  for (d in 1:(length(dates) - 2)){ #for each date difference (remembering that the 1st date is considered not reliable)
    diff <- difftime(dates[seq_2[d]], dates[seq_1[d]], units = c("days")) #calculate difference between two dates
    date_diff <- append(date_diff, as.numeric(diff), after = length(date_diff)) #append days between observations to list
    
    
    dist_diff <- obs[seq_2[d]] - obs[seq_1[d]] #calculate difference between measurements
    obs_diff <- append(obs_diff, dist_diff, after = length(obs_diff)) #append measurement differences to list
    
    
  }
 
  growth <- as.numeric(obs_diff) / as.numeric(date_diff) #Divide mm distance by number of days to get mm/day
  id_df <- do.call(cbind, list(id_df,growth)) #add growth measurements to new column combined with metadata  
  colnames(id_df)[length(id_df)] <- "mm_day" #Change column name
  growth_df1 <- rbind(growth_df1, id_df) #add growth df for each individual tree to a single df
  
}

#2017 data was noisy. Creating subset starting at 2018
dendroband$date_observed <- as.POSIXct(dendroband$date_observed, format="%Y-%m-%d")
after2017 <- subset(dendroband, date_observed > "2018-01-01")


##############

#Plot distance

##############



#Plotting total distance by species 2017-2019
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_TotalDistanceFromCollar_2017-2019.png")
ggplot(data=dendroband[!is.na(dendroband$dist_from_collar),]) + #Set basic plot parameters
  geom_point(aes(x=date_observed, y=dist_from_collar, group = id, col = as.factor(id)), size=1) + #Add points
  geom_line(aes(x=date_observed, y=dist_from_collar, group = id, col = as.factor(id)), size=.9) + #Specify line graph
  theme(legend.position = "none",  axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust=0.5)) + 
  ylab(expression("Distance from Collar (mm)"))+
  xlab(expression("Date Observed")) +
  ggtitle("Total Measured Distance from Collar to Line 2017-2019") +
  scale_x_datetime(date_breaks = "2 months", labels = date_format("%b %Y"))+
  scale_color_discrete() +
  facet_wrap(~taxon, ncol=2, scales = "free") #Specify panels
dev.off()

#Plotting distance by plot 2017-2019
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_TotalDistanceFromCollar_byPlot_2017-2019.png")
ggplot(data=dendroband[!is.na(dendroband$dist_from_collar),]) + #Set basic plot parameters
  geom_point(aes(x=date_observed, y=dist_from_collar, group = id, col = as.factor(id)), size=1) + #Add points
  geom_line(aes(x=date_observed, y=dist_from_collar, group = id, col = as.factor(id)), size=.9) + #Specify line graph
  theme(legend.position = "none",  axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust=0.5)) + 
  ylab(expression("Distance from Collar (mm)"))+
  xlab(expression("Date Observed")) +
  ggtitle("Total Measured Distance from Collar to Line 2017-2019") +
  scale_x_datetime(date_breaks = "2 months", labels = date_format("%b %Y"))+
  scale_color_discrete() +
  facet_wrap(~plot, scales = "free") #Specify panels
dev.off()





#Plotting total distance by species 2018-2019
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_TotalDistanceFromCollar_2018-2019.png")
ggplot(data=after2017[!is.na(after2017$dist_from_collar),]) + #Set basic plot parameters
  geom_point(aes(x=date_observed, y=dist_from_collar, group = id, col = as.factor(id)), size=1) + #Add points
  geom_line(aes(x=date_observed, y=dist_from_collar, group = id, col = as.factor(id)), size=.9) + #Specify line graph
  theme(legend.position = "none",  axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust=0.5)) + #legend and label parameters
  ylab(expression("Distance from Collar (mm)"))+
  xlab(expression("Date Observed")) +
  ggtitle("Total Measured Distance from Collar to Line 2018-2019") +
  scale_x_datetime(date_breaks = "2 months", labels = date_format("%b %Y"))+
  scale_color_discrete() +
  facet_wrap(~taxon, ncol=2, scales = "free") #Specify panels
dev.off()

#Plotting distance by plot 2018-2019
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_TotalDistanceFromCollar_byPlot_2018-2019.png")
ggplot(data=after2017[!is.na(after2017$dist_from_collar),]) + #Set basic plot parameters
  geom_point(aes(x=date_observed, y=dist_from_collar, group = id, col = as.factor(id)), size=1) + #Add points
  geom_line(aes(x=date_observed, y=dist_from_collar, group = id, col = as.factor(id)), size=.9) + #Specify line graph
  theme(legend.position = "none",  axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust=0.5)) + 
  ylab(expression("Distance from Collar (mm)"))+
  xlab(expression("Date Observed")) +
  ggtitle("Total Measured Distance from Collar to Line 2018-2019") +
  scale_x_datetime(date_breaks = "2 months", labels = date_format("%b %Y"))+
  scale_color_discrete() +
  facet_wrap(~plot, scales = "free") #Specify panels
dev.off()

#Boxplot showing total distance from collar by each plot for 2018-2019
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_TotalDistancefromCollar_Boxplot_ByPlot.png")
ggboxplot(after2017, x = "plot", y = "dist_from_collar", 
          color = "plot",
          legend = "none",
          ylab = "Distance From Collar (mm)", xlab = "Plot", 
          title = "Total Distance from Collar from 2018-2019")+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5)) #Specify panels
dev.off()


#Boxplot showing how each species compares across plots
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_TotalDistancefromCollar_Boxplot_PlotVsSpecies.png")
ggboxplot(after2017, x = "plot", y = "dist_from_collar", 
          color = "taxon",
          legend = "none",
          ylab = "Growth per Day (mm)", xlab = "Plot", 
          title = "Total Distance from Collar from 2018-2019")+
  facet_wrap(~taxon, ncol=2, scales = "free")+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5)) #Specify panels
dev.off()

#Showing within plot comparisons of species
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_TotalDistancefromCollar_Boxplot_ByPlot.png")
ggboxplot(after2017, x = "plot", y = "dist_from_collar", 
          color = "taxon",
          legend = "none",
          ylab = "Growth per Day (mm)", xlab = "Plot", 
          title = "Total Distance from Collar from 2018-2019")+
  facet_wrap(~plot, ncol=2, scales = "free")+
  theme(legend.position = "right", plot.title = element_text(hjust=0.5)) #Specify panels
dev.off()




####################

#Plot growth in mm/day

####################

#Changing species names for easier graphing
growth_df1$taxon <- growth_df1$species
growth_df1$taxon <- recode(growth_df1$species, alba = "Q. alba", 
                           americana = "T. americana", 
                           macrocarpa = "Q. macrocarpa",
                           nigra = "J. nigra",
                           rubra = "Q. rubra",
                           saccharum = "A. saccharum",
                           serotina = "P. serotina",
                           virginiana = "O. virginiana")
#subsetting data for 2018-2019
growth_df2 <- na.omit(growth_df1)
growth_df2$date_observed <- as.POSIXct(growth_df2$date_observed, format="%Y-%m-%d")
growth_df3 <- subset(growth_df2, date_observed > "2018-01-01")


#Boxplot showing total growth per day compared across plots
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_GrowthPerDay2018-2019_Boxplot_PlotsvsSpecies.png")
ggboxplot(growth_df3, x = "plot", y = "mm_day", 
          color = "plot",
          legend = "none",
          ylab = "Growth per Day (mm)", xlab = "Plot", 
          title = "Growth per Day from 2018-2019")+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5)) #Specify panels
dev.off()


#Showing comparison of growth per day from 2018-2019 of species across plots
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_GrowthPerDay2018-2019_Boxplot_PlotsvsSpecies.png")
ggboxplot(growth_df3, x = "plot", y = "mm_day", 
          color = "taxon",
          legend = "none",
          ylab = "Growth per Day (mm)", xlab = "Plot", 
          title = "Growth per Day from 2018-2019")+
  facet_wrap(~taxon, ncol=2, scales = "free")+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5)) #Specify panels
dev.off()


tapply(growth_df3$mm_day, growth_df3$plot, mean)
tapply(growth_df3$mm_day, growth_df3$plot, sd)
res.aov <- aov(mm_day ~ plot, data = growth_df2)
summary(res.aov)


# Growth per day from 2017-2019
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_GrowthPerDay_2017-2019_BySpecies.png")
ggplot(data=growth_df2) + #Set basic plot parameters
  geom_point(aes(x=date_observed, y=mm_day, group = id, col = as.factor(id)), size=1) + #Add points
  geom_line(aes(x=date_observed, y=mm_day, group = id, col = as.factor(id)), size=.9) + #Specify line graph
  theme(legend.position = "none",  axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust=0.5)) + 
  ylab(expression("Growth (mm)"))+
  ggtitle("Growth per day (mm) 2017-2019") +
  scale_color_discrete()+ #Specify color scheme
  scale_x_datetime(date_breaks = "2 months", labels = date_format("%b %Y"))+
  facet_wrap(~taxon, ncol=2, scales = "free") #Specify panels
#scale_y_continuous(breaks=c(0.00, 5.00, 10.00)) #Set y axis scale
dev.off()


# Growth per day for subset from 2018-2019
png("~/GitHub/EastWoods-MonitoringPlots/DendrometerBands/Figures//EWDendoband_GrowthPerDay_2018-2019_BySpecies.png")
ggplot(data=growth_df3) + #Set basic plot parameters
  geom_point(aes(x=date_observed, y=mm_day, group = id, col = as.factor(id)), size=1) + #Add points
  geom_line(aes(x=date_observed, y=mm_day, group = id, col = as.factor(id)), size=.9) + #Specify line graph
  theme(legend.position = "none",  axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust=0.5)) + 
  ylab(expression("Growth (mm)"))+
  ggtitle("Growth per day (mm) 2018-2019") +
  scale_color_discrete()+ #Specify color scheme
  scale_x_datetime(date_breaks = "2 months", labels = date_format("%b %Y"))+
  facet_wrap(~taxon, ncol=2, scales = "free") #Specify panels
  #scale_y_continuous(breaks=c(0.00, 5.00, 10.00)) #Set y axis scale
dev.off()

