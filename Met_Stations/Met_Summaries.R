library(ggplot2)

path.met <- "/Volumes/GoogleDrive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/Data_Clean/Clean_data/"
path.figs <- file.path("/Volumes/GoogleDrive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations", "figures")

theme.meghan <-   theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_rect(fill=NA, colour = "black", size=.7),
                        axis.title.x = element_text(margin = margin(t = 10, b=5), size=14),
                        axis.title.y = element_text(margin = margin(l = 5, r=5), size=14),
                        axis.text.x= element_text(margin = margin(t = 10), size=12),
                        axis.text.y=element_text(margin = margin(r = 10), size=12),
                        axis.ticks.length=unit(-0.3, "cm"),
                        axis.ticks.margin=unit(0.5, "cm"),
                        axis.ticks = element_line(colour = "black", size = 0.4))


plots <- dir(path.met)

met.all <- data.frame()
for(PLOT in plots){
  fnow <- read.csv(file.path(path.met, PLOT, paste0(PLOT, ".csv")))
  
  met.all <- rbind(met.all, fnow)
}
met.all$Date <- as.Date(met.all$Date)
met.all$Date_Time <- as.POSIXct(met.all$Date_Time, format="%Y-%m-%d %H:%M:%S")
met.all$Year <- lubridate::year(met.all$Date)
met.all$Month <- lubridate::month(met.all$Date)
met.all$Yday <- lubridate::yday(met.all$Date)
# head(met.all)
summary(met.all)

# Reduce or timeframe to just the period we're analyzing wiht other data
df.yday <- data.frame(Yday=1:365)
met.all <- met.all[met.all$Year>=2019,]
met.all <- merge(met.all, df.yday, all=T)
summary(met.all)



met.day.mean <- aggregate(cbind(Air_Temp, Soil_Temp, Soil_Moisture, Relative_Humidity, PAR) ~ Plot_Name + Date + Year + Month + Yday, data=met.all, FUN=mean)
met.day.min <- aggregate(cbind(Air_Temp, Soil_Temp, Soil_Moisture, Relative_Humidity, PAR) ~ Plot_Name + Date + Year + Month+ Yday, data=met.all, FUN=min)
met.day.max <- aggregate(cbind(Air_Temp, Soil_Temp, Soil_Moisture, Relative_Humidity, PAR) ~ Plot_Name + Date + Year + Month+ Yday, data=met.all, FUN=max)

day.mean.long <- stack(met.day.mean[,c("Air_Temp", "Soil_Temp", "Soil_Moisture", "Relative_Humidity", "PAR")])
# names(day.mean.long) 
day.mean.long[,c("Plot_Name", "Date", "Year", "Month", "Yday")] <- met.day.mean[,c("Plot_Name", "Date", "Year", "Month", "Yday")]
summary(day.mean.long)

day.mean.long$Season[day.mean.long$Month %in% c(1:2)] <- "0 - Winter"
day.mean.long$Season[day.mean.long$Month %in% c(3:4)] <- "1 - Early Spring"
day.mean.long$Season[day.mean.long$Month %in% c(5)] <- "3 - Late Spring"
day.mean.long$Season[day.mean.long$Month %in% c(6:8)] <- "4 - Summer"
day.mean.long$Season[day.mean.long$Month %in% c(9:10)] <- "5 - Early Fall"
day.mean.long$Season[day.mean.long$Month %in% c(11)] <- "6 - Late Fall"
day.mean.long$Season[day.mean.long$Month %in% c(12)] <- "7 - Early Winter"

pdf(file.path(path.figs, "Meteorology_by_Year.pdf"), height=8, width=11)
for(YR in unique(day.mean.long$Year)){
  print(ggplot(data=day.mean.long[day.mean.long$Year==YR,]) +
    ggtitle(YR) +
    facet_grid(ind~Month, scales="free_y") +
    geom_boxplot(aes(x=Plot_Name, y=values, fill=Plot_Name)) +
    scale_fill_brewer(palette = "Dark2") +
    theme_linedraw() + theme.meghan + theme(legend.position="bottom", strip.background = element_rect(fill=NA), strip.text = element_text(color="black"), axis.text.x = element_blank()) )
  }
dev.off()


pdf(file.path(path.figs, "Meteorology_by_Variable.pdf"), height=8, width=11)
for(VAR in unique(day.mean.long$ind)){
  print(ggplot(data=day.mean.long[day.mean.long$ind==VAR,]) +
          ggtitle(VAR) +
          facet_wrap(~Month, scales="free_y") +
          geom_boxplot(aes(x=as.factor(Year), y=values, fill=Plot_Name)) +
          scale_fill_brewer(palette = "Dark2") +
          labs(x="Year") +
          theme_bw() + theme.meghan + theme(legend.position="bottom", strip.background = element_rect(fill=NA), strip.text = element_text(color="black")) )
}
dev.off()

pdf(file.path(path.figs, "Meteorology_by_Plot.pdf"), height=8, width=11)
for(PLOT in unique(day.mean.long$Plot_Name)){
  print(ggplot(data=day.mean.long[day.mean.long$Plot_Name==PLOT,]) +
          ggtitle(PLOT) +
          facet_grid(ind~Month, scales="free_y") +
          geom_boxplot(aes(x=as.factor(Year), y=values, fill=as.factor(Year))) +
          # scale_fill_brewer(palette = "Dark2") +
          labs(x="Year") +
          theme_linedraw() + theme.meghan + theme(legend.position="bottom", strip.background = element_rect(fill=NA), strip.text = element_text(color="black"), axis.text.x = element_blank()) )
}
dev.off()


pdf(file.path(path.figs, "Meteorology_by_Variable_Daily.pdf"), height=8, width=11)
for(VAR in unique(day.mean.long$ind)){
  print(ggplot(data=day.mean.long[day.mean.long$ind==VAR,]) +
          ggtitle(VAR) +
          facet_wrap(~Plot_Name) +
          geom_line(aes(x=Date, y=values, color=Plot_Name)) +
          scale_color_brewer(palette = "Dark2") +
          labs(x="Year") +
          theme_bw() + theme.meghan + theme(legend.position="bottom", strip.background = element_rect(fill=NA), strip.text = element_text(color="black")) )
}
dev.off()

ggplot(data=met.day.mean ) +
  facet_wrap(~Plot_Name) +
  geom_line(aes(x=Date, y=Air_Temp, color=Plot_Name)) +
  scale_color_brewer(palette="Dark2")


ggplot(data=met.day.mean[met.day.mean$Month %in% 6:8,]) +
  geom_boxplot(aes(x=as.factor(Year), y=Soil_Temp, fill=Plot_Name)) +
  scale_color_brewer(palette = "Dark2") 
ggplot(data=met.day.mean[met.day.mean$Month %in% 6:8,]) +
  geom_boxplot(aes(x=as.factor(Year), y=Soil_Moisture, fill=Plot_Name)) +
  scale_color_brewer(palette = "Dark2") 
ggplot(data=met.day.mean[met.day.mean$Month %in% 6:8,]) +
  geom_boxplot(aes(x=as.factor(Year), y=PAR, fill=Plot_Name)) +
  scale_color_brewer(palette = "Dark2") 


# Getting some summary stats
summer.stats <- aggregate(cbind(Air_Temp, Soil_Temp, Soil_Moisture, Relative_Humidity, PAR) ~ Plot_Name + Year, data=met.all[met.all$Month %in% 6:8,], FUN=mean)

ggplot(data=summer.stats) +
  geom_line(aes(x=Year, y=Soil_Temp, color=Plot_Name))
