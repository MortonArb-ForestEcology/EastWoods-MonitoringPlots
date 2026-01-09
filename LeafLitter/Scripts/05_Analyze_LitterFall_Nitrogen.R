library(ggplot2)
library(tidyverse)
library(nlme);  # Does the mixed effects model
library(emmeans) # will et us do a multi-comparisons test
library(MuMIn)

# Set up file paths etc. --> this should also indicate where you can find these files!
path.google <- "~/Google Drive/My Drive/REU 2025 - Morton Arboretum Leaf Litter"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.figs <- file.path(path.litter, "figures") # where we shoudl save some figures
path.save <- file.path(path.litter, "LeafLitterData_Clean_forArchiving") # Where we should save the data
path.REU <- file.path("~/Library/CloudStorage/GoogleDrive-lizer1@stolaf.edu/.shortcut-targets-by-id/1q2wvODXrDo0tgOTLpFqF7TqcWoKoHZjW/URF-REU 2025 - Lizer - Leaf Litter")

# Read in Data
aggLeafTrap <- read.csv(file.path(path.REU, "LeafLitter_byTrap_latest.csv"))
weekPeakTotal <- read.csv(file.path(path.REU, "LeafLitter_Peak_byTrap.csv"))

weekPeakSpp <- read.csv(file.path(path.REU, "PeakLeafDates_byPlot_bySpp.csv"))
weekPeakSpp <- weekPeakSpp[!is.na(weekPeakSpp$sci_name) & weekPeakSpp$sci_name %in% c("Quercus alba", "Quercus rubra", "Acer saccharum"),]
summary(weekPeakSpp)

datSpp <- read.csv(file.path(path.REU, "LeafLitter_byTrap_bySpecies_latest.csv"))
datSpp$date_collection <- as.Date(datSpp$date_collection)
datSpp <- datSpp[!is.na(datSpp$sci_name) & datSpp$sci_name %in% c("Quercus alba", "Quercus rubra", "Acer saccharum"),]
summary(datSpp)


datLLN <- read.csv(file.path(path.REU, "LeafLitter-Nitrogen_bySpecies_combined.csv"))
datLLN <- datLLN[!datLLN$sci_name %in% c("Tilia americana"),]
datLLN$date_collection <- as.Date(datLLN$date_collection)
datLLN$year <- lubridate::year(datLLN$date_collection)
datLLN$week <- lubridate::week(datLLN$date_collection)
datLLN$yday <- lubridate::yday(datLLN$date_collection)

metSummer <- read.csv(file.path(path.REU,"data/daymet/daymet_June-July-August_summaries_2017-2023.csv"))
metSummer$Precip.tot <- metSummer$prcp..mm.day.*sum(lubridate::days_in_month(6:8))


summary(aggLeafTrap)
summary(weekPeakTotal)
summary(weekPeakSpp)
summary(datLLN)
summary(datSpp)
summary(metSummer)

# If we want to do a weighted-average of leaf N for our key species, we need to merge that in
aggLeafPlotSpp <- aggregate(cbind(mass_g, mass_g_day)~ year + plot + sci_name + week + date_collection, data=datSpp, FUN=mean, na.rm=T)
summary(aggLeafPlotSpp)


summary(datLLN)
datLLNmerge <- merge(datLLN, aggLeafPlotSpp[,c("year", "sci_name", "date_collection", "plot", "mass_g", "mass_g_day")], all.x=T, all.y=F)
datLLNmerge$mass_g_day_Prop <- NA
summary(datLLNmerge)

# Calculating the weights for our C:N data
for(PLT in unique(datLLNmerge$plot)){
  for(SPP in unique(datLLNmerge$sci_name[datLLNmerge$plot==PLT])){
    for(YR in unique(datLLNmerge$year[datLLNmerge$plot==PLT & datLLNmerge$sci_name==SPP])){
      indNow <- which(datLLNmerge$plot==PLT & datLLNmerge$sci_name==SPP & datLLNmerge$year==YR)
      
      datLLNmerge$mass_g_day_Prop[indNow] <- datLLNmerge$mass_g_day[indNow]/sum(datLLNmerge$mass_g_day[indNow], na.rm=T)
    }
  }
}
datLLNmerge$C.N.weighted <- datLLNmerge$C.N * datLLNmerge$mass_g_day_Prop
datLLNmerge$perN.weighted <- datLLNmerge$X.N * datLLNmerge$mass_g_day_Prop
datLLNmerge$perC.weighted <- datLLNmerge$X.C * datLLNmerge$mass_g_day_Prop
summary(datLLNmerge)

datLLNpeak <- aggregate(cbind(C.N.weighted, perN.weighted, perC.weighted) ~ plot + sci_name + year, data=datLLNmerge, FUN=sum)
summary(datLLNpeak)

# Merging summer met in with our key datasets
datLLNpeak <- merge(datLLNpeak, metSummer, all.x=T, all.y=F)
summary(datLLNpeak)

# Just a quick visual inspection!
ggplot(datLLNpeak, aes(x=Precip.tot, y=perN.weighted, color=sci_name, fill=sci_name)) +
  geom_point() +
  stat_smooth(method="lm")

weekPeakSpp <- merge(weekPeakSpp, metSummer, all.x=T, all.y=F)
summary(weekPeakSpp)

ggplot(weekPeakSpp, aes(x=Precip.tot, y=weekPeakWt, color=sci_name, fill=sci_name)) +
  geom_point() +
  stat_smooth(method="lm")


weekPeakTotal <- merge(weekPeakTotal, metSummer, all.x=T, all.y=F)
summary(weekPeakTotal)

ggplot(weekPeakTotal, aes(x=Precip.tot, y=weekPeakWt)) +
  geom_point() +
  stat_smooth(method="lm")

# Following the progression of analyses in Cierra's presentation 
varsDrought <- c("Precip.tot", "VPD.tmax", "tmax..deg.c.", "n.Rainless")

summary(weekPeakTotal)
weekPeakStack <- stack(weekPeakTotal[,varsDrought])
weekPeakStack[,c("year", "plot", "trap_ID", "weekPeakWt", "totalMass_year")] <- weekPeakTotal[,c("year", "plot", "trap_ID", "weekPeakWt", "totalMass_year")]
summary(weekPeakStack)

ggplot(data=weekPeakStack, aes(x=values, y=weekPeakWt)) +
  facet_wrap(~ind, scales="free_x") +
  geom_point() +
  stat_smooth(method="lm")

summary(weekPeakSpp)
weekPeakStackSpp <- stack(weekPeakSpp[,varsDrought])
weekPeakStackSpp[,c("year", "plot", "sci_name", "weekPeakWt", "totalMass_year")] <- weekPeakSpp[,c("year", "plot", "sci_name", "weekPeakWt")]
summary(weekPeakStackSpp)

ggplot(data=weekPeakStackSpp, aes(x=values, y=weekPeakWt, color=sci_name, fill=sci_name)) +
  facet_wrap(~ind, scales="free_x") +
  geom_point() +
  stat_smooth(method="lm")

summary(datLLNpeak)
llnStackSpp <- stack(datLLNpeak[,varsDrought])
llnStackSpp[,c("year", "plot", "sci_name", "C.N.weighted", "perN.weighted", "perC.weighted")] <- datLLNpeak[,c("year", "plot", "sci_name", "C.N.weighted", "perN.weighted", "perC.weighted")]
summary(llnStackSpp)

ggplot(data=llnStackSpp, aes(x=values, y=perN.weighted, color=sci_name, fill=sci_name)) +
  facet_wrap(~ind, scales="free_x") +
  geom_point() +
  stat_smooth(method="lm")


# Based off of the above exploratory figures, lets roll with precip & n.Rainless as our vars
summary(weekPeakTotal)
totDropPrecip <- lme(weekPeakWt ~ Precip.tot, random=list(plot=~1, trap_ID=~1), data=weekPeakTotal)
summary(totDropPrecip)
anova(totDropPrecip)
r.squaredGLMM(totDropPrecip)

ggplot(weekPeakTotal, aes(x=Precip.tot, y=weekPeakWt)) +
  geom_smooth(method="lm", se=FALSE, linewidth = 1.5, color = "#4A5D7F") +
  geom_point(size = 2.5, color = "#4A5D7F", alpha=0.7) +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Week of Peak Leaf Drop"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_text(color = "#2E3033"),
    axis.line = element_line(color = "#2E3033", linewidth = 0.8, linetype = "solid"),
    legend.position = "none"
  )

totDropRainless <- lme(weekPeakWt ~ n.Rainless, random=list(plot=~1, trap_ID=~1), data=weekPeakTotal)
summary(totDropRainless)
r.squaredGLMM(totDropRainless)

ggplot(weekPeakTotal, aes(x=n.Rainless, y=weekPeakWt)) +
  geom_point() +
  stat_smooth(method="lm")

totDropTmax <- lme(weekPeakWt ~ tmax..deg.c., random=list(plot=~1, trap_ID=~1), data=weekPeakTotal)
summary(totDropTmax)
r.squaredGLMM(totDropTmax)

ggplot(weekPeakTotal, aes(x=tmax..deg.c., y=weekPeakWt)) +
  geom_point() +
  stat_smooth(method="lm")

totDropVPDmax <- lme(weekPeakWt ~ VPD.tmax, random=list(plot=~1, trap_ID=~1), data=weekPeakTotal)
summary(totDropVPDmax)
r.squaredGLMM(totDropVPDmax)

ggplot(weekPeakTotal, aes(x=VPD.tmax, y=weekPeakWt)) +
  geom_point() +
  stat_smooth(method="lm")


library(mgcv)
totDropVPDmax2 <- gamm(weekPeakWt ~ s(VPD.tmax, k=3), random=list(plot=~1, trap_ID=~1), data=weekPeakTotal)
summary(totDropVPDmax2)
summary(totDropVPDmax2$gam)
plot(totDropVPDmax2$gam)
r.squaredGLMM(totDropVPDmax2$lme)

ggplot(weekPeakTotal, aes(x=VPD.tmax, y=weekPeakWt)) +
  geom_point() +
  stat_smooth(method="loess")



# Working at the plot level to make it comparable to our nitrogen data
sppDropPrecip <- lme(weekPeakWt ~ Precip.tot*sci_name, random=list(plot=~1), data=weekPeakSpp)
summary(sppDropPrecip)
anova(sppDropPrecip)
r.squaredGLMM(sppDropPrecip)

sppDropPrecip2 <- lme(weekPeakWt ~ Precip.tot, random=list(plot=~1, sci_name=~1), data=weekPeakSpp)
summary(sppDropPrecip2)
anova(sppDropPrecip2)
r.squaredGLMM(sppDropPrecip2)

ggplot(weekPeakSpp, aes(x=Precip.tot, y=weekPeakWt, color=sci_name, fill=sci_name)) +
  geom_point() +
  stat_smooth(method="lm")


# Now looking at leaf percent Nitrogen
sppNPrecip <- lme(perN.weighted ~ Precip.tot*sci_name, random=list(plot=~1), data=datLLNpeak)
summary(sppNPrecip)
anova(sppNPrecip)
r.squaredGLMM(sppNPrecip)

sppNPrecip2 <- lme(perN.weighted ~ Precip.tot, random=list(plot=~1, sci_name=~1), data=datLLNpeak)
summary(sppNPrecip2)
anova(sppNPrecip2)
r.squaredGLMM(sppNPrecip2)

my_species_color_map <- c(
  "Quercus rubra" = "#8C3F48",
  "Quercus alba" = "#A6761D", 
  "Acer saccharum" = "#006D6F"
)

ggplot(datLLNpeak, aes(x=Precip.tot, y=perN.weighted, color=sci_name, fill=sci_name)) +
  geom_point(aes(color=sci_name), size = 2.5, alpha=0.7) +
  stat_smooth(method="lm", se=FALSE, linewidth = 2, alpha=0.8) +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  scale_color_manual(values=my_species_color_map) +
  scale_y_continuous() +
  scale_x_continuous() + 
  theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_text(color = "#2E3033"),
    axis.line = element_line(color = "#2E3033", linewidth = 0.8, linetype = "solid"),
    legend.position = "none"
  )


summary(datLLNpeak)
summary(weekPeakSpp)
datLLNpeak <- merge(datLLNpeak, weekPeakSpp[,c("year", "plot", "sci_name", "weekPeakWt", "propPeak", "prop3Wk")], all.x=T, all.y=F)
summary(datLLNpeak)

ggplot(datLLNpeak, aes(x=weekPeakWt, y=perN.weighted, color=sci_name, fill=sci_name)) +
  geom_point(aes(color=sci_name), size = 3, alpha=0.7) +
  stat_smooth(method = "lm", se= FALSE, size = 2) + 
  labs(
    x = "Week of Peak Leaf Drop",
    y = "Nitrogen Percentage\nin Leaf Litter (%)",
    color = "Species"
  ) + 
  scale_color_manual(values = my_species_color_map) + 
  theme_minimal(base_size = 19) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_text(color = "#2E3033"),
    axis.line = element_line(color = "#2E3033", linewidth = 0.8, linetype = "solid"),
    legend.position = "none",
  )

peakVn <- lme(perN.weighted ~ weekPeakWt, random=list(plot=~1, sci_name=~1), data=datLLNpeak)
summary(peakVn)
anova(peakVn)
r.squaredGLMM(peakVn)

write.csv(datLLNpeak, file.path(path.REU, "LeafLitter_peak_integrated_TimingNDrought.csv"), row.names=F)
write.csv(datLLNmerge, file.path(path.REU, "LeafLitter_combined_TimingNDrought.csv"), row.names=F)



########################################################################################################################################################
#Generating prediction figures

#drought vs timing
df_drought_peak <- data.frame(
  drought_severity = seq(0, 10, by = 0.5),
  Hypothesis = rep(c("Stress", "Sink"), each = 21)
) %>%
  mutate(
    predicted_week_peak = case_when(
      Hypothesis == "Stress" ~ 48 - (drought_severity * 2), 
      Hypothesis == "Sink" ~ 27 + (drought_severity * 2)
    )
  )
# Filter data for hypothesis 1 only
df_H1 <- df_drought_peak %>%
  filter(Hypothesis == "Stress")

ggplot(df_H1, aes(x = drought_severity, y = predicted_week_peak)) +
  geom_line(linewidth = 1.5, color = "#C47F4F") + 
  geom_point(size = 0, color = "#C47F4F") +
  labs(
    x = "Stress Metric",
    y = "Time of Leaf Drop"
  ) +
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 5)) + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + 
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none"
  )

ggplot(df_H1, aes(x = drought_severity, y = predicted_week_peak)) +
  geom_line(linewidth = 1.5, color = "#C47F4F") + 
  geom_point(size = 0, color = "#C47F4F") +
  labs(
    x = "Drought Metric",
    y = "Time of Leaf Drop"
  ) +
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 5)) + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + 
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none"
  )
# Filter data for hypothesis 2 only
df_H2 <- df_drought_peak %>%
  filter(Hypothesis == "Sink")

ggplot(df_H2, aes(x = drought_severity, y = predicted_week_peak)) +
  geom_line(linewidth = 1.5, color = "#3d7d4f") + 
  geom_point(size = 0, color = "#3d7d4f") +
  labs(
    x = "Drought Metric",
    y = "Time of Leaf Drop"
  ) +
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 5)) + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none"
  )

#making it match precipitation
ggplot(df_H1, aes(x = drought_severity, y = predicted_week_peak)) +
  geom_line(linewidth = 1.5, color = "#3d7d4f") + 
  geom_point(size = 0, color = "#3d7d4f") +
  labs(
    x = "Precipitation",
    y = "Time of Leaf Drop"
  ) +
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 5)) + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + 
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none"
  )

ggplot(df_H2, aes(x = drought_severity, y = predicted_week_peak)) +
  geom_line(linewidth = 1.5, color = "#C47F4F") + 
  geom_point(size = 0, color = "#C47F4F") +
  labs(
    x = "Precipitation",
    y = "Time of Leaf Drop"
  ) +
  scale_y_continuous(limits = c(25, 50), breaks = seq(25, 50, 5)) + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + 
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none"
  )




#Nitrogen vs drought
df_drought_N <- data.frame(
  drought_severity = seq(0, 10, by = 0.5),
  Hypothesis = rep(c("Stress", "Sink"), each = 21)
) %>%
  mutate(
    predicted_N = case_when(
      Hypothesis == "Stres" ~ 0.5 + (drought_severity * 0.5), 
      Hypothesis == "Sink" ~ 2.3 - (drought_severity * 0.15)
    )
  )
# Filter data for Hypothesis 1 only
df_H1N <- df_drought_N %>%
  filter(Hypothesis == "Stress")

ggplot(df_H1N, aes(x = drought_severity, y = predicted_N)) +
  geom_line(linewidth = 1.5, color = "#C47F4F") + 
  geom_point(size = 0, color = "#C47F4F") +
  labs(
    x = "Drought Metric",
    y = "Nitrogen Percentage\nin Leaf Litter (%)"
  ) +
  scale_y_continuous(limits = c(0.5, 2.5), breaks = seq(1.0, 2.5, 0.5)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none", 
    axis.title.y = element_text(margin = margin(r = 8)
    ))
# Filter data for Hypothesis 2 only
df_H2N <- df_drought_N %>%
  filter(Hypothesis == "Sink")

ggplot(df_H2N, aes(x = drought_severity, y = predicted_N)) +
  geom_line(linewidth = 1.5, color = "#3d7d4f") + 
  geom_point(size = 0, color = "#3d7d4f") +
  labs(
    x = "Drought Metric",
    y = "Nitrogen Percentage\nin Leaf Litter (%)"
  )+ 
  scale_y_continuous(limits = c(0.5, 2.5), breaks = seq(1.0, 2.5, 0.5)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + 
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none", 
    axis.title.y = element_text(margin = margin(r = 8)
    ))
#making it match precipitation
ggplot(df_H1N, aes(x = drought_severity, y = predicted_N)) +
  geom_line(linewidth = 1.5, color = "#3d7d4f") + 
  geom_point(size = 0, color = "#3d7d4f") +
  labs(
    x = "Precipitation",
    y = "Nitrogen Percentage\nin Leaf Litter (%)"
  ) +
  scale_y_continuous(limits = c(0.5, 2.5), breaks = seq(1.0, 2.5, 0.5)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none", 
    axis.title.y = element_text(margin = margin(r = 8)
    ))
ggplot(df_H2N, aes(x = drought_severity, y = predicted_N)) +
  geom_line(linewidth = 1.5, color = "#C47F4F") + 
  geom_point(size = 0, color = "#C47F4F") +
  labs(
    x = "Drought Metric",
    y = "Nitrogen Percentage\nin Leaf Litter (%)"
  )+ 
  scale_y_continuous(limits = c(0.5, 2.5), breaks = seq(1.0, 2.5, 0.5)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + 
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none", 
    axis.title.y = element_text(margin = margin(r = 8)
    ))




#Nitrogen vs Timing
df_peak_nitrogen <- data.frame(
  week_peak = seq(40, 50, by = 0.5)
) %>%
  mutate(
    predicted_N_percent = 2.5 - (week_peak - 40) * 0.15
  )

ggplot(df_peak_nitrogen, aes(x = week_peak, y = predicted_N_percent)) +
  geom_line(linewidth = 1.5, color = "#4A5D7F") + 
  geom_point(size = 0, color = "#4A5D7F") +
  labs(
    x = "Time of Leaf Drop",
    y = "Nitrogen Percentage\nin Leaf Litter (%)"
  ) +
  scale_y_continuous(limits = c(1.0, 2.5), breaks = seq(1.0, 2.5, 0.5)) +
  scale_x_continuous(limits = c(40, 50)) +
  theme_minimal(base_size = 22) +
  theme(
    axis.title = element_text(color = "#2E3033"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#2E3033", linewidth = 1, linetype = "solid"),
    legend.position = "none", 
    axis.title.y = element_text(margin = margin(r = 8))
  )
