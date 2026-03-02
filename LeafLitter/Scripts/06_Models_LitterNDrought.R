library(ggplot2)
library(tidyverse)
library(tidyr)
library(nlme);  # Does the mixed effects model
library(emmeans) # will et us do a multi-comparisons test
library(MuMIn)

# Set up file paths etc. --> this should also indicate where you can find these files!
path.google <- "~/Google Drive/My Drive/REU 2025 - Morton Arboretum Leaf Litter"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.figs <- file.path(path.litter, "figures") # where we shoudl save some figures
path.save <- file.path(path.litter, "LeafLitterData_Clean_forArchiving") # Where we should save the data
path.REU <- file.path("~/Library/CloudStorage/GoogleDrive-lizer1@stolaf.edu/.shortcut-targets-by-id/1q2wvODXrDo0tgOTLpFqF7TqcWoKoHZjW/URF-REU 2025 - Lizer - Leaf Litter")


#1: Load data and verify variables ----
datLLNpeak <- read.csv(file.path(path.REU, "LeafLitter_peak_integrated_TimingNDrought.csv"))
#^^all weeks collapsed into a single annual value per plot (weighted peak week)
summary(datLLNpeak)
str(datLLNpeak)

datLLNmerge <- read.csv(file.path(path.REU, "LeafLitter_combined_TimingNDrought.csv"))
#^^maintains individual entries for each collection
summary(datLLNmerge)
str(datLLNmerge)



datLLNmerge$date_collection <- as.Date(datLLNmerge$date_collection)
dat.midsummer.merge <- datLLNmerge %>% 
  filter(month(date_collection) %in% c(6, 7, 8))
summary(dat.midsummer.merge)

dat.fall.merge <- datLLNmerge %>% 
  filter(month(date_collection) %in% c(9, 10, 11))
summary(dat.fall.merge)

###########################################################
#2: Stoich EDA  ----
ggplot(datLLNmerge, aes(x = yday, y = X.N, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="loess", se=F) +
  #facet_wrap(~year) + 
  coord_cartesian(xlim=c(270,340)) +
  theme_minimal()

datLLNmerge %>% filter(X.N > 7) #theres an outlier
datLLNmerge %>% filter(year == 2025)

datLLNmerge_no.outlier <- datLLNmerge %>% filter(X.N < 7)

ggplot(datLLNmerge_no.outlier, aes(x = yday, y = X.N, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="loess", se=F) +
  #facet_wrap(~year) + 
  coord_cartesian(xlim=c(150,335)) +
  theme_minimal()

ggplot(dat.midsummer.merge, aes(x = yday, y = X.N, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="loess", se=F) +
  #facet_wrap(~year) + 
  coord_cartesian(xlim=c(150,335)) +
  theme_minimal()

ggplot(dat.fall.merge, aes(x = yday, y = X.N, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="loess", se=F) +
  #facet_wrap(~year) + 
  coord_cartesian(xlim=c(265,335)) +
  theme_minimal()


ggplot(datLLNmerge, aes(x = yday, y = X.C, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="loess", se=F) +
  facet_wrap(~year) + 
  coord_cartesian(xlim=c(270,340)) +
  theme_minimal()

ggplot(datLLNmerge, aes(x = yday, y = C.N, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="loess", se=F) +
  facet_wrap(~year) + 
  coord_cartesian(xlim=c(270,340)) +
  theme_minimal()

ggplot(datLLNmerge, aes(x = yday, y = mass_g_day, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="loess", se=F) +
  facet_wrap(~year) + 
  coord_cartesian(xlim=c(270,340)) +
  theme_minimal()


datStoich <- datLLNpeak %>%
  pivot_longer(cols = c(perN.weighted, perC.weighted, C.N.weighted, weekPeakWt), 
               names_to = "Metric", values_to = "Value")

ggplot(datStoich, aes(x = Precip.tot, y = Value, color = sci_name)) +
  facet_wrap(~Metric, scales = "free_y") +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se=F) +
  theme_minimal() 

#species
sppCNPrecip <- lme(C.N.weighted ~ Precip.tot*sci_name, random=list(plot=~1), data=datLLNpeak)
summary(sppCNPrecip)
anova(sppCNPrecip)
r.squaredGLMM(sppCNPrecip)
shapiro.test(resid(sppCNPrecip))
qqnorm(resid(sppCNPrecip))

sppNPrecip <- lme(perN.weighted ~ Precip.tot*sci_name, random=list(plot=~1), data=datLLNpeak)
summary(sppNPrecip)
anova(sppNPrecip)
r.squaredGLMM(sppNPrecip)
shapiro.test(resid(sppNPrecip))
qqnorm(resid(sppNPrecip))

spplogNPrecip <- lme(log(perN.weighted)~ Precip.tot*sci_name, random=list(plot=~1), data=datLLNpeak)
summary(spplogNPrecip)
anova(spplogNPrecip)
r.squaredGLMM(spplogNPrecip)
shapiro.test(resid(spplogNPrecip))
qqnorm(resid(spplogNPrecip))

sppCPrecip <- lme(perC.weighted ~ Precip.tot*sci_name, random=list(plot=~1), data=datLLNpeak)
summary(sppCPrecip)
anova(sppCPrecip)
r.squaredGLMM(sppCPrecip)
shapiro.test(resid(sppCPrecip))
qqnorm(resid(sppCPrecip))

#forest
CNPrecip_forest <- lme(C.N.weighted ~ Precip.tot, random=list(plot=~1), data=datLLNpeak)
summary(CNPrecip_forest)
anova(CNPrecip_forest)
r.squaredGLMM(CNPrecip_forest)
shapiro.test(resid(CNPrecip_forest))
qqnorm(resid(CNPrecip_forest))

NPrecip_forest <- lme(perN.weighted ~ Precip.tot, random=list(plot=~1, sci_name=~1), data=datLLNpeak)
summary(NPrecip_forest)
anova(NPrecip_forest)
r.squaredGLMM(NPrecip_forest)
shapiro.test(resid(NPrecip_forest))
qqnorm(resid(NPrecip_forest))
hist(resid(NPrecip_forest))

logNPrecip_forest <- lme(log(perN.weighted) ~ Precip.tot, random=list(plot=~1), data=datLLNpeak)
summary(logNPrecip_forest)
anova(logNPrecip_forest)
r.squaredGLMM(logNPrecip_forest)
shapiro.test(resid(logNPrecip_forest))
qqnorm(resid(logNPrecip_forest))

sqrtNPrecip_forest <- lme(sqrt(perN.weighted) ~ Precip.tot, random=list(plot=~1), data=datLLNpeak)
summary(sqrtNPrecip_forest)
anova(sqrtNPrecip_forest)
r.squaredGLMM(sqrtNPrecip_forest)
shapiro.test(resid(sqrtNPrecip_forest))
qqnorm(resid(sqrtNPrecip_forest))

CPrecip_forest <- lme(perC.weighted ~ Precip.tot, random=list(plot=~1), data=datLLNpeak)
summary(CPrecip_forest)
anova(CPrecip_forest)
r.squaredGLMM(CPrecip_forest)
shapiro.test(resid(CPrecip_forest))
qqnorm(resid(CPrecip_forest))


#############################
#3: Selecting drought variables (thanks leah for code inspo) ----
Nprecip <- lme(perN.weighted ~ Precip.tot, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
shapiro.test(resid(Nprecip))
qqnorm(resid(Nprecip))

NVPD<- lme(perN.weighted ~ VPD.avg, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
shapiro.test(resid(NVPD))
qqnorm(resid(NVPD))

Nrainless <- lme(perN.weighted ~ n.Rainless, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
shapiro.test(resid(Nrainless))
qqnorm(resid(Nrainless))

Nrainlessconsec <- lme(perN.weighted ~ RainlessConsec.max, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
shapiro.test(resid(Nrainlessconsec))
qqnorm(resid(Nrainlessconsec))

Nprecipday <- lme(perN.weighted ~ prcp..mm.day., random=list(sci_name=~1, plot=~1), data=datLLNpeak)
shapiro.test(resid(Nprecipday))
qqnorm(resid(Nprecipday))


DropPrecip <- lme(weekPeakWt ~ Precip.tot, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
summary(DropPrecip)
anova(DropPrecip)
r.squaredGLMM(DropPrecip)
shapiro.test(resid(DropPrecip))
qqnorm(resid(DropPrecip))
hist(resid(DropPrecip))
hist(datLLNpeak$weekPeakWt)
hist(datLLNpeak$Precip.tot)
# head(weekPeakSpp)


DropVPD <- lme(weekPeakWt ~ VPD.avg, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
shapiro.test(resid(DropVPD))
qqnorm(resid(DropVPD))

Droprainless <- lme(weekPeakWt ~ n.Rainless, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
shapiro.test(resid(Droprainless))
qqnorm(resid(Droprainless))

Droprainlessconsec <- lme(weekPeakWt ~ RainlessConsec.max, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
shapiro.test(resid(Droprainlessconsec))
qqnorm(resid(Droprainlessconsec))

Dropprecipday <- lme(weekPeakWt ~ prcp..mm.day., random=list(sci_name=~1, plot=~1), data=datLLNpeak)
shapiro.test(resid(Dropprecipday))
qqnorm(resid(Dropprecipday))


get_stats <- function(model, name) {
  r2 <- r.squaredGLMM(model)
  data.frame(
    Model = name,
    AIC = AIC(model),
    R2_Marginal = r2[1],
    R2_Conditional = r2[2])}

Ncombined <- lme(perN.weighted ~ Precip.tot + RainlessConsec.max, 
                  random = list(sci_name=~1, plot = ~1), 
                  data = datLLNpeak)
summary(Ncombined)
r.squaredGLMM(Ncombined)

Dropcombined <- lme(weekPeakWt ~ Precip.tot + RainlessConsec.max, 
                                  random = list(sci_name=~1, plot = ~1), 
                                  data = datLLNpeak)
summary(Dropcombined)
r.squaredGLMM(Dropcombined)

Nmodel_comparison <- rbind(
  get_stats(Nprecip, "N: Total Precip"),
  get_stats(NVPD, "N: Vapor Pressure Defecit"),
  get_stats(Nrainless, "N: Number of Rainless Days"),
  get_stats(Nrainlessconsec, "N: Number of Consecutive Rainless Days"),
  get_stats(Ncombined, "N: Total Precip AND Consec Rainless"),
  get_stats(Nprecipday, "N: Precipitation Per Day")
)
print(Nmodel_comparison)

Dropmodelcomparison <- rbind(
  get_stats(DropPrecip, "Timing: Total Precip"),
  get_stats(DropVPD, "Timing: Vapor Pressure Defecit"),
  get_stats(Droprainless, "Timing: Number of Rainless Days"),
  get_stats(Droprainlessconsec, "Timing: Number of Consecutive Rainless Days"),
            get_stats(Dropcombined, "Timing: Total Precip AND Consec Rainless"),
  get_stats(Dropprecipday, "Timing: Precipitation Per Day")
)
print(Dropmodelcomparison)

#############################
#4: Structural Equations Model ----

#showing that I want to test N + timing AND N + weather
library(multcomp)
library(multcompView)
library(piecewiseSEM)
library(lme4)
library(lmerTest)

Nprecip_linked <- lme(perN.weighted ~ Precip.tot + weekPeakWt, random = list(sci_name = ~1, plot = ~1), data = datLLNpeak)

forest_sem <- psem(DropPrecip, Nprecip_linked)
summary(forest_sem)


spNPrecip_linked <- lmer(perN.weighted ~ Precip.tot*sci_name + weekPeakWt*sci_name + (1 | plot), data = datLLNpeak)
spDropPrecip <- lmer(weekPeakWt ~ Precip.tot * sci_name + (1 | plot), data = datLLNpeak)
summary(spDropPrecip)
anova(spDropPrecip)
r.squaredGLMM(spDropPrecip)

spec_SEM <- psem(spDropPrecip, spNPrecip_linked)
summary(spec_SEM, standardize = "none")


###########################
#Weirdness when I added in 2019, investigating outlier ----

datLLNpeak[which.max(resid(NPrecip_forest, type = "pearson")), ]

#dat_no_outlier <- datLLNpeak[-13, ]

NPrecip_no_outlier <- lme(perN.weighted ~ Precip.tot, random = list(sci_name = ~1, plot = ~1), data = datLLNpeak)
summary(NPrecip_no_outlier)
r.squaredGLMM(NPrecip_no_outlier)

NTime_no_outlier <- lme(perN.weighted ~ weekPeakWt, random = list(sci_name = ~1, plot = ~1), data = datLLNpeak)
summary(NTime_no_outlier)
r.squaredGLMM(NTime_no_outlier)

ggplot(datLLNpeak, aes(x = weekPeakWt, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~year) + 
  #coord_cartesian(xlim=c(270,340)) +
  theme_minimal()

ggplot(datLLNpeak, aes(x = Precip.tot, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
 # facet_wrap(~plot)
 facet_wrap(~year) + 
  #coord_cartesian(xlim=c(270,340)) +
  theme_minimal()

no_2019 <- datLLNpeak[datLLNpeak$year!=2019,]

no2019_Nprecip <- lme(perN.weighted ~ Precip.tot, random=list(plot=~1, sci_name=~1), data=no_2019)
summary(no2019_Nprecip)
anova(no2019_Nprecip)
r.squaredGLMM(no2019_Nprecip)
no_2019[which.max(resid(no2019_Nprecip, type = "pearson")), ]


#ok so there's still a suspiciously low p-value and visualizing makes it look like theres some missing data points...
expected_years <- unique(datLLNpeak$year)
expected_plots <- c("B-127", "U-134", "N-115", "HH-115")
expected_species <- c("Acer saccharum", "Quercus alba", "Quercus rubra")

all_expected <- expand.grid(year = expected_years, plot = expected_plots, sci_name = expected_species)

missing_points <-anti_join(all_expected, datLLNpeak, by = c("year","plot","sci_name"))
print(missing_points)

#I wrote down the p-value for this before I added 2019 so this can be a check
DropPrecip <- lme(weekPeakWt ~ Precip.tot, random=list(sci_name=~1, plot=~1), data=no_2019)
Nprecip_linked <- lme(perN.weighted ~ Precip.tot + weekPeakWt, random = list(sci_name = ~1, plot = ~1), data = no_2019)

no_2019_sem <- psem(DropPrecip, Nprecip_linked)
summary(no_2019_sem)

#More not believing this
YearN <- lme(perN.weighted ~ as.factor(year), random = list(sci_name = ~1, plot = ~1), data = dat_no_outlier)
summary(YearN)
r.squaredGLMM(YearN)

Year_randomN <- lme(perN.weighted ~ Precip.tot, random = list(year=~1, sci_name=~1, plot=~1), data = dat_no_outlier)
summary(Year_randomN)
r.squaredGLMM(Year_randomN)

Year_randomNDrop <- lme(perN.weighted ~ weekPeakWt, random = list(year=~1, sci_name=~1, plot=~1), data = dat_no_outlier)
summary(Year_randomNDrop)
r.squaredGLMM(Year_randomNDrop)



########################################################
#Important Models rn ----
NPrecip_forest <- lme(perN.weighted ~ Precip.tot, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
summary(NPrecip_forest)
anova(NPrecip_forest)
r.squaredGLMM(NPrecip_forest)

sppNPrecip <- lme(perN.weighted ~ Precip.tot*sci_name, random=list(plot=~1), data=datLLNpeak)
summary(sppNPrecip)
anova(sppNPrecip)
r.squaredGLMM(sppNPrecip)

DropPrecip <- lme(weekPeakWt ~ Precip.tot, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
summary(DropPrecip)
anova(DropPrecip)
r.squaredGLMM(DropPrecip)

spDropPrecip <- lme(weekPeakWt ~ Precip.tot * sci_name, random=list(plot=~1), data=datLLNpeak)
summary(spDropPrecip)
anova(spDropPrecip)
r.squaredGLMM(spDropPrecip)

TimeN <- lme(perN.weighted ~ weekPeakWt, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
summary(TimeN)
anova(TimeN)
r.squaredGLMM(TimeN)

spTimeN <- lme(perN.weighted ~ weekPeakWt * sci_name, random=list(plot=~1), data = datLLNpeak)
summary(spTimeN)
anova(spTimeN)
r.squaredGLMM(spTimeN)




NPrecip_year <- lme(perN.weighted ~ Precip.tot, random=list(year=~1, sci_name=~1, plot=~1), data=datLLNpeak)
summary(NPrecip_year)
anova(NPrecip_year)
r.squaredGLMM(NPrecip_year)

DropPrecip_year <- lme(weekPeakWt ~ Precip.tot, random=list(year=~1, sci_name=~1, plot=~1), data=datLLNpeak)
summary(DropPrecip_year)
anova(DropPrecip_year)
r.squaredGLMM(DropPrecip_year)

TimeN_year <- lme(perN.weighted ~ weekPeakWt, random=list(year=~1, sci_name=~1, plot=~1), data=datLLNpeak)
summary(TimeN_year)
anova(TimeN_year)
r.squaredGLMM(TimeN_year)


####################################################################
#Making graphs for collaborator meeting ----


library(mosaic)
fav_stats(datLLNpeak$Precip.tot)
fav_stats(datLLNpeak$perN.weighted)
fav_stats(datLLNpeak$weekPeakWt)

ggplot(datLLNpeak, aes(x = Precip.tot, y = perN.weighted, color = sci_name)) +
  geom_jitter(alpha =0.6, width =3) +
  geom_smooth(method ="lm", se=F) +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()

ggplot(datLLNpeak, aes(x = Precip.tot, y = perN.weighted, color = sci_name)) +
  geom_jitter(alpha =0.6, width =3) +
  geom_smooth(aes(color = sci_name), method = "lm", se = FALSE, size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", size = 1.5, se = FALSE)+
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()

ggplot(datLLNpeak, aes(x = Precip.tot, y = perN.weighted)) +
  geom_smooth(aes(group = 1), method = "lm", color = "black",
              linewidth = 2, se = FALSE) +
  stat_smooth(aes(color = sci_name, alpha = 0.3), method = "lm",
              geom = "line", linewidth = 0.8, se = FALSE) +
  geom_point(aes(color = sci_name), alpha = 0.3) +
  theme_minimal() +
  labs(x = "Total Summer Precipitation (mm)",
       y = "Nitrogen Percentage in Leaf Litter (%)")


ggplot(datLLNpeak, aes(x = Precip.tot, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~plot) +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()



ggplot(datLLNpeak, aes(x = Precip.tot, y = weekPeakWt, color = sci_name)) +
  geom_point(aes(color = sci_name), alpha = 0.3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black",
              linewidth = 2, se = FALSE) +
  stat_smooth(aes(color = sci_name, alpha = 0.3), method = "lm",
              geom = "line", linewidth = 0.8, se = FALSE) +
  guides(alpha = "none") +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Week of Peak Litterfall",
    color = "Species"
  ) +
  theme_minimal()

ggplot(datLLNpeak, aes(x = Precip.tot, y = weekPeakWt, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~plot) +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Week of Peak Litterfall",
    color = "Species"
  ) +
  theme_minimal()



ggplot(datLLNpeak, aes(x = weekPeakWt, y = perN.weighted, color = sci_name)) +
  geom_point(aes(color = sci_name), alpha = 0.3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black",
              linewidth = 2, se = FALSE) +
  stat_smooth(aes(color = sci_name, alpha = 0.3), method = "lm",
              geom = "line", linewidth = 0.8, se = FALSE) +
  guides(alpha = "none") +
  labs(
    x = "Week of Peak Litterfall",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()

ggplot(datLLNpeak, aes(x = weekPeakWt, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~plot) +
  labs(
    x = "Week of Peak Litterfall",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()

ggplot(datLLNpeak, aes(x = weekPeakWt, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~year) +
  labs(
    x = "Week of Peak Litterfall",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()



datStoich <- datLLNpeak%>%
  pivot_longer(cols = c(perN.weighted, perC.weighted, C.N.weighted), 
               names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = case_match(Metric,
                             "perN.weighted" ~ "Nitrogen (%)",
                             "perC.weighted" ~ "Carbon (%)",
                             "C.N.weighted"  ~ "C:N Ratio"))

ggplot(datStoich, aes(x = Precip.tot, y = Value, color = sci_name)) +
  facet_wrap(~Metric, scales = "free_y") +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se=F) +
  labs(x = "Total Summer Precipitation (mm)",
       y = "Measured Value",
       color = "Species") +
  theme_minimal() 

ggplot(datStoich, aes(x = weekPeakWt, y = Value, color = sci_name)) +
  facet_wrap(~Metric, scales = "free_y") +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se=F) +
  labs(x = "Week of Peak Litterfall",
       y = "Measured Value",
       color = "Species") +
  theme_minimal() 


datDrought <- datLLNpeak%>%
  pivot_longer(cols = c(Precip.tot, VPD.avg, n.Rainless, RainlessConsec.max), 
               names_to = "Drought_Metric", values_to = "Metric_Value") %>%
  mutate(Drought_Metric = case_match(Drought_Metric,
                             "Precip.tot" ~ "Total Summer Precipitation",
                             "VPD.avg" ~ "Average VPD",
                             "n.Rainless"  ~ "# of Rainless Days",
                             "RainlessConsec.max" ~ "# of Consecutive\n Rainless Days"))

ggplot(datDrought, aes(x = Metric_Value, y = perN.weighted, color = sci_name)) +
  facet_wrap(~Drought_Metric, scales = "free_x") +
  geom_jitter(alpha = 0.5) +
  stat_smooth(method = "lm", se=F) +
  labs(x = "Drought Metric Value Value",
       y = "Nitrogen Percentage\n in Leaf Litter (%)",
       color = "Species") +
  theme_minimal()

ggplot(datDrought, aes(x = Metric_Value, y = weekPeakWt, color = sci_name)) +
  facet_wrap(~Drought_Metric, scales = "free_x") +
  geom_jitter(alpha = 0.5) +
  stat_smooth(method = "lm", se=F) +
  labs(x = "Drought Metric Value Value",
       y = "Week of Peak Litterfall",
       color = "Species") +
  theme_minimal()



dat_annual_summary <- datLLNpeak %>%
  group_by(year) %>%
  summarize(
    meanN = mean(perN.weighted, na.rm = TRUE),
    seN = sd(perN.weighted, na.rm = TRUE) / sqrt(n()),
    Precip = mean(Precip.tot))

ggplot(dat_annual_summary, aes(x = Precip, y = meanN)) +
  geom_smooth(method = "lm", se=FALSE) +
  geom_point() +
  geom_text(aes(label = year), vjust = -1.5) +
  theme_minimal() +
  labs(x = "Total Precipitation (mm)", 
       y = "Mean %N")
meanNprecip <- lm(meanN ~ Precip, data=dat_annual_summary)
summary(meanNprecip)



dat_species_summary <- datLLNpeak %>%
  group_by(year, sci_name) %>%
  summarize(
    meanN = mean(perN.weighted, na.rm = TRUE),
    seN = sd(perN.weighted, na.rm = TRUE) / sqrt(n()),
    Precip = mean(Precip.tot))

ggplot(dat_species_summary, aes(x = Precip, y = meanN, color = sci_name)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(x = "Total Precipitation (mm)", 
       y = "Mean %N",
       color = "Species")+
  theme_minimal()
  


# dat_distyear <- datLLNpeak %>%
# dplyr::select(year, perN.weighted, weekPeakWt, Precip.tot) %>%
# pivot_longer(cols = -year, names_to = "Variable", values_to = "Value") %>%
# mutate(Clean_name = case_match(Variable,
#                                 "year" ~ "Year",
#                                  "perN.weighted" ~ "%N",
#                                "weekPeakWt"  ~ "Week of Peak Litterfall",
#                               "Precip.tot" ~ "Total Precipitation (mm)"))

# ggplot(dat_distyear, aes(x = Variable, y = Value)) +
# geom_boxplot(outlier.shape = NA, alpha = 0.5) +
# geom_jitter(aes(color = as.factor(year)), width = 0.2, size = 2) +
# facet_wrap(~Clean_name, scales = "free_y") +
# theme_minimal() +
# theme(axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#    axis.ticks.x = element_blank()) +
#  labs(color = "Year")


# dat_distspec <- datLLNpeak %>%
# dplyr::select(sci_name, perN.weighted, weekPeakWt, Precip.tot) %>%
# pivot_longer(cols = -sci_name, names_to = "Variable", values_to = "Value") %>%
# mutate(Clean_name = case_match(Variable,
#                              "sci_name" ~ "Species",
#                             "perN.weighted" ~ "%N",
#                            "weekPeakWt"  ~ "Week of Peak Litterfall",
#                           "Precip.tot" ~ "Total Precipitation (mm)"))

# ggplot(dat_distspec, aes(x = Variable, y = Value)) +
# geom_boxplot(outlier.shape = NA, alpha = 0.5) +
# geom_jitter(aes(color = sci_name), width = 0.2, size = 2) +
#  facet_wrap(~Clean_name, scales = "free_y") +
# theme_minimal() +
# theme(axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#    axis.ticks.x = element_blank()) +
# labs(color = "Species")



dat_longyear <- datLLNpeak %>%
dplyr::select(year, perN.weighted, weekPeakWt, Precip.tot) %>%
pivot_longer(cols = -year, names_to = "Variable", values_to = "Value") %>%
mutate(Clean_name = case_match(Variable,
                             "perN.weighted" ~ "%N",
                            "weekPeakWt"    ~ "Week of Peak Litterfall",
                           "Precip.tot"    ~ "Total Precipitation (mm)"))

ggplot(dat_longyear %>% filter(Variable == "perN.weighted"), aes(x = as.factor(year), y = Value)) +
geom_boxplot(data = dat_longyear %>% filter(Variable == "perN.weighted") %>% mutate(year = "All"),
            aes(x = "Total Range"), alpha = 0.5) +
 geom_boxplot(aes(fill = as.factor(year)), alpha = 0.7, outlier.shape = NA) +
geom_jitter(width = 0.15, alpha = 0.4) +
 facet_wrap(~Clean_name, scales = "free") +
theme_minimal() +
 labs(x = "Year (vs Total Range)",
     y = "Percent Nitrogen (%)",
    fill = "Year")

ggplot(dat_longyear %>% filter(Variable == "weekPeakWt"), aes(x = as.factor(year), y = Value)) +
geom_boxplot(data = dat_longyear %>% filter(Variable == "weekPeakWt") %>% mutate(year = "All"),
            aes(x = "Total Range"), alpha = 0.5) +
geom_boxplot(aes(fill = as.factor(year)), alpha = 0.7, outlier.shape = NA) +
geom_jitter(width = 0.15, alpha = 0.4) +
facet_wrap(~Clean_name, scales = "free") +
theme_minimal() +
labs(x = "Year (vs Total Range)",
   y = "Week of Peak Litterfall",
  fill = "Year")


dat_precipyear <- datLLNpeak %>%
dplyr::select(year, Precip.tot) %>%
distinct()
ggplot(dat_precipyear, aes(x = Precip.tot, y = as.factor(year))) +
geom_point(aes(color = as.factor(year)), size = 5) +
theme_minimal() +
labs(x = "Total Precipitation (mm)",
   y = "Year",
  color = "Year")


dat_longsp <- datLLNpeak %>%
  dplyr::select(sci_name, perN.weighted, weekPeakWt, Precip.tot) %>%
  pivot_longer(cols = -sci_name, names_to = "Variable", values_to = "Value") %>%
  mutate(Clean_name = case_match(Variable,
                                "perN.weighted" ~ "%N",
                                 "weekPeakWt"    ~ "Week of Peak Litterfall",
                                "Precip.tot"    ~ "Total Precipitation (mm)"))


ggplot(dat_longsp %>% filter(Variable == "perN.weighted"), aes(x = as.factor(sci_name), y = Value)) +
  geom_boxplot(data = dat_longsp %>% filter(Variable == "perN.weighted") %>% mutate(sci_name = "All"),
               aes(x = "Total Range"), alpha = 0.5) +
  geom_boxplot(aes(fill = as.factor(sci_name)), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  facet_wrap(~Clean_name, scales = "free") +
  theme_minimal() +
  labs(x = "Species (vs Total Range)", 
       y = "Percent Nitrogen (%)",
       fill = "Species")

ggplot(dat_longsp %>% filter(Variable == "weekPeakWt"), aes(x = as.factor(sci_name), y = Value)) +
  geom_boxplot(data = dat_longsp %>% filter(Variable == "weekPeakWt") %>% mutate(sci_name = "All"), 
               aes(x = "Total Range"), alpha = 0.5) +
  geom_boxplot(aes(fill = as.factor(sci_name)), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  facet_wrap(~Clean_name, scales = "free") +
  theme_minimal() +
  labs(x = "Species (vs Total Range)", 
       y = "Week of Peak Litterfall",
       fill = "Species")


########################################
#Non-parametric tests ----
year_catN <- aov(perN.weighted ~ as.factor(year), data=datLLNpeak)
summary(year_catN)

year_cat_time <- aov(weekPeakWt ~ as.factor(year), data=datLLNpeak)
summary(year_cat_time)


YearN <- lme(perN.weighted ~ as.factor(year), random = list(sci_name = ~1, plot = ~1), data = datLLNpeak)
summary(YearN)
r.squaredGLMM(YearN)



cor.test(datLLNpeak$Precip.tot, datLLNpeak$perN.weighted, method = "spearman")
cor.test(datLLNpeak$Precip.tot, datLLNpeak$weekPeakWt, method = "spearman")
cor.test(datLLNpeak$weekPeakWt, datLLNpeak$perN.weighted, method = "spearman")


# Kruskal-Wallis: Does Nitrogen vary significantly by Year?
kruskal.test(perN.weighted ~ as.factor(year), data = datLLNpeak)

# Kruskal-Wallis: Does Timing vary significantly by Year?
kruskal.test(weekPeakWt ~ as.factor(year), data = datLLNpeak)


library(quantreg)
Nprecipmodel_median <- rq(perN.weighted ~ Precip.tot, data = datLLNpeak, tau = 0.5)
summary(Nprecipmodel_median)

Timeprecipmodel_median <- rq(weekPeakWt ~ Precip.tot, data = datLLNpeak, tau = 0.5)
summary(Timeprecipmodel_median)

TimeNmodel_median <- rq(perN.weighted ~ weekPeakWt, data = datLLNpeak, tau = 0.5)
summary(TimeNmodel_median)


taus <- c(0.1, 0.5, 0.9)
for (t in taus) {
  model <- rq(perN.weighted ~ Precip.tot, data = datLLNpeak, tau = t)
  print(summary(model))}
for (t in taus) {
  model <- rq(weekPeakWt ~ Precip.tot, data = datLLNpeak, tau = t)
  print(summary(model))}
for (t in taus) {
  model <- rq(perN.weighted ~ weekPeakWt, data = datLLNpeak, tau = t)
  print(summary(model))}





#############################
#make sure to actually load in midsummer

hist(dat.midsummer.merge$X.N, main="Midsummer Nitrogen Distribution")


ggplot(dat.midsummer.merge, aes(x = as.factor(year), y = X.N, fill = as.factor(year))) +
  geom_boxplot() +
  labs(y = "Nitrogen (%)", x = "Year") +
  theme_minimal()

ggplot(dat.midsummer.merge, aes(x = sci_name, y = X.N, fill = sci_name)) +
  geom_boxplot() +
  labs(y = "Nitrogen (%)", x = "Species") +
  theme_minimal()

ggplot(dat.midsummer.merge, aes(x = plot, y = X.N, fill = plot)) +
  geom_boxplot() +
  labs(y = "Nitrogen (%)", x = "Plot") +
  theme_minimal()

ggplot(dat.midsummer.merge, aes(x = plot, y = X.N, fill = sci_name)) +
  geom_boxplot() +
  labs(y = "Nitrogen (%)", x = "Plot") +
  theme_minimal()

ggplot(dat.midsummer.merge, aes(x = sci_name, y = X.N, fill = plot)) +
  geom_boxplot() +
  labs(y = "Nitrogen (%)", x = "Plot") +
  theme_minimal()

ggplot(dat.midsummer.merge, aes(x = as.factor(year), y = X.N, fill = plot)) +
  geom_boxplot() +
  labs(y = "Nitrogen (%)", x = "Year") +
  theme_minimal()

ggplot(dat.midsummer.merge, aes(x = as.factor(year), y = X.N, fill = sci_name)) +
  geom_boxplot() +
  labs(y = "Nitrogen (%)", x = "Year") +
  theme_minimal()


dat.midsummer.merge %>%
  group_by(as.factor(year)) %>%
  summarize(mean = mean(X.N),
            median = median(X.N),
            sd = sd(X.N),
            iqr = IQR(X.N),
            n = n())
dat.midsummer.merge %>%
  group_by(plot) %>%
  summarize(mean = mean(X.N),
            median = median(X.N),
            sd = sd(X.N),
            iqr = IQR(X.N),
            n = n())
dat.midsummer.merge %>%
  group_by(sci_name) %>%
  summarize(mean = mean(X.N),
            median = median(X.N),
            sd = sd(X.N),
            iqr = IQR(X.N),
            n = n())

midsummer_yearN <- aov(X.N ~ as.factor(year), data=dat.midsummer.merge)
summary(midsummer_yearN)

midsummer_plotN <- aov(X.N ~ plot, data=dat.midsummer.merge)
summary(midsummer_plotN)

midsummer_specN <- aov(X.N ~ sci_name, data=dat.midsummer.merge)
summary(midsummer_specN)


summer_mixed1 <- lme(X.N ~ as.factor(year), random=list(sci_name=~1), data=dat.midsummer.merge)
summary(summer_mixed1)
anova(summer_mixed1)

summer_mixed2 <- lme(X.N ~ as.factor(year)*sci_name, random=list(plot=~1), data=dat.midsummer.merge)
summary(summer_mixed2)

summer_mixed3 <- lme(X.N ~ as.factor(year)*plot, random=list(sci_name=~1), data=dat.midsummer.merge)
summary(summer_mixed3)


##Actually calculating Resorption efficiency ----

#first start with species per year

#creating reference "green summer value" for each species per year
year_spec_means <- dat.midsummer.merge %>%
  group_by(year, sci_name) %>%
  summarize(greenN_yearspec = mean(X.N, na.rm = TRUE), .groups = "drop")

#joining to peak litter data
dat_efficiency_yearspec <- datLLNpeak %>%
  left_join(year_spec_means, by = c("year", "sci_name"))
summary(dat_efficiency_yearspec)

#calculate resorption efficiency
dat_efficiency_yearspec <- dat_efficiency_yearspec %>%
  mutate(resorp_eff_yearspec = ((greenN_yearspec - perN.weighted) / greenN_yearspec) * 100)

mosaic::fav_stats(dat_efficiency_yearspec$resorp_eff_yearspec)

#run models with this!
eff_precip_yearspec <- lme(resorp_eff_yearspec ~ Precip.tot, random = list(sci_name = ~1), data = dat_efficiency_yearspec)
summary(eff_precip_yearspec)
anova(eff_precip_yearspec)
r.squaredGLMM(eff_precip_yearspec)

eff_time_yearspec <- lme(resorp_eff_yearspec ~ weekPeakWt, random = list(sci_name = ~1), data = dat_efficiency_yearspec)
summary(eff_time_yearspec)
anova(eff_time_yearspec)
r.squaredGLMM(eff_time_yearspec)

#looking at some plots
ggplot(dat_efficiency_yearspec, aes(x = Precip.tot, y = resorp_eff_yearspec, color = sci_name)) +
  geom_point(aes(color = sci_name), alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", color = "black",
              linewidth = 2, se = FALSE) +
  stat_smooth(aes(color = sci_name, alpha = 0.5), method = "lm",
              geom = "line", linewidth = 0.8, se = FALSE) +
  labs(
    title = "Green Value by Years and Species",
    x = "Total Summer Precipitation (mm)",
    y = "Resorption Efficiency",
    color = "Species",
  ) +
  guides(alpha = "none") +
  theme_minimal()

ggplot(dat_efficiency_yearspec, aes(x = weekPeakWt, y = resorp_eff_yearspec, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  labs(
    title = "Green Value by Years and Species",
    x = "Week of Peak Litterfall",
    y = "Resorption Efficiency",
    color = "Species"
  ) +
  guides(alpha = "none") +
  theme_minimal()

ggplot(dat_efficiency_yearspec, aes(x = weekPeakWt, y = resorp_eff_yearspec, color = sci_name)) +
  geom_point(aes(color = sci_name), alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", color = "black",
              linewidth = 2, se = FALSE) +
  stat_smooth(aes(color = sci_name, alpha = 0.5), method = "lm",
              geom = "line", linewidth = 0.8, se = FALSE) +
  labs(
    title = "Green Value by Years and Species",
    x = "Week of Peak Litterfall",
    y = "Resorption Efficiency",
    color = "Species"
  ) +
  guides(alpha = "none") +
  theme_minimal()


#repeat with a yearly green value

#creating reference "green summer value" for each YEAR
year_N_means <- dat.midsummer.merge %>%
  group_by(year) %>%
  summarize(greenN_year = mean(X.N, na.rm = TRUE), .groups = "drop")

#joining to peak litter data
dat_efficiency_year <- datLLNpeak %>%
  left_join(year_N_means, by = c("year"))
summary(dat_efficiency_year)

#calculate resorption efficiency
dat_efficiency_year <- dat_efficiency_year %>%
  mutate(resorp_eff_year = ((greenN_year - perN.weighted) / greenN_year) * 100)

mosaic::fav_stats(dat_efficiency_year$resorp_eff_year)

#run models with this!
eff_precip_year <- lme(resorp_eff_year ~ Precip.tot, random = list(sci_name = ~1), data = dat_efficiency_year)
summary(eff_precip_year)
anova(eff_precip_year)

eff_time_year <- lme(resorp_eff_year ~ weekPeakWt, random = list(sci_name = ~1), data = dat_efficiency_year)
summary(eff_time_year)
anova(eff_time_year)

#looking at some plots
ggplot(dat_efficiency_year, aes(x = Precip.tot, y = resorp_eff_year, color = sci_name)) +
  geom_jitter(alpha =0.6, width =3) +
  geom_smooth(method ="lm", se=F) +
  labs(
    title = "Yearly Green Value",
    x = "Total Summer Precipitation (mm)",
    y = "Resorption Efficiency",
    color = "Species"
  ) +
  theme_minimal()
ggplot(dat_efficiency_year, aes(x = Precip.tot, y = resorp_eff_year)) +
  geom_jitter(alpha =0.6, width =3) +
  geom_smooth(method ="lm", se=F) +
  labs(
    title = "Yearly Green Value",
    x = "Total Summer Precipitation (mm)",
    y = "Resorption Efficiency"
  ) +
  theme_minimal()


ggplot(dat_efficiency_year, aes(x = weekPeakWt, y = resorp_eff_year, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  labs(
    title = "Yearly Green Value",
    x = "Week of Peak Litterfall",
    y = "Resorption Efficiency",
    color = "Species"
  ) +
  theme_minimal()

ggplot(dat_efficiency_year, aes(x = weekPeakWt, y = resorp_eff_year)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  labs(
    title = "Yearly Green Value",
    x = "Week of Peak Litterfall",
    y = "Resorption Efficiency"
  ) +
  theme_minimal()

#Since we have large inconsistencies with the date of midsummer collection (june 12th-august 31st maybe a species global value will be better)
#creating reference "green summer value" for each SPECIES
spec_N_means <- dat.midsummer.merge %>%
  group_by(sci_name) %>%
  summarize(greenN_spec = mean(X.N, na.rm = TRUE), .groups = "drop")

#joining to peak litter data
dat_efficiency_spec <- datLLNpeak %>%
  left_join(spec_N_means, by = c("sci_name"))
summary(dat_efficiency_spec)

#calculate resorption efficiency
dat_efficiency_spec <- dat_efficiency_spec %>%
  mutate(resorp_eff_spec = ((greenN_spec - perN.weighted) / greenN_spec) * 100)

mosaic::fav_stats(dat_efficiency_spec$resorp_eff_spec)

#run models with this!
eff_precip_spec <- lme(resorp_eff_spec ~ Precip.tot, random = list(plot =~1, sci_name = ~1), data = dat_efficiency_spec)
summary(eff_precip_spec)
anova(eff_precip_spec)
r.squaredGLMM(eff_precip_spec)

eff_time_spec <- lme(resorp_eff_spec ~ weekPeakWt, random = list(plot =~1, sci_name = ~1), data = dat_efficiency_spec)
summary(eff_time_spec)
anova(eff_time_spec)
r.squaredGLMM(eff_time_spec)

#looking at some plots
ggplot(dat_efficiency_spec, aes(x = Precip.tot, y = resorp_eff_spec, color = sci_name)) +
  geom_jitter(alpha =0.6, width =3) +
  geom_smooth(method ="lm", se=F) +
  labs(
    title = "Species Green Value",
    x = "Total Summer Precipitation (mm)",
    y = "Resorption Efficiency",
    color = "Species"
  ) +
  theme_minimal()

ggplot(dat_efficiency_spec, aes(x = weekPeakWt, y = resorp_eff_spec, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  labs(
    title = "Species Green Value",
    x = "Week of Peak Litterfall",
    y = "Resorption Efficiency",
    color = "Species"
  ) +
  theme_minimal()


###change this code to have a version of this with the desired resorption efficiency
# datDroughteff <- datLLNpeak%>%
#   pivot_longer(cols = c(Precip.tot, VPD.avg, n.Rainless, RainlessConsec.max), 
#                names_to = "Drought_Metric", values_to = "Metric_Value") %>%
#   mutate(Drought_Metric = case_match(Drought_Metric,
#                                      "Precip.tot" ~ "Total Summer Precipitation",
#                                      "VPD.avg" ~ "Average VPD",
#                                      "n.Rainless"  ~ "# of Rainless Days",
#                                      "RainlessConsec.max" ~ "# of Consecutive\n Rainless Days"))
# 
# ggplot(datDrought, aes(x = Metric_Value, y = perN.weighted, color = sci_name)) +
#   facet_wrap(~Drought_Metric, scales = "free_x") +
#   geom_jitter(alpha = 0.5) +
#   stat_smooth(method = "lm", se=F) +
#   labs(x = "Drought Metric Value Value",
#        y = "Nitrogen Percentage\n in Leaf Litter (%)",
#        color = "Species") +
#   theme_minimal()

