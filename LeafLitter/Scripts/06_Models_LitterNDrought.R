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

###########################################################
#2: Stoich EDA  ----
ggplot(datLLNmerge, aes(x = yday, y = X.N, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="loess", se=F) +
  facet_wrap(~year) + 
  coord_cartesian(xlim=c(270,340)) +
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

dat_no_outlier <- datLLNpeak[-13, ]

NPrecip_no_outlier <- lme(perN.weighted ~ Precip.tot, random = list(sci_name = ~1, plot = ~1), data = dat_no_outlier)
summary(NPrecip_no_outlier)
r.squaredGLMM(NPrecip_no_outlier)

NTime_no_outlier <- lme(perN.weighted ~ weekPeakWt, random = list(sci_name = ~1, plot = ~1), data = dat_no_outlier)
summary(NTime_no_outlier)
r.squaredGLMM(NTime_no_outlier)

ggplot(datLLNpeak, aes(x = weekPeakWt, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~year) + 
  #coord_cartesian(xlim=c(270,340)) +
  theme_minimal()

ggplot(dat_no_outlier, aes(x = Precip.tot, y = perN.weighted, color = sci_name)) +
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
#NPrecip_forest <- lme(perN.weighted ~ Precip.tot, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
NPrecip_forest <- lme(perN.weighted ~ Precip.tot, random=list(sci_name=~1, plot=~1), data=dat_no_outlier)
summary(NPrecip_forest)
anova(NPrecip_forest)
r.squaredGLMM(NPrecip_forest)

sppNPrecip <- lme(perN.weighted ~ Precip.tot*sci_name, random=list(plot=~1), data=datLLNpeak)
summary(sppNPrecip)
anova(sppNPrecip)
r.squaredGLMM(sppNPrecip)

#DropPrecip <- lme(weekPeakWt ~ Precip.tot, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
DropPrecip <- lme(weekPeakWt ~ Precip.tot, random=list(sci_name=~1, plot=~1), data=dat_no_outlier)
summary(DropPrecip)
anova(DropPrecip)
r.squaredGLMM(DropPrecip)

spDropPrecip <- lme(weekPeakWt ~ Precip.tot * sci_name, random=list(plot=~1), data = )
summary(spDropPrecip)
anova(spDropPrecip)
r.squaredGLMM(spDropPrecip)

#TimeN <- lme(perN.weighted ~ weekPeakWt, random=list(sci_name=~1, plot=~1), data=datLLNpeak)
TimeN <- lme(perN.weighted ~ weekPeakWt, random=list(sci_name=~1, plot=~1), data=dat_no_outlier)
summary(TimeN)
anova(TimeN)
r.squaredGLMM(TimeN)




NPrecip_year <- lme(perN.weighted ~ Precip.tot, random=list(year=~1, sci_name=~1, plot=~1), data=dat_no_outlier)
summary(NPrecip_year)
anova(NPrecip_year)
r.squaredGLMM(NPrecip_year)

DropPrecip_year <- lme(weekPeakWt ~ Precip.tot, random=list(year=~1, sci_name=~1, plot=~1), data=dat_no_outlier)
summary(DropPrecip_year)
anova(DropPrecip_year)
r.squaredGLMM(DropPrecip_year)

TimeN_year <- lme(perN.weighted ~ weekPeakWt, random=list(year=~1, sci_name=~1, plot=~1), data=dat_no_outlier)
summary(TimeN_year)
anova(TimeN_year)
r.squaredGLMM(TimeN_year)


####################################################################
#Making graphs for collaborator meeting

ggplot(dat_no_outlier, aes(x = Precip.tot, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()

ggplot(dat_no_outlier, aes(x = Precip.tot, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~plot) +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()



ggplot(dat_no_outlier, aes(x = Precip.tot, y = weekPeakWt, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Week of Peak Litterfall",
    color = "Species"
  ) +
  theme_minimal()

ggplot(dat_no_outlier, aes(x = Precip.tot, y = weekPeakWt, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~plot) +
  labs(
    x = "Total Summer Precipitation (mm)",
    y = "Week of Peak Litterfall",
    color = "Species"
  ) +
  theme_minimal()



ggplot(dat_no_outlier, aes(x = weekPeakWt, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  labs(
    x = "Week of Peak Litterfall",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()

ggplot(dat_no_outlier, aes(x = weekPeakWt, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~plot) +
  labs(
    x = "Week of Peak Litterfall",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()

ggplot(dat_no_outlier, aes(x = weekPeakWt, y = perN.weighted, color = sci_name)) +
  geom_point(alpha =0.6) +
  geom_smooth(method ="lm", se=F) +
  facet_wrap(~year) +
  labs(
    x = "Week of Peak Litterfall",
    y = "Nitrogen Percentage\n in Leaf Litter (%)",
    color = "Species"
  ) +
  theme_minimal()
