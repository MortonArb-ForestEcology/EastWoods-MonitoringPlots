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


#1: Load data and verify variables
datLLNpeak <- read.csv(file.path(path.REU, "LeafLitter_peak_integrated_TimingNDrought.csv"))
#^^all weeks collapsed into a single annual value per plot (weighted peak week)
summary(datLLNpeak)
str(datLLNpeak)

datLLNmerge <- read.csv(file.path(path.REU, "LeafLitter_combined_TimingN.csv"))
#^^maintains individual entries for each collection
summary(datLLNmerge)
str(datLLNmerge)

###########################################################

#2: Stoich
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

NPrecip_forest <- lme(perN.weighted ~ Precip.tot, random=list(plot=~1), data=datLLNpeak)
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
#3: Selecting drought variables (thanks leah for code inspo)
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
                                  data = weekPeakSpp)
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
#4: Structural Equations Model

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

spec_SEM <- psem(spDropPrecip, spNPrecip_linked)
summary(spec_SEM, standardize = "none")


