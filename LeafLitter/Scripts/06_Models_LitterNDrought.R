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

CPrecip_forest <- lme(perC.weighted ~ Precip.tot, random=list(plot=~1), data=datLLNpeak)
summary(CPrecip_forest)
anova(CPrecip_forest)
r.squaredGLMM(CPrecip_forest)
shapiro.test(resid(CPrecip_forest))
qqnorm(resid(CPrecip_forest))

#other models just to have in one place


#############################
#3: Selecting drought variables (thanks leah for code inspo)
Nprecip <- lme(perN.weighted ~ Precip.tot, random=list(plot=~1), data=datLLNpeak)
NVPD<- lme(perN.weighted ~ VPD.avg, random=list(plot=~1), data=datLLNpeak)
Nrainless <- lme(perN.weighted ~ n.Rainless, random=list(plot=~1), data=datLLNpeak)
Nrainlessconsec <- lme(perN.weighted ~ RainlessConsec.max, random=list(plot=~1), data=datLLNpeak)
Nprecipday <- lme(perN.weighted ~ prcp..mm.day., random=list(plot=~1), data=datLLNpeak)

get_stats <- function(model, name) {
  r2 <- r.squaredGLMM(model)
  data.frame(
    Model = name,
    AIC = AIC(model),
    R2_Marginal = r2[1],
    R2_Conditional = r2[2])}

DropPrecip <- lme(weekPeakWt ~ Precip.tot, random=list(plot=~1), data=weekPeakSpp)
DropVPD <- lme(weekPeakWt ~ VPD.avg, random=list(plot=~1), data=weekPeakSpp)
Droprainless <- lme(weekPeakWt ~ n.Rainless, random=list(plot=~1), data=weekPeakSpp)
Droprainlessconsec <- lme(weekPeakWt ~ RainlessConsec.max, random=list(plot=~1), data=weekPeakSpp)
Dropprecipday <- lme(weekPeakWt ~ prcp..mm.day., random=list(plot=~1), data=datLLNpeak)


Ncombined <- lme(perN.weighted ~ Precip.tot + RainlessConsec.max, 
                  random = list(plot = ~1), 
                  data = datLLNpeak)
summary(Ncombined)
r.squaredGLMM(Ncombined)

Dropcombined <- lme(weekPeakWt ~ Precip.tot + RainlessConsec.max, 
                                  random = list(plot = ~1), 
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
