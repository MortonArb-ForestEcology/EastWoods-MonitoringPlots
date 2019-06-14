#Reading in data

library(googlesheets)
ll.df <- gs_title("Leaf_Litter_Data")
dat.ll <- data.frame(gs_read(ll.df, ws="raw_data"))

#Setting to factors
for(i in 1:ncol(dat.ll)){
  if(class(dat.ll[,i])=="character") dat.ll[,i] <- as.factor(dat.ll[,i])
}
summary(dat.ll)


#Some initial information
nrow(dat.ll) # Number of observations
range(dat.ll$date_collection) # Date range (note: incomplete at moment)
length(unique(dat.ll$species)) # Number of species observed (assuming no typos)


#Stats and data wrangling packages
library(ggplot2); library(dplyr); library("ggpubr")



#Plotting raw total mass of all tissues by species
png(file = "~/GitHub/EastWoods-MonitoringPlots/LeafLitter/Figures//LL_TotalMassAllTissue_BySpecies_IncludesOutliers.png")
ggboxplot(dat.ll, x = "plot", y = "mass_g", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Mass (g)", xlab = "Plot", 
          title = "(Raw) Total Mass of All Tissue Sep2018-Jan2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "free")
dev.off()

#Mean, SD, and ANOVA (will need to change ANOVA) for mass across plots
tapply(dat.ll$mass_g, dat.ll$plot, mean)
tapply(dat.ll$mass_g, dat.ll$plot, sd)
res.aov <- aov(mass_g ~ plot, data = dat.ll)
summary(res.aov)


#Getting rid of huge outlier of heavy chunk of ash bark
library(data.table)
outlierReplace = function(dat.ll, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dat.ll, rows, cols, newValue)
  }
}
outlierReplace(dat.ll, "mass_g", which(dat.ll$mass_g > 50), NA)


#Replotting after outlier removal

png("~/GitHub/EastWoods-MonitoringPlots/LeafLitter/Figures//LL_TotalMassofAllTissueByPlot.png")
ggboxplot(dat.ll, x = "plot", y = "mass_g", 
          color = "plot",
          ylab = "Mass (g)", xlab = "Plot",
          title = "(Raw) Total Mass By Plot Sept2018-Jan2019",
          legend = "none")+
  theme(plot.title = element_text(hjust=0.5))
dev.off()

png("~/GitHub/EastWoods-MonitoringPlots/LeafLitter/Figures//LL_TotalMassAllTissue_BySpecies_DiscardOutliers.png")
ggboxplot(dat.ll, x = "plot", y = "mass_g", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Mass (g)", xlab = "Plot", 
          title = "(Raw) Total Mass of All Tissue Sep2018-Jan2019",
          legend = "right") +
  facet_wrap(~plot, scales = "free")+
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_blank())
dev.off()



png("~/GitHub/EastWoods-MonitoringPlots/LeafLitter/Figures//LL_TotalMassofAllTissueByTissueType.png")
ggboxplot(dat.ll, x = "plot", y = "mass_g", 
          color = "tissue",
          ylab = "Mass (g)", xlab = "Plot",
          facet.by = "plot",
          title = "(Raw) Total Mass By Tissue Type Sept2018-Jan2019",
          legend = "right")+
  facet_wrap(~plot, scales = "free")+
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_blank()
  )  
dev.off()

tapply(dat.ll$mass_g, dat.ll$plot, mean)
tapply(dat.ll$mass_g, dat.ll$plot, sd)
res.aov <- aov(mass_g ~ plot, data = dat.ll)
summary(res.aov)


#Subsetting for leaf only

dat.ll.leaf <- dat.ll[dat.ll$tissue=="leaf",]

summary(dat.ll.leaf)

png("~/GitHub/EastWoods-MonitoringPlots/LeafLitter/Figures//LL_TotalMass_LeavesOnly.png")
ggboxplot(dat.ll.leaf, x = "taxon", y = "mass_g", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Mass (g)", xlab = "Species",
          title = "(Raw) Total Leaf Tissue Mass By Species Sept 2018- Jan 2019 ",
          legend = "none") + 
  theme(axis.text.x = element_text(angle=70, hjust=1, size = 10))+
  theme(plot.title = element_text(hjust=0.5)) +
  facet_wrap(~plot, scales = "free")
dev.off()
          
tapply(dat.ll.leaf$mass_g, dat.ll.leaf$plot, mean)
tapply(dat.ll.leaf$mass_g, dat.ll.leaf$plot, sd)
res.aov <- aov(mass_g ~ plot, data = dat.ll.leaf)
summary(res.aov)

png("~/GitHub/EastWoods-MonitoringPlots/LeafLitter/Figures//LL_LeavesOnly_PlotComparison.png")
ggboxplot(dat.ll.leaf, x = "plot", y = "mass_g", 
          color = "plot",
          ylab = "Mass (g)", xlab = "Plot",
          title = "(Raw) Total Leaf Mass By Plot Sept2018-Jan2019",
          legend = "none")+
  theme(plot.title = element_text(hjust=0.5))
dev.off()


