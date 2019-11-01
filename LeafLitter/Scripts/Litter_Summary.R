#Script for summarizing leaf litter data. Largely cannibalized from Bethany
library(googlesheets)
library(ggplot2)
library(dplyr)
library(ggpubr)

#grabbing the file from google drive and making it a workable data frame
litter.df <- gs_title("Leaf_Litter_Data")
dat.lit <- data.frame(gs_read(litter.df, ws="raw_data"))

for(i in 1:ncol(dat.lit)){
  if(class(dat.lit[,i])=="character") dat.lit[,i] <- as.factor(dat.lit[,i])
}

str(dat.lit)

dat.lit <- dat.lit %>% mutate(mass.sqrt = sqrt(mass_g))
dat.lit <- dat.lit %>% mutate(month = month(date_collection))

#--------------------------#
ggboxplot(dat.lit, x = "plot", y = "mass_g", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Mass (g)", xlab = "Plot", 
          title = "(Raw) Total Mass of All Tissue Sep2018-Aug2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "free")

#box plot with mass squared to look at outliers


ggboxplot(dat.lit, x = "plot", y = "mass.sqrt", 
          color = "taxon",
          facet.by = "plot",
          ylab = "Sqaure root of Mass (g)", xlab = "Plot", 
          title = "(Raw) Sqaure root of Mass of All Tissue Sep2018-Aug2019",
          legend = "right") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust=0.5))+
  facet_wrap(~plot, scales = "free")

#Mean, SD, and ANOVA (will need to change ANOVA) for mass across plots
tapply(dat.lit$mass_g, dat.lit$plot, mean)
tapply(dat.lit$mass_g, dat.lit$plot, sd)
res.aov <- aov(mass_g ~ plot, data = dat.lit)
summary(res.aov)

#Getting rid of huge outlier of heavy chunk of ash bark
library(data.table)
outlierReplace = function(dat.lit, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dat.lit, rows, cols, newValue)
  }
}
outlierReplace(dat.lit, "mass_g", which(dat.lit$mass_g > 50), NA)

#Total mass of all tissue by facet by species

for(i in 1:ncol(dat.lit)){
  if(class(dat.lit[,i])=="factor") dat.lit[,i] <- as.character(dat.lit[,i])
}

dat.stack <- stack(dat.lit[,c("mass_g", "tissue", "mass.sqrt")])
names(dat.stack) <- c("values", "var")
dat.stack[,c("plot","taxon","date_collection", "month")] <- dat.lit[,c("plot","taxon","date_collection")]
summary(dat.stack)

ggplot(dat.stack, aes(x=plot, y = values))+
  facet_wrap(~var, scales = "free_y")+
  geom_jitter(aes(color=taxon))

#graph to see distribution of tissues in different plots
ggplot(dat.lit, aes(x=plot, y=tissue))+
  geom_jitter(aes(color=genus))+
  ggtitle("Types of tissue in different plots")

ggplot(dat.lit, aes(x=plot, y=mass.sqrt))+
  geom_jitter(aes(color=genus))+
  ggtitle("Mass in different plots")

#----------------------------#
#Working with just oaks
dat.oaks <- subset(dat.lit, genus == "Quercus")

dat.oakstack <- stack(dat.oaks[,c("mass_g", "tissue", "mass.sqrt")])
names(dat.oakstack) <- c("values", "var")
dat.oakstack[,c("plot","taxon","date_collection", "month")] <- dat.oaks[,c("plot","taxon","date_collection", "month")]
summary(dat.oakstack)

ggplot(dat.oaks, aes(x=plot, y=tissue))+
  geom_jitter(aes(color=species))+
  ggtitle("Types of oak tissue in different plots")

ggplot(dat.oaks, aes(x=plot, y=mass.sqrt))+
  geom_boxplot(aes(color=species))+
  ggtitle("Mass in different plots")

ggplot(dat.oakstack, aes(x=month, y = values))+
  facet_wrap(~var, scales = "free_y")+
  geom_jitter(aes(color=plot))

ggplot(dat.lit, aes(x=month, y=tissue))+
  facet_wrap(~plot, scales = "free_y")+
  geom_jitter(aes(color=genus))+
  ggtitle("Types of oak tissue in different plots")

          

