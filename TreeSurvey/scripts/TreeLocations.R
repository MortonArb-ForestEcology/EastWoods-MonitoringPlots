library(ggplot2)
path.figs <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/TreeSurvey/"
dir.exists(path.figs)

polar2cart<-function(x, y , dist, bearing, as.deg=FALSE){
  ## Translate Polar coordinates into Cartesian coordinates
  ## based on starting location, distance, and bearing
  ## as.deg indicates if the bearing is in degrees (T) or radians (F)
  
  if(as.deg){
    ##if bearing is in degrees, convert to radians
    bearing=bearing*pi/180
  }
  
  newx<-x+dist*sin(bearing)  ##X
  newy<-y+dist*cos(bearing)  ##Y
  return(list("x"=newx,"y"=newy))
}

treeSS <- "1Cvkvf5oklMVM1gB4AMDd1KDw04WJwbI6irOo31L2RCk" # Key for "Tree_PlotSurvey_Master
treePts <- googlesheets4::read_sheet(ss=treeSS, sheet="raw data")
treePts$Vigor <- as.ordered(treePts$Vigor)
treePts$IMLS_Plot <- as.factor(treePts$IMLS_Plot)
treePts$Sp_code <- as.factor(treePts$Sp_code)
treePts$Canopy <- as.factor(treePts$Canopy)
summary(treePts)

# The extend plot data needs to be re-mapped.  Thse are alllook pretty darn wonky for N115 & U-134!
ss2019 <- "15HO4VHQ0OMm5KAKM0ZhdWQPdMKRdJIAZAuRFWTQo-78" # "Plot Expansion 2019"
dat2019 <- googlesheets4::read_sheet(ss=ss2019, sheet="raw data")
# # dat2019$bearing_to_tree <- dat2019$azimuth_to_ref-180
dat2019$Vigor <- as.ordered(dat2019$Vigor)
dat2019$IMLS_Plot <- as.factor(dat2019$IMLS_Plot)
dat2019$Sp_code <- as.factor(dat2019$Sp_code)
dat2019$Canopy <- as.factor(dat2019$Canopy)
dat2019$reference_x <- dat2019$reference_x+10 # Our other dataset has the SW corner as 0,0
dat2019$reference_y <- dat2019$reference_y+10 # Our other dataset has the SW corner as 0,0
# 
# # U-134 also seems to have an additional weirdness on its y coord
dat2019$reference_y[dat2019$IMLS_Plot=="U-134"] <- dat2019$reference_y[dat2019$IMLS_Plot=="U-134"]-10
# 
dat2019$x_tree2 <- sin(dat2019$bearing_to_tree*pi/180)*dat2019$distance_to_ref+dat2019$reference_x
dat2019$y_tree2 <- cos(dat2019$bearing_to_tree*pi/180)*dat2019$distance_to_ref+dat2019$reference_y
summary(dat2019)
# # head(dat2019)


png(file.path(path.figs, "EastWoods_MonitoringPlots_TreeLocations-All.png"), height=8, width=8, units="in", res=180)
ggplot(data=treePts) +
  coord_equal(xlim=c(-15, 35), ylim=c(-15, 35)) +
  facet_wrap(~IMLS_Plot) +
  geom_rect(data=data.frame(IMLS_Plot=c("B-127", "N-115", "U-134", "HH-115")), xmin=0, xmax=20, ymin=0, ymax=20, color="blue", fill="blue", alpha=0.2, linewidth=2) +
  geom_rect(data=data.frame(IMLS_Plot=c("B-127", "N-115", "U-134")), xmin=-10, xmax=30, ymin=-10, ymax=30, color="red", fill=NA, size=2) +
  geom_rect(data=data.frame(IMLS_Plot=c("HH-115")), xmin=0, xmax=30, ymin=-10, ymax=30, color="red", fill=NA, size=2) +
  geom_point(aes(x=X, y=Y, size=DBH)) + 
  geom_point(data=dat2019, aes(x=x_tree2, y=y_tree2, size=DBH)) + 
  theme_bw()
dev.off()

png(file.path(path.figs, "EastWoods_MonitoringPlots_TreeLocations-DomCoDom.png"), height=8, width=8, units="in", res=180)
ggplot(data=treePts[treePts$Canopy %in% c("C", "CD", "D") & !is.na(treePts$Canopy) & !is.na(treePts$Vigor) & treePts$Vigor<=2,]) +
  coord_equal(xlim=c(-15, 35), ylim=c(-15, 35)) +
  facet_wrap(~IMLS_Plot) +
  geom_rect(data=data.frame(IMLS_Plot=c("B-127", "N-115", "U-134", "HH-115")), xmin=0, xmax=20, ymin=0, ymax=20, color="blue", fill="blue", alpha=0.2, linewidth=2) +
  geom_rect(data=data.frame(IMLS_Plot=c("B-127", "N-115", "U-134")), xmin=-10, xmax=30, ymin=-10, ymax=30, color="red", fill=NA, size=2) +
  geom_rect(data=data.frame(IMLS_Plot=c("HH-115")), xmin=0, xmax=30, ymin=-10, ymax=30, color="red", fill=NA, size=2) +
  geom_point(aes(x=X, y=Y, size=DBH, color=Sp_code)) + 
  geom_point(data=dat2019[dat2019$Canopy %in% c("C", "CD", "D") & !is.na(dat2019$Canopy) & !is.na(dat2019$Vigor) & dat2019$Vigor<=2,], aes(x=x_tree2, y=y_tree2, size=DBH, color=Sp_code)) + 

  scale_color_manual(values=c("ACSA"="purple3", "JUNI"="rosybrown4", "PRSE" = "rosybrown3", "QUAL"="violetred1", "QUMA"="violetred2", "QURU"="violetred3", "TIAM"="rosybrown2", "OSVI"="wheat2", "ULRU"="wheat3")) +
  theme_bw()
dev.off()


png(file.path(path.figs, "EastWoods_MonitoringPlots_TreeLocations-DomCoDom_AcerQuercus.png"), height=8, width=8, units="in", res=180)
ggplot(data=treePts[treePts$Canopy %in% c("C", "CD", "D") & !is.na(treePts$Canopy) & !is.na(treePts$Vigor) & treePts$Vigor<=2 & treePts$Sp_code %in% c("ACSA", "QUAL", "QUMA", "QURU"),]) +
  coord_equal(xlim=c(-15, 35), ylim=c(-15, 35)) +
  facet_wrap(~IMLS_Plot) +
  geom_rect(data=data.frame(IMLS_Plot=c("B-127", "N-115", "U-134", "HH-115")), xmin=0, xmax=20, ymin=0, ymax=20, color="blue", fill="blue", alpha=0.2, linewidth=2) +
  geom_rect(data=data.frame(IMLS_Plot=c("B-127", "N-115", "U-134")), xmin=-10, xmax=30, ymin=-10, ymax=30, color="red", fill=NA, size=2) +
  geom_rect(data=data.frame(IMLS_Plot=c("HH-115")), xmin=0, xmax=30, ymin=-10, ymax=30, color="red", fill=NA, size=2) +
  geom_point(aes(x=X, y=Y, size=DBH, color=Sp_code)) + 
  geom_point(data=dat2019[dat2019$Canopy %in% c("C", "CD", "D") & !is.na(dat2019$Canopy) & !is.na(dat2019$Vigor) & dat2019$Vigor<=2 &  dat2019$Sp_code %in% c("ACSA", "QUAL", "QUMA", "QURU"),], aes(x=x_tree2, y=y_tree2, size=DBH, color=Sp_code)) + 
  
  scale_color_manual(values=c("ACSA"="purple3", "JUNI"="rosybrown4", "PRSE" = "rosybrown3", "QUAL"="violetred1", "QUMA"="violetred2", "QURU"="violetred3", "TIAM"="rosybrown2", "OSVI"="wheat2", "ULRU"="wheat3")) +
  theme_bw()
dev.off()
