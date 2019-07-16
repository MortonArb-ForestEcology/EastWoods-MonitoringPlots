polar2cart<-function(x,y,dist,bearing,as.deg=FALSE){
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

survey.2019 <- googlesheets::gs_title("Plot Expansion 2019")
dat.2019 <- data.frame(googlesheets::gs_read(survey.2019, ws="raw data"))
# dat.2019$bearing_to_tree <- dat.2019$azimuth_to_ref-180
dat.2019$x_tree2 <- sin(dat.2019$bearing_to_tree*pi/180)*dat.2019$distance_to_ref+dat.2019$reference_x
dat.2019$y_tree2 <- cos(dat.2019$bearing_to_tree*pi/180)*dat.2019$distance_to_ref+dat.2019$reference_y
summary(dat.2019)
head(dat.2019)

dat.2019$y_tree2[dat.2019$IMLS_Plot=="U134"] <- dat.2019$y_tree2[dat.2019$IMLS_Plot=="U134"]-10

path.figs <- "/Volumes/GoogleDrive/My Drive/East Woods/Rollinson_Monitoring/Data/TreeSurvey/"
library(ggplot2)

png(file.path(path.figs, "PlotExpansion2019_TreeLocations.png"), height=8, width=8, units="in", res=180)
ggplot(data=dat.2019) +
  coord_equal() +
  facet_wrap(~IMLS_Plot) +
  # annotate("rect", xmin=-10, xmax=10, ymin=-10, ymax=10, fill="blue", alpha=0.5) +
  # annotate("rect", xmin=-20, xmax=20, ymin=-20, ymax=20, color="red", fill=NA, size=2) +
  geom_rect(data=data.frame(IMLS_Plot=c("B127", "N115", "U134", "HH115")), xmin=-10, xmax=10, ymin=-10, ymax=10, fill="blue", alpha=0.5) +
  geom_rect(data=data.frame(IMLS_Plot=c("B127", "N115", "U134")), xmin=-20, xmax=20, ymin=-20, ymax=20, color="red", fill=NA, size=2) +
  geom_rect(data=data.frame(IMLS_Plot=c("HH115")), xmin=-10, xmax=20, ymin=-20, ymax=20, color="red", fill=NA, size=2) +
  geom_point(aes(x=x_tree, y=y_tree, size=DBH)) + 
  theme_bw()
dev.off()
