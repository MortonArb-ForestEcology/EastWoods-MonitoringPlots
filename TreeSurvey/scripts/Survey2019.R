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
dat.2019$x_offset2 <- sin(dat.2019$bearing_to_tree*pi/180)*dat.2019$distance_to_ref
dat.2019$x_tree <- sin(dat.2019$bearing_to_tree*pi/180)*dat.2019$distance_to_ref+dat.2019$reference_x
dat.2019$y_tree <- cos(dat.2019$bearing_to_tree*pi/180)*dat.2019$distance_to_ref+dat.2019$reference_y
summary(dat.2019)

library(ggplot2)
ggplot(data=dat.2019) +
  coord_equal() +
  facet_wrap(~IMLS_Plot) +
  annotate("rect", xmin=-10, xmax=10, ymin=-10, ymax=10, fill="red", alpha=0.5) +
  annotate("rect", xmin=-20, xmax=20, ymin=-20, ymax=20, fill="blue", alpha=0.5) +
  geom_point(aes(x=x_tree, y=y_tree, size=DBH))
