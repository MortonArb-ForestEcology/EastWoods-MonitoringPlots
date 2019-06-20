# ----------------------------------------
# Script to map which trees in the Oak Collection have dendrometer bands
# Christy Rollinson, crollinson@mortonarb.org
# 3 April, 2017

# Goal:
# 1. Make maps to help phenology observation volunteers find their trees
# 2. Make maps showing the phenological status of the oak collection (goal: update weekly)
# ----------------------------------------



# ----------------------------------------
# Load libraries, base datasets, assign file paths, etc.
# ----------------------------------------
# ---------------------
# Loading useful libraries
# ---------------------
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages
library(googlesheets)
# ---------------------

# ---------------------
# Setting File paths
# ---------------------
dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Dendrobands/"
# setwd(dir.base)


path.dat <- file.path(dir.base, "Data/Oak Collection")
maps.out <- file.path(dir.base, "figures/maps_dendrometers")
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
# ---------------------

# ---------------------
# Loading GIS layers
# # Note: Must be connected to GIS server for this to work
# ---------------------
# Trees! 2016 is the most recent I could find
# Note: there's a LOT of info I don't want
# db.cols <- c("sci_name", "sort_scina", "sci_nm3", "type", "trade_nm", "plant_id", "habitat", "coll_id", "coll_nm", "collsuba", "grid_loc", "x_coord", "y_coord")
# trees <- readOGR("/Volumes/GIS/Collections/PLANTDB/HDB_2015-05-12.shp")
# summary(trees[,db.cols])
# names(trees)
# plot(trees)


#Collection Boundaries
collections <- readOGR("/Volumes/GIS/Collections/Collections_outlines/coll_bndry_master_plan.shp")
summary(collections)

# Morton Grid system
# I *think* this is a 100-foot grid (30.48 m)
morton.grid <- readOGR("/Volumes/GIS/Collections/grid_system/adjustedgrid.shp")
class(morton.grid)
# plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray20")

# Water, streams
water <- readOGR("/Volumes/GIS/Collections/hydrology/water.shp")
pools <- readOGR("/Volumes/GIS/Collections/hydrology/wat_pools_seasonal.shp")
streams.perm <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_permanent.shp")
streams.int <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_seasonal.shp")

# Roads, Trails
roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_2011-2020_ctrln.shp")
paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
parking <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/parking_lots.shp")
# summary(roads)
# summary(paths)

# Woodland boundarys
woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
woods <- woods[2,] # We only want to worry about the main block; row 1 = King's Grove, row 2= main tract; row 3 = weird patch

# The start point was found by trial and error
grid.labs.x <- data.frame(grid.x=seq(323102, by=30.5, length.out=length(89:107)), grid.y=571230, x.lab=89:107)
grid.labs.y <- data.frame(grid.x=323102-30.5, grid.y=seq(571227+30.5, by=30.5, length.out=length(3:15)), y.lab=LETTERS[seq(from=3, to=15)])

# summary(grid.labs.x)
labs.x <- SpatialPointsDataFrame(coords = grid.labs.x[,c("grid.x", "grid.y")], grid.labs.x, proj4string=CRS(projection(roads)))
labs.y <- SpatialPointsDataFrame(coords = grid.labs.y[,c("grid.x", "grid.y")], grid.labs.y, proj4string=CRS(projection(roads)))

# plot(morton.grid)
# plot(labs.x, add=T, col="blue")
# plot(labs.y, add=T, col="blue")
# plot(labs.y, add=T)
# ---------------------



# ---------------------
# Loading Tree Data & Metadata
# ---------------------
oaks.all <- read.csv("/Volumes/GoogleDrive/My Drive/Morton_Data_Misc/2018-03-02_151758630-BRAHMSOnlineData.csv")
summary(oaks.all)

# band.trees <- read.csv("data/dendrometers/Dendroband_Installation.csv")
# https://drive.google.com/open?id=1CtcZeEKoaQdLOwxb-9kpqpWgeCf1AziWYt9UT5YH31M
# sheet.band <- gs_title("DendrobandObservations_OakCollection")
sheet.band <- gs_key("1CtcZeEKoaQdLOwxb-9kpqpWgeCf1AziWYt9UT5YH31M")
# gs_ls()
sheet.band

# get the data from a particular sheet
dat.band <- data.frame(gs_read(sheet.band, ws="Oak Collection"))
summary(dat.band)

for(i in 1:ncol(dat.band)){
  if(class(dat.band[,i])=="character") dat.band[,i] <- as.factor(dat.band[,i])
}
summary(dat.band)

# ---------------------

# ----------------------------------------

# ----------------------------------------
# Subsetting and graphing
# ----------------------------------------

# Transforming our datalayers to lat/lon to mesh with the tree data
woods <- spTransform(woods, CRS("+proj=longlat"))
roads <- spTransform(roads, CRS("+proj=longlat"))
paths <- spTransform(paths, CRS("+proj=longlat"))
morton.grid <- spTransform(morton.grid, CRS("+proj=longlat"))
labs.x <- spTransform(labs.x, CRS("+proj=longlat"))
labs.y <- spTransform(labs.y, CRS("+proj=longlat"))

labs.x <- data.frame(labs.x)
labs.y <- data.frame(labs.y)
names(labs.x)[4:5] <- c("long", "lat")
names(labs.y)[4:5] <- c("long", "lat")
summary(labs.x)

# Make things faster by only plotting part of the grid (since that's what takes forever)
extent.map <- c(range(labs.x$long, na.rm=T)+c(0.0002,-0.0002), range(labs.y$lat, na.rm=T)+c(0, 0.00005))
grid.crop <- crop(morton.grid, extent.map)
# coord_equal(xlim=range(labs.x$long, na.rm=T)+c(0.0002,-0.0002), ylim=range(labs.y$lat, na.rm=T)+c(0, 0.00005)) +
  

png(file.path(maps.out, paste0("Map_Dendrometers_OakCollection_", Sys.Date(), ".png")), height=8, width=11, unit="in", res=220)
ggplot() +
  coord_equal(xlim=extent.map[1:2], ylim=extent.map[3:4]) +
  geom_polygon(data=woods, aes(x=long, y=lat, group=group), fill="darkgreen", alpha=0.5) +
  geom_path(data=roads[roads$name=="main route east side", ], aes(x=long, y=lat, group=group), size=5, color="gray80") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=3, linetype="dashed", color="brown") +
  # geom_path(data=morton.grid, aes(x=long, y=lat, group=group), size=0.5, linetype="dotted", color="gray30") +
  geom_path(data=grid.crop, aes(x=long, y=lat, group=group), size=0.5, linetype="dotted", color="gray30") +
  geom_point(data=oaks.all, aes(x=BgLongitude, y=BgLatitude), color="black", size=1.5) +
  geom_point(data=oaks.all[oaks.all$PlantNumber %in% dat.band$id,], aes(x=BgLongitude, y=BgLatitude), color="green4", size=10) +
  geom_text(data=labs.x[2:nrow(labs.x),], aes(x=long, y=lat+0.0002, label=x.lab), color="black", fontface="bold") +
  geom_text(data=labs.y[2:nrow(labs.y),], aes(x=long+0.0005, y=lat, label=y.lab), color="black", fontface="bold") +
  # scale_color_manual(values=c("gray50", "darkolivegreen3", "green4")) +
  ggtitle(paste0("The Morton Arboretum\nLocation of Dendrometer Bands: Oak Collection")) +
  # labs(x="x (meters)", y="y (meters)") +
  theme(panel.grid=element_blank(),
        plot.background=element_blank(),
        panel.background=element_blank()) +
  theme(plot.title=element_text(face="bold", hjust=0.5, size=24)) +
  theme(legend.position="bottom",
        legend.title=element_text(size=36, face="bold"),
        legend.key=element_rect(fill=NA),
        legend.text=element_text(size=36))
dev.off()

# Make an image for each plant
for(ID in unique(dat.band$id)){
  png(file.path(maps.out, "accessions", paste0("Map_Dendrometers_OakCollection_", ID, ".png")), height=4, width=6, unit="in", res=80)
  print(
    ggplot() +
      coord_equal(xlim=extent.map[1:2], ylim=extent.map[3:4]) +
      geom_polygon(data=woods, aes(x=long, y=lat, group=group), fill="darkgreen", alpha=0.5) +
      geom_path(data=roads[roads$name=="main route east side", ], aes(x=long, y=lat, group=group), size=5, color="gray80") +
      geom_path(data=paths, aes(x=long, y=lat, group=group), size=3, linetype="dashed", color="brown") +
      # geom_path(data=morton.grid, aes(x=long, y=lat, group=group), size=0.5, linetype="dotted", color="gray30") +
      geom_path(data=grid.crop, aes(x=long, y=lat, group=group), size=0.25, linetype="dotted", color="gray30") +
      geom_point(data=oaks.all, aes(x=BgLongitude, y=BgLatitude), color="gray50", size=1) +
      geom_point(data=oaks.all[oaks.all$PlantNumber %in% dat.band$id,], aes(x=BgLongitude, y=BgLatitude), color="black", size=2) +
      geom_point(data=oaks.all[oaks.all$PlantNumber==paste(ID),], aes(x=BgLongitude, y=BgLatitude), color="blue", size=5) +
      geom_text(data=labs.x[2:nrow(labs.x),], aes(x=long, y=lat+0.0002, label=x.lab), color="black", fontface="bold") +
      geom_text(data=labs.y[2:nrow(labs.y),], aes(x=long+0.0005, y=lat, label=y.lab), color="black", fontface="bold") +
      # scale_color_manual(values=c("gray50", "darkolivegreen3", "green4")) +
      # ggtitle(paste0("The Morton Arboretum\nLocation of Dendrometer Bands: Oak Collection")) +
      # labs(x="x (meters)", y="y (meters)") +
      theme(panel.grid=element_blank(),
            plot.background=element_blank(),
            panel.background=element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      theme(legend.position="bottom",
            legend.title=element_text(size=36, face="bold"),
            legend.key=element_rect(fill=NA),
            legend.text=element_text(size=36))
    )
  dev.off()
}
# ----------------------------------------
