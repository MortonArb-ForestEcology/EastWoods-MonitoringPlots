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
dir.base <- "/Volumes/GoogleDrive/My Drive/East Woods/Rollinson_Monitoring"
# setwd(dir.base)

# "/Volumes/GoogleDrive/My Drive/East Woods/Rollinson_Monitoring/"
path.dat <- file.path(dir.base, "Data/Dendrometer")
maps.out <- file.path(dir.base, "figures/maps")
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
# ---------------------

# ---------------------
# Loading GIS layers
# # Note: Must be connected to GIS server for this to work
# ---------------------
# Trees! 2016 is the most recent I could find
# Note: there's a LOT of info I don't want
db.cols <- c("sci_name", "sort_scina", "sci_nm3", "type", "trade_nm", "plant_id", "habitat", "coll_id", "coll_nm", "collsuba", "grid_loc", "x_coord", "y_coord")
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
trees.sheet <- gs_title("Tree_PlotSurvey_2017")
trees.sheet

trees.all <- data.frame(gs_read(trees.sheet, "raw data"))
summary(trees.all)

for(i in 1:ncol(trees.all)){
  if(class(trees.all[,i])=="character") trees.all[,i] <- as.factor(trees.all[,i])
}
summary(trees.all)

# band.trees <- read.csv("data/dendrometers/Dendroband_Installation.csv")
# https://drive.google.com/open?id=1CtcZeEKoaQdLOwxb-9kpqpWgeCf1AziWYt9UT5YH31M
sheet.band <- gs_title("DendrobandObservations_EastWoods")
# gs_ls()
sheet.band

# get the data from a particular sheet
dat.band <- data.frame(gs_read(sheet.band, ws="data 2017"))
summary(dat.band)

for(i in 1:ncol(dat.band)){
  if(class(dat.band[,i])=="character") dat.band[,i] <- as.factor(dat.band[,i])
}
summary(dat.band)


png(file.path(maps.out, paste0("Map_Dendrometers_EastWoods_", Sys.Date(), ".png")), height=8, width=9, unit="in", res=220)
ggplot(data=trees.all) +
  coord_equal() +
  facet_wrap(~Plot) +
  geom_point(aes(x=X, y=Y, size=DBH)) +
  geom_point(data=trees.all[trees.all$Tag %in% dat.band$id, ], aes(x=X, y=Y, size=DBH, color=Sp_code)) +
  theme_bw()
dev.off()

# ---------------------

# ----------------------------------------
