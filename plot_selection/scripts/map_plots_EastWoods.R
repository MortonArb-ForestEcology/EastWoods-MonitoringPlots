# ---------------------------------------
# Mapping & Plot Layout for, long-term high-res monitoring plots for Forest Ecology Research
# Christy Rollinson; crollinson@mortonarb.org
# 23 May, 2017
#
# ------------------
# Description: 
# ------------------
# 1 Plot in each of 4 management areas were selected from the list generated in script 1_plot_selection.
# This script will map the location for each
#
# Plot Design:
#  - 20 x 20 m square centered on IMLS plot
#    - 5-m grid for orientation & subplot
#    - All trees >5 cm dbh
#  - 5 x 5 m subplot shrubs, saplings (x 2 per plot?)
#  - 1 x 1 m subplot for forbs, seedlings (x4 per plot?)
#
# Things that will be measured in the plots:
# A. Vegetation (weekly):
#    1. Trees -- individuals: species, DBH, dendrobands, leaf/flower phenology
#    2. Shrubs (subplots) -- individuals: species, density, leaf/flower phenology, cover
#    3. Herbs (subplots) -- species: cover, phenology
#    4. Seedlings (subplots) -- individuals: phenology, survival, growth?? (fall only?)
# B. Micromet (continous)
#    1. PAR
#    2. Temperature
#    3. Humidity
#    4. Soil Temperature
#    5. Soil Moisture
# C. Other (monthly)
#    1. Leaf Fall
#    2. Seed rain
# D. Possible Additions (lead by others)
#    1. Soil Nutrients
#    2. Root dynamics
#    3. Coarse Woody debris
#    4. Throughfall chemistry
# ------------------
# ---------------------------------------


# ---------------------------------------
# Define file paths to different libraries & layers
#
# Useful layers:
# - Candidate Plots
# - DEM 
# - Roads
# - Trails
# - Streams
# - Woodland Boundary
# - burn units
# - harvest units
# ---------------------------------------
# Libraries
library(raster); library(rgdal); library(rgeos)
library(lubridate)

# File paths
path.local <- "~/Desktop/Research/EastWoods-MonitoringPlots/plot_selection/"
path.gis <- "/Volumes/GIS/"

path.maps <- "maps/"
setwd(path.local)


# Useful layers:
# # IMLS plots found in Bob Fahey's files on Shared Drive; now copied locally
# NOTE: 2 sets -- all plots and then some that look like they've been filtered out b/c of roads etc
candidates <- readOGR("data/CandidatePlots.shp")
plot(candidates)
summary(candidates)

# Extracting the sites we're using
candidates$PlotID <- as.factor(paste0(candidates$stand, candidates$order))
selected <- data.frame(stand=c("A", "B", "C", "D"),
                       order=c( 1 ,  5 ,  6 ,  1))
selected$PlotID <- as.factor(paste0(selected$stand, selected$order))
selected.pts <- candidates[candidates$PlotID %in% selected$PlotID,]

# DEM 
dem <- raster("/Volumes/GIS/Collections/DEMs/ewoods/") # Elevation looks like its in feet

# Water, streams
water <- readOGR("/Volumes/GIS/Collections/hydrology/water.shp")
# water <- readOGR("/Volumes/GIS/Collections/hydrology/wat_co_arboretum.shp")
pools <- readOGR("/Volumes/GIS/Collections/hydrology/wat_pools_seasonal.shp")
streams.perm <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_permanent.shp")
streams.int <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_seasonal.shp")

# Roads, Trails
roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_2011-2020_ctrln.shp")
paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
parking <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/parking_lots.shp")
summary(roads)
summary(paths)

# Property & Woodland boundarys
property <- readOGR("/Volumes/GIS/Collections/boundaries/Morton_Arboretum.shp")
woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
woods <- woods[2,] # We only want to worry about the main block; row 1 = King's Grove, row 2= main tract; row 3 = weird patch
# mgmt  <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Boundaries/New Management Units.shp")
# harvest <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Canopy Thinning/Canopy Thinning.shp")
# burn <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Burn/Burned_Area.shp")
# summary(harvest)

# Transforming everythign to the DEM crs just to make life easier
candidates   <- spTransform(candidates, projection(dem))
selected.pts <- spTransform(selected.pts, projection(dem))
water        <- spTransform(water, projection(dem))
pools        <- spTransform(pools, projection(dem))
streams.perm <- spTransform(streams.perm, projection(dem))
streams.int  <- spTransform(streams.int, projection(dem))
roads        <- spTransform(roads, projection(dem))
paths        <- spTransform(paths, projection(dem))
woods        <- spTransform(woods, projection(dem))
property     <- spTransform(property, projection(dem))
parking      <- spTransform(parking, projection(dem))
# ---------------------------------------


# ---------------------------------------
# Making an updated, pretty map for files
# ---------------------------------------
library(ggplot2)
library(scales)
library(ggspatial)

dem.df <- data.frame(rasterToPoints(dem))
summary(dem.df)

ewoods <- fortify(woods)
holes <- which(!duplicated(ewoods$group))

# Stolen from http://apps.fishandwhistle.net/archives/1126
fixfeature <- function(df) {
  ringstarts <- which(!duplicated(df$group))
  if(length(ringstarts) < 2) {
    return(df)
  } else {
    ringstarts <- c(ringstarts, nrow(df))
    indicies <- c(1:(ringstarts[2]-1), do.call(c, lapply(2:(length(ringstarts)-1), function(x) {
      c(1, ringstarts[x]:(ringstarts[x+1]-1))
    })), nrow(df))
    return(df[indicies,])
  }
}

ewoods <- fixfeature(ewoods)
summary(ewoods)

# Adding the x-y coordinates for our selected plots
selected.pts <- selected.pts[order(selected.pts$PlotID),]
data.frame(selected.pts)
selected.pts$PlotID
selected[,c("x", "y")] <- coordinates(selected.pts)
summary(selected)

png("maps/MonitoringPlot_Locations_Simple.png", height=6, width=8, units="in", res=180)
ggplot() +
  # geom_raster(data=dem.df, aes(x=x, y=y, fill=ewoods)) +
  geom_polygon(data=ewoods, aes(x=long, y=lat, group=id), fill="green4", alpha=0.5) +
  geom_path(data=property, aes(x=long, y=lat, group=group), color="black", size=2) +
  geom_polygon(data=parking, aes(x=long, y=lat, group=group), fill="navajowhite4") +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), color="ivory3", size=1) +
  # geom_path(data=roads, aes(x=long+5, y=lat+5, group=group), color="ivory", size=0.2) +
  geom_path(data=paths, aes(x=long, y=lat, group=group), color="tan4", size=0.7, linetype="dashed") +
  geom_point(data=selected, aes(x=x, y=y), size=5, color="blue") +
  scale_fill_gradient(low="black", high="white", name="Elevation") +
  coord_equal(xlim=range(dem.df$x), ylim=c(min(dem.df$y), 4631000), expand=F) +
  theme_bw() +
  theme(legend.position="top",
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank())
dev.off()

# ---------------------------------------
