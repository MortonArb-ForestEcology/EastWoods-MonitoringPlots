# ---------------------------------------
# Selection of permanent, weekly monitoring plots for Forest Ecology Research
# Christy Rollinson; crollinson@mortonarb.org
# 29 March, 2017
#
# ------------------
# Description: 
# ------------------
# Script to select 4 points from the IMLS(?) Survey to establish permanent intensive
# montoring sites. These plots are targeted to look at unified, temporal dynamics of 
# ecosystems and measure most demographic and plant community ecology processes we can
# observe.  Due to the intensive monitoring nature, right now we're only going to start
# with 4 so that it's a manageable number.
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
# 
# ------------------
# Plot selection criteria
# ------------------
# - Selection Method: Stratified Random
# - Stratification by burn
# - Plot edges 
#   - >= 10m from roads & paths
#   - >= 5m from stream/drainages
#   - >= 10m from forest edge
# - Maximum slope:
# - Maximum/Minimum TPI (avoid concavities):
# - No recent harvest activity
# ------------------
# ---------------------------------------

# ---------------------------------------
# Define file paths to different libraries & layers
#
# Useful layers:
# - IMLS points
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

# File paths
path.local <- "~/Desktop/Research/EastWoods-MonitoringPlots/plot_selection/"
path.gis <- "/Volumes/GIS/"
setwd(path.local)


# Useful layers:
# # IMLS plots found in Bob Fahey's files on Shared Drive; now copied locally
# NOTE: 2 sets -- all plots and then some that look like they've been filtered out b/c of roads etc
imls.all <- readOGR("/Volumes/elm/Fahey/Old Projects/East Woods Survey/Maps/imlsposts.shp")
imls.all <- imls.all[coordinates(imls.0)[,1]>0,] # There's one weird plot that needs to be filtered out
plot(imls.all)

# The filtered set from Bob
imls.bob <- readOGR("/Volumes/elm/Fahey/Old Projects/East Woods Survey/Maps/imlsposts2.shp")
summary(imls)

# DEM 
dem <- raster("/Volumes/GIS/Collections/DEMs/ewoods/")

# Water, streams
water <- readOGR("/Volumes/GIS/Collections/hydrology/water.shp")
# water <- readOGR("/Volumes/GIS/Collections/hydrology/wat_co_arboretum.shp")
pools <- readOGR("/Volumes/GIS/Collections/hydrology/wat_pools_seasonal.shp")
streams.perm <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_permanent.shp")
streams.int <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_seasonal.shp")
springs <- readOGR("/Volumes/GIS/Collections/hydrology/wat_springs.shp")

# Roads, Trails
roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_MASTER_ctrln.shp")
paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
summary(roads)
summary(paths)

# Woodland boundarys
woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
mgmt  <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Boundaries/New Management Units.shp")
harvest <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Canopy Thinning/Canopy Thinning.shp")
burn <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Burn/Burned_Area.shp")
summary(harvest)



plot(dem)
plot(spTransform(burn, projection(dem)), add=T, col="tomato", lwd=0.5)
plot(spTransform(harvest, projection(dem)), add=T, col="tan", lwd=0.5)
plot(spTransform(mgmt, projection(dem)), add=T, lwd=1.5)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
# plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="green4")
plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3)
plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="darkgreen")
plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3)


plot(dem)
# plot(spTransform(pools, projection(dem)), col="blue", add=T)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
# plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="green4")
plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3)
plot(spTransform(imls, projection(dem)), pch=19, cex=0.5, col="red3", add=T)
# ---------------------------------------
