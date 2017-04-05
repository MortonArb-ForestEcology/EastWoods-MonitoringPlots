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
library(lubridate)

# File paths
path.local <- "~/Desktop/Research/EastWoods-MonitoringPlots/plot_selection/"
path.gis <- "/Volumes/GIS/"

path.maps <- "maps/"
setwd(path.local)


# Useful layers:
# # IMLS plots found in Bob Fahey's files on Shared Drive; now copied locally
# NOTE: 2 sets -- all plots and then some that look like they've been filtered out b/c of roads etc
imls.all <- readOGR("/Volumes/shres/Fahey/Old Projects/East Woods Survey/Maps/imlsposts.shp")
imls.all <- imls.all[coordinates(imls.all)[,1]>0,] # There's one weird plot that needs to be filtered out
plot(imls.all)

# The filtered set from Bob
imls.bob <- readOGR("/Volumes/shres/Fahey/Old Projects/East Woods Survey/Maps/imlsposts2.shp")
summary(imls.bob)

# DEM 
dem <- raster("/Volumes/GIS/Collections/DEMs/ewoods/") # Elevation looks like its in feet

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
woods <- woods[2,] # We only want to worry about the main block; row 1 = King's Grove, row 2= main tract; row 3 = weird patch
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
plot(spTransform(imls.bob, projection(dem)), pch=19, cex=0.5, col="red3", add=T)
# ---------------------------------------


# ---------------------------------------
# Doing some calculations:
# 1. Topographic Indicies
# 2. Road, trail, stream buffers
# ---------------------------------------
# Topographic indices: slope, aspect, TPI
# Note: doing this one at a time to make it easier to interact with
dir.terrain <- file.path(path.local, "../EastWoods_GIS")
# if(!dir.exists(dir.terrain)) dir.create(dir.terrain)
# slope <- terrain(dem, opt="slope", unit="radians", neighbors=8, filename=file.path(dir.terrain, "eastwoods_slope"), overwrite=T)
# aspect <- terrain(dem, opt="aspect", unit="degrees", neighbors=8, filename=file.path(dir.terrain, "eastwoods_aspect"), overwrite=T)
# tpi <- terrain(dem, opt="TPI", neighbors=8, filename=file.path(dir.terrain, "eastwoods_tpi"), overwrite=T)
# tpi[tpi < -0.5] <- NA
# tpi[tpi > 0.5] <- NA
# tpi
# Now that we've calculated these, lets just load them rather than continuing to calculate them
slope <- raster(file.path(dir.terrain, "eastwoods_slope"))
aspect <- raster(file.path(dir.terrain, "eastwoods_aspect"))
tpi <- raster(file.path(dir.terrain, "eastwoods_tpi"))

par(mfrow=c(2,2))
plot(dem, main="elevation")
plot(slope, main="slope")
plot(aspect, main="aspect")
plot(tpi, main="tpi")
par(mfrow=c(1,1))

# Making some buffers around the lines & polygons
# Note: Add 14 m to buffer to account for plot size (rather than do buffer around plot center)
#  - previously had 10 m which would make a circle inside of our square plot;
#  - we want 14 m (the circle AROUND our plot) to make sure we don't get a corner in the way
# Buffers around: 
#  roads - 10 m + 5 m for road width, 
#  trails - 10m + 2 m for trail width, 
#  streams - 5m + 1 m for stream width 
#  forest boundary - 10 m + 0 for boundary width (assume perfect accuracy)
# 
# plot(roads)
plot(dem)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")

# Road buffer: assume line is in middle of road & road is ~5 meters wide, we'll do a 15m buffer to get us points 10m away from road
road.buff10 <- buffer(roads, width=10+5+14)
path.buff10 <- buffer(paths, width=10+2+14)
stream.buff5 <- buffer(streams.int, width=5+2+14)
plot(dem)
plot(spTransform(road.buff10, projection(dem)), col="gray50", add=T)
plot(spTransform(path.buff10, projection(dem)), col="tan4", add=T)
plot(spTransform(stream.buff5, projection(dem)), col="blue", add=T)
plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3, col="black")
class(road.buff10)

# East woods boundary
woods.border <- as(woods, "SpatialLinesDataFrame")
woods.buff10 <- buffer(woods.border, width=10+14)
plot(dem)
plot(spTransform(woods.buff10, projection(dem)), col="darkgreen", add=T)
# plot(spTransform(woods[2,], projection(dem)), col="green3", add=T)
plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3, col="black")
# ---------------------------------------

# ---------------------------------------
# Removing plots that 
# ---------------------------------------
plot(dem)
# plot(spTransform(burn, projection(dem)), add=T, col="tomato", lwd=0.5)
# plot(spTransform(harvest, projection(dem)), add=T, col="tan", lwd=0.5)
# plot(spTransform(mgmt, projection(dem)), add=T, lwd=1.5)
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

# First only subsetting those that are actually in the main Wooded boundary
sites <- intersect(imls.all, woods)

# Filtering out attributes
sites2 <- sites[is.na(over(sites, woods.buff10)),]
sites2 <- sites2[is.na(over(sites2, road.buff10)),]
sites2 <- sites2[is.na(over(sites2, path.buff10)),]
sites2 <- sites2[is.na(over(sites2, stream.buff5)),]

# Remove any sites in the harvested area
sites3 <- sites2[is.na(over(sites2, harvest)[,1]),]


nrow(imls.all)
nrow(sites)
nrow(sites2)
nrow(sites3)

plot(dem)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
plot(spTransform(sites3, projection(dem)), add=T, pch=19, cex=0.3)
# plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3)
plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="darkgreen")
# ---------------------------------------

# After getting rid of points with problematic proximities, we're down to potential 126 sites
# This is after 500 original options, and 260 in the East Woods alone.
# Only 89 aren't in areas that have been harvested

# ---------------------------------------
# Looking at topographic & management attributes of the remaining plots for 
# Stratified random sampling with relatively similar topogrpahic configuration
#
# Topogrpahic attributes:
# 1. Elevation
# 2. Aspect (dev. from N)
# 3. TPI
# 4. Slope
#
# Additional Attributes:
# - Burn Unit/Frequency
# ---------------------------------------
plot(dem)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
plot(spTransform(sites3, projection(dem)), add=T, pch=19, cex=0.3)
# plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3)
plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="darkgreen")

summary(sites3)
aspect.n <- aspect
aspect.n[aspect.n>180] <- aspect.n[aspect.n>180]-360
aspect.n

# Extracting with a 10-meter buffer because we're planning on 20-meter plots (so a radius of 14 would make a circle OUTSIDE of our square)
# We want to look at both the mean and the sd so we can try to have relatively homogenous configurations within our plots
sites3$elevation <- extract(dem     , spTransform(sites3, projection(dem)), buffer=14, fun=mean)
sites3$aspect    <- extract(aspect.n, spTransform(sites3, projection(dem)), buffer=14, fun=mean)
sites3$tpi       <- extract(tpi     , spTransform(sites3, projection(dem)), buffer=14, fun=mean)
sites3$slope     <- extract(slope   , spTransform(sites3, projection(dem)), buffer=14, fun=mean)
sites3$elevation.sd <- extract(dem     , spTransform(sites3, projection(dem)), buffer=14, fun=sd)
sites3$aspect.sd    <- extract(abs(aspect.n), spTransform(sites3, projection(dem)), buffer=14, fun=sd)
sites3$tpi.sd       <- extract(tpi     , spTransform(sites3, projection(dem)), buffer=14, fun=sd)
sites3$slope.sd     <- extract(slope   , spTransform(sites3, projection(dem)), buffer=14, fun=sd)

summary(sites3)

par(mfrow=c(2,2))
hist(sites3$elevation)
hist(sites3$aspect)
hist(sites3$tpi)
hist(sites3$slope)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(sites3$elevation.sd)
hist(sites3$aspect.sd)
hist(sites3$tpi.sd)
hist(sites3$slope.sd)
par(mfrow=c(1,1))

summary(sites3)

# Excluding points that have a lot of topographic variability within their areas
# Using the 90% quantile as a cutoff; make sure to always reference this against sites3
sites4 <- sites3[sites3$tpi.sd < quantile(sites3$tpi.sd, 0.9),]
sites4 <- sites4[sites4$aspect.sd < quantile(sites3$aspect.sd, 0.9),]
sites4 <- sites4[sites4$slope.sd < quantile(sites3$slope.sd, 0.9),]
sites4 <- sites4[sites4$elevation.sd < quantile(sites3$elevation.sd, 0.9),]
summary(sites4)
dim(sites3); dim(sites4)

# We also have 3 plots that are really spatially separated from the rest, so lets just remove those too
sites4 <- sites4[coordinates(spTransform(sites4, projection(dem)))[,1]>412700,]
# nrow(sites4)
# nrow(sites4b)

plot(dem)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
plot(spTransform(sites4, projection(dem)), add=T, pch=19, cex=0.3)
# plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3)
plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="darkgreen")


plot(dem)
plot(spTransform(burn, projection(dem)), add=T, col="tomato", lwd=0.5)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
plot(spTransform(sites4, projection(dem)), add=T, pch=19, cex=0.3)
# plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3)
plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="darkgreen")
# ---------------------------------------


# ---------------------------------------
# Looking at the burn units to do our stratified sampling
# ---------------------------------------
# Getting burn history for each plot
site.burns <- over(sites4, burn, returnList=T)

for(i in 1:length(site.burns)){
  burn.n <- nrow(site.burns[[i]])
  sites4[i,"burn.n"] <- burn.n
  
  # If this hasn't burned, skip it, otherwise calculate the most recent year of burn
  if(burn.n == 0) next
  sites4[i,"burn.last"] <- ifelse(max(year(site.burns[[i]]$Burn_Date),na.rm=T)>0, max(year(site.burns[[i]]$Burn_Date),na.rm=T), NA)
}
summary(sites4)

# Get rid of anything with records of burn, but no date for the last time a burn happened
sites4 <- sites4[sites4$burn.n==0 | (sites4$burn.n>0 & !is.na(sites4$burn.last)),]
summary(sites4)
dim(sites4)

hist(sites4$burn.n, breaks=seq(min(sites4$burn.n), max(sites4$burn.n), 1))
# hist(sites4$burn.last)

# Categorizing our burn frequency
sites4$burn.regime <- as.factor(ifelse(sites4$burn.n==0, "none", 
                                       ifelse(sites4$burn.n>=10, "lots", "some")))

# Looking at our data by burn class
summary(sites4[sites4$burn.regime=="none",])
summary(sites4[sites4$burn.regime=="some",])
summary(sites4[sites4$burn.regime=="lots",])

# For our "some" category, narrow the burn down to just 2012 burns
sites4 <- sites4[sites4$burn.regime!="some" | (sites4$burn.regime=="some" & sites4$burn.last<2015),]
summary(sites4)

plot(dem)
# plot(spTransform(burn, projection(dem)), add=T, col="tomato", lwd=0.5)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
plot(spTransform(sites4[sites4$burn.regime=="none",], projection(dem)), add=T, pch=19, cex=0.5)
plot(spTransform(sites4[sites4$burn.regime=="some",], projection(dem)), add=T, pch=19, cex=0.5, col="chocolate2")
plot(spTransform(sites4[sites4$burn.regime=="lots",], projection(dem)), add=T, pch=19, cex=0.5, col="red")
# plot(spTransform(imls.all, projection(dem)), add=T, pch=19, cex=0.3)
plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="darkgreen")


# Looking at the map, we have 3 good areas to base our stratification on:
# None - A: inside loop (x<413500 & y<4630000)
# None - B: outside loop x>413500 & y>4629500
# Some - C: north of loop, y>463000
# LOTS - D: south of loop (no outliers)
sites4$stand <- as.factor(ifelse(sites4$burn.regime=="lots", "D",
                                 ifelse(sites4$burn.regime=="some" & coordinates(spTransform(sites4, projection(dem)))[,2]>4630000, "C",
                                 ifelse(sites4$burn.regime=="none" & coordinates(spTransform(sites4, projection(dem)))[,1]>413500 & coordinates(spTransform(sites4, projection(dem)))[,2]>4629500, "B",
                                 ifelse(sites4$burn.regime=="none" & coordinates(spTransform(sites4, projection(dem)))[,1]<413500 & coordinates(spTransform(sites4, projection(dem)))[,2]<4630000, "A", NA)))))

sites4 <- sites4[!is.na(sites4$stand),]
summary(sites4)
dim(sites4)
plot(dem)
# plot(spTransform(burn, projection(dem)), add=T, col="tomato", lwd=0.5)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
plot(spTransform(sites4[sites4$stand=="A",], projection(dem)), add=T, pch=19, cex=1)
plot(spTransform(sites4[sites4$stand=="B",], projection(dem)), add=T, pch=19, cex=1)
plot(spTransform(sites4[sites4$stand=="C",], projection(dem)), add=T, pch=19, cex=1, col="chocolate2")
plot(spTransform(sites4[sites4$stand=="D",], projection(dem)), add=T, pch=19, cex=1, col="red")
plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="darkgreen")
legend(x=411300, y=4630700, xjust=0, yjust=0, legend=c("None", "Some", "Lots"), pch=19, col=c("black", "chocolate2", "red"), bty="n", cex=1.5, title="Burn Regime")
# ---------------------------------------


# ---------------------------------------
# Randomly assigning an order within each stand and saving it as a .csv
# ---------------------------------------
set.seed(1549)
for(i in unique(sites4$stand)){
  plot.order <- sample(1:nrow(sites4[sites4$stand==i,]), nrow(sites4[sites4$stand==i,]))
  sites4[sites4$stand==i,"order"] <- plot.order
}
data.frame(sites4[sites4$stand==i,])

png(file.path(path.maps, "CandidatePlots.png"), height=8, width=11, units="in", res=220)
plot(dem)
# plot(spTransform(burn, projection(dem)), add=T, col="tomato", lwd=0.5)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
plot(spTransform(sites4[sites4$stand=="A",], projection(dem)), add=T, pch=19, cex=1)
plot(spTransform(sites4[sites4$stand=="B",], projection(dem)), add=T, pch=19, cex=1)
plot(spTransform(sites4[sites4$stand=="C",], projection(dem)), add=T, pch=19, cex=1, col="chocolate2")
plot(spTransform(sites4[sites4$stand=="D",], projection(dem)), add=T, pch=19, cex=1, col="red")
plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="darkgreen")
legend(x=411300, y=4630700, xjust=0, yjust=0, legend=c("None", "Some", "Lots"), pch=19, col=c("black", "chocolate2", "red"), bty="n", cex=1.5, title="Burn Regime")
dev.off()

png(file.path(path.maps, "CandidatePlots_Order.png"), height=8, width=11, units="in", res=220)
plot(dem)
# plot(spTransform(burn, projection(dem)), add=T, col="tomato", lwd=0.5)
plot(spTransform(water, projection(dem)), col="blue", add=T)
plot(spTransform(pools, projection(dem)), col="lightblue", add=T)
plot(spTransform(streams.perm, projection(dem)), col="blue", add=T)
plot(spTransform(streams.int, projection(dem)), col="blue", add=T, lty="dashed")
plot(spTransform(springs, projection(dem)), col="blue", add=T, pch=8)
plot(spTransform(roads, projection(dem)), add=T, lwd=2, col="gray50")
plot(spTransform(paths, projection(dem)), add=T, lwd=1.5, col="tan4", lty="dashed")
text(x=coordinates(spTransform(sites4[sites4$stand=="A",], projection(dem)))[,1], y=coordinates(spTransform(sites4[sites4$stand=="A",], projection(dem)))[,2], label=data.frame(sites4)[sites4$stand=="A","order"], pch=19, cex=0.7, font=2)
text(x=coordinates(spTransform(sites4[sites4$stand=="B",], projection(dem)))[,1], y=coordinates(spTransform(sites4[sites4$stand=="B",], projection(dem)))[,2], label=data.frame(sites4)[sites4$stand=="B","order"], pch=19, cex=0.7, font=2)
text(x=coordinates(spTransform(sites4[sites4$stand=="C",], projection(dem)))[,1], y=coordinates(spTransform(sites4[sites4$stand=="C",], projection(dem)))[,2], label=data.frame(sites4)[sites4$stand=="C","order"], pch=19, cex=0.7, font=2, col="chocolate2")
text(x=coordinates(spTransform(sites4[sites4$stand=="D",], projection(dem)))[,1], y=coordinates(spTransform(sites4[sites4$stand=="D",], projection(dem)))[,2], label=data.frame(sites4)[sites4$stand=="D","order"], pch=19, cex=0.7, font=2, col="red")
plot(spTransform(woods, projection(dem)), add=T, lwd=3, border="darkgreen")
legend(x=411300, y=4630700, xjust=0, yjust=0, legend=c("None", "Some", "Lots"), pch=19, col=c("black", "chocolate2", "red"), bty="n", cex=1.5, title="Burn Regime")
dev.off()

writeOGR(sites4, "data", "CandidatePlots", "ESRI Shapefile")


sites4.df <- data.frame(spTransform(sites4, CRS("+proj=longlat")))
sites4.df <- sites4.df[,c("coords.x1", "coords.x2", "CORNER", "stand", "order", "burn.regime", "burn.n", "burn.last", "elevation", "aspect", "tpi", "slope")]
names(sites4.df)[1:2] <- c("Lon", "Lat")
sites4.df <- sites4.df[order(sites4.df$stand, sites4.df$order),]
summary(sites4.df)

head(sites4.df)

write.csv(sites4.df, "data/CandidatePlots.csv", row.names=F)

# ---------------------------------------
