# Script to read in, clean, and analyze our past leaf litter data
# Known Issues to work through:
# 1. sometimes bags are empty; this *should* be true 0s, but possible it's inconsistent
# # # note: They have put "Empty" in the notes column if the bag was empty; "knocked over" means it should be no obs, not an obs of 0
# 2. 

library(ggplot2)

# Set up file paths etc. --> this should also indicate where you can find these files!
path.google <- "~/Google Drive/My Drive"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.figs <- file.path(path.litter, "figures") # where we shoudl save some figures
path.save <- file.path(path.litter, "LeafLitterData_Clean_forArchiving") # Where we shoudl save the data

if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)
if(!dir.exists(path.save)) dir.create(path.save, recursive=T)

# Should we overwrite old data or not?
overwrite=T

# The Google Drive key ID for the leaf litter spreadsheet; 
keyLeafLitter <- "1d7Py4ehN2PmrmKmyv2hDUkX4fWa95xdlQlGVBN9x20g" 

# Using a formatting theme consistent with what Meghan has done
theme.meghan <-   theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_rect(fill="white", colour = "black", size=.7),
                        axis.title.x = element_text(margin = margin(t = 10, b=5), size=14),
                        axis.title.y = element_text(margin = margin(l = 5, r=5), size=14),
                        axis.text.x= element_text(margin = margin(t = 10), size=12),
                        axis.text.y=element_text(margin = margin(r = 10), size=12),
                        axis.ticks.length=unit(-0.3, "cm"),
                        # axis.ticks.margin=unit(0.5, "cm"),
                        axis.ticks = element_line(colour = "black", linewidth = 0.4))


# Setting a consistent color scheme across all graphs
plotOrder <- c("B-127", "U-134", "N-115", "HH-115")
ewPlotColors <- c("#1B9E77","#D95F02", "#7570B3", "#E7298A")
names(ewPlotColors) = plotOrder
ewPlotColors


################################################
# Read in and save the data ----
################################################
# Read in and look at the data
datLeafLitter <- googlesheets4::read_sheet(ss=keyLeafLitter, sheet="raw_data")
summary(datLeafLitter)

### ---------------------------
# # #  Uncommment this code if columns are reading in as a list to track down issues
### ---------------------------
# # Columns to figure out: Num_fruit.Length
# # num_fruit nuM_mature_fruit
# test <- datLeafLitter$num_mature_fruit
# head(test)
# 
# testType <- unlist(lapply(test, FUN=function(x){class(x)}))
# summary(as.factor(testType))
# which(testType=="character")
### ---------------------------

# Conver to a data frame because tibbles drive Christy crazy
datLeafLitter <- data.frame(datLeafLitter)

# Converting columns to factors to make checks easier
colFact <- c("sorter", "plot", "trap_ID", "genus", "species", "oak_group", "tissue", "weigher", "data_entry")
for(COL in colFact){
  datLeafLitter[,COL] <- as.factor(datLeafLitter[,COL])
}
datLeafLitter$plot <- factor(datLeafLitter$plot, levels=plotOrder)

summary(datLeafLitter)

# dropping unused columns
colDrop <- c("oak_group", "taxon", "mass_per_cm")
datLeafLitter <- datLeafLitter[,!names(datLeafLitter) %in% colDrop]
summary(datLeafLitter)

# Subset to only things that have actually been weighed
# Paul & Roxanne pre-populate their spreadsheet, which drives me crazy, but we'll have that fight a bit later
# datLeafLitter <- datLeafLitter[!is.na(datLeafLitter$date_weighed) & as.Date(datLeafLitter$date_weighed)<Sys.Date(),]
datLeafLitter <- datLeafLitter[!is.na(datLeafLitter$date_weighed) & datLeafLitter$date_weighed<=Sys.Date(),]
summary(datLeafLitter)


summary(datLeafLitter$genus)
summary(datLeafLitter$species)
summary(datLeafLitter$tissue)



# Cleaning up dat based on tissue characterization:
# # If tissue == "EMPTY BAG" this is a true 0 and mass is 0
# # if tissue is or something like "FELL OVER", "No Bag" then it's missing data and is NOT a 0
datLeafLitter$mass_g[datLeafLitter$tissue=="EMPTY BAG"] <- 0
summary(datLeafLitter)

# Saving Data before the current year for archiving.
yrsAll <- unique(lubridate::year(datLeafLitter$date_collection))
yrMax <- max(yrsAll)
yrsSave <- yrsAll[yrsAll<yrMax]

# For the current year, write the most recent date that's been processed
# check for existing files
fYrMax <- dir(path.save, as.character(yrMax))

datNow <- datLeafLitter[lubridate::year(datLeafLitter$date_collection)==yrMax,]
# summary(datNow)
if(nrow(datNow)>0){
  dateEnd <- max(as.Date(datLeafLitter$date_collection))
  fName <- paste0("MortonArb_EastWoods_LeafLitter_", yrMax, "_upto_", dateEnd, ".csv")
  
  write.csv(datNow, file.path(path.save, fName), row.names=F)
  
  if(length(fYrMax[fYrMax!=fName])>0) rm(fYrMax)
} 



# Save Past Years
for(YR in yrsSave){
  # YR=yrsSave[1]
  fName <- paste0("MortonArb_EastWoods_LeafLitter_", YR, ".csv")
  if(!overwrite & file.exists(file.path(path.save, fName))) next
  
  datNow <- datLeafLitter[lubridate::year(datLeafLitter$date_collection)==YR,]
  # summary(datNow)
  
  write.csv(datNow, file.path(path.save, fName), row.names=F)
}
################################################



################################################
# Now doing some plotting of the data ----
# Things we want to look at:
# - Average biomass by tissue by plot by year
# - Average biomass of leaves, fruit by species through time each year
# - Total number of fruits, ripe fruits by species per year
#
# NOTE: Need to account for absent data at all stages
# NOTE: because we care about the plot, we'll want to aggregate across traps first to get the mean per plot
# NOTE: For species, we'll need to do the above, but also account for absent data <-- this will require some thought
################################################
datLeafLitter$year <- lubridate::year(datLeafLitter$date_collection)
datLeafLitter$yday <- lubridate::yday(datLeafLitter$date_collection)
summary(datLeafLitter)

# - Average biomass by tissue by plot by year
aggTissTrap <- aggregate(mass_g ~ tissue + year + plot + trap_ID, data=datLeafLitter, FUN=sum)
aggTissTrap$plot <- factor(aggTissTrap$plot, levels=plotOrder)
summary(aggTissTrap)

ggplot(data=aggTissTrap) +
  facet_wrap(~tissue) +
  geom_boxplot(aes(x=as.factor(year), y=mass_g, fill=plot)) +
  labs(x="Year", y="Shannon BD (H')") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme.meghan

aggTissPlot <-aggregate(mass_g ~ tissue + year + plot + trap_ID, data=datLeafLitter, FUN=mean)
summary(aggTissPlot)
################################################
