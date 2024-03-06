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

