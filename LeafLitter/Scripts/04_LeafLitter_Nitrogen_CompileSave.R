library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)
library(stringr)
library(readr)

path.google <- "~/Google Drive/My Drive"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.CN <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data/CN_runs")
path.figs <- file.path(path.litter, "figures") # where we shoudl save some figures
path.save <- file.path(path.litter, "LeafLitterData_Clean_forArchiving") # Where we shoudl save the data

path.CN <- "~/Library/CloudStorage/GoogleDrive-lizer1@stolaf.edu/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Leaf_litter_data/CN_Runs/"

dir(path.CN)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Formating 2018 ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
LLN2018 <- read.csv(file.path(path.CN, "Fall_2018_run1.csv"))
summary(LLN2018)
head(LLN2018)

# 2018 Plot
LLN2018$plot[substr(LLN2018$Name,1,1)=="B"] <- "B-127"  
LLN2018$plot[substr(LLN2018$Name,1,1)=="H"] <- "HH-115"  
LLN2018$plot[substr(LLN2018$Name,1,1)=="N"] <- "N-115"  
LLN2018$plot[substr(LLN2018$Name,1,1)=="U"] <- "U-134"  
LLN2018$plot <-as.factor(LLN2018$plot)

# 2018 Date
LLN2018$date_collection <- as.Date(substr(LLN2018$Name, 3, 10), format="%y-%m-%d")
summary(LLN2018)

# 2018 Species
LLN2018$sci_name <- substr(LLN2018$Name,11,12)
LLN2018$sci_name <- car::recode(LLN2018$sci_name, "'QA'='Quercus alba'; 'TA'='Tilia americana'; 'AS'='Acer saccharum'")
LLN2018$sci_name <-as.factor(LLN2018$sci_name)
summary(LLN2018)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Formating 2021-2022 ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LLN2122path<-list.files(path.CN, pattern = "Lizer_EWLL_2021-2022_run.xlsx", full.names = TRUE)
LLN2122 <- read_xlsx(file.path(path.CN, "Lizer_EWLL_2021-2022_run.xlsx"))
LLN2122 <- data.frame(LLN2122)
names(LLN2122) <- c("Name", "X.N", "X.C", "C.N")
head(LLN2122)

# 2021-2022 Plot
LLN2122$plot[substr(LLN2122$Name,1,1)=="B"] <- "B-127"  
LLN2122$plot[substr(LLN2122$Name,1,1)=="H"] <- "HH-115"  
LLN2122$plot[substr(LLN2122$Name,1,1)=="N"] <- "N-115"  
LLN2122$plot[substr(LLN2122$Name,1,1)=="U"] <- "U-134"  
LLN2122$plot <-as.factor(LLN2122$plot)
summary(LLN2122)

# 2021-2022 Date
LLN2122$date_collection <- unlist(lapply(strsplit(LLN2122$Name, "_"), function(x){x[2]}))
LLN2122$date_collection <- as.Date(LLN2122$date_collection, format="%Y%m%d")
summary(LLN2122)

# 2021-2022 Species
LLN2122$sci_name <- unlist(lapply(strsplit(LLN2122$Name, "_"), function(x){x[3]}))
LLN2122$sci_name <- car::recode(LLN2122$sci_name, "'QUAL'='Quercus alba'; 'QUAB'='Quercus alba'; 'QURU'='Quercus rubra'; 'ACSA'='Acer saccharum'")
LLN2122$sci_name <- as.factor(LLN2122$sci_name)
summary(LLN2122)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Formatting  "Run 2" ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
dir(path.CN)
LLN2 <- data.frame(read_xlsx(file.path(path.CN, "Lizer_EWLL_run2_0715_corrected.xlsx")))
LLN2 <- LLN2[,c("Name", "N.....", "C.....", "C.N..ratio")]
names(LLN2) <- c("Name", "X.N", "X.C", "C.N")
LLN2 <- LLN2[!LLN2$Name %in% c("blank-", "runIn", "standard", "stop", "test", NA, "Acetanilide", "check"),]
head(LLN2)
tail(LLN2)

# Run2 Plot
LLN2$plot[substr(LLN2$Name,1,1)=="B"] <- "B-127"  
LLN2$plot[substr(LLN2$Name,1,1)=="H"] <- "HH-115"  
LLN2$plot[substr(LLN2$Name,1,1)=="N"] <- "N-115"  
LLN2$plot[substr(LLN2$Name,1,1)=="U"] <- "U-134"  
LLN2$plot <-as.factor(LLN2$plot)
summary(LLN2)

# Run2 Date
LLN2$date_collection <- unlist(lapply(strsplit(LLN2$Name, "_"), function(x){x[2]}))
LLN2$date_collection <- as.Date(LLN2$date_collection, format="%Y%m%d")
summary(LLN2)

# Run2 Species
LLN2$sci_name <- unlist(lapply(strsplit(LLN2$Name, "_"), function(x){x[3]}))
LLN2$sci_name <- car::recode(LLN2$sci_name, "'QUAL'='Quercus alba'; 'QUAB'='Quercus alba'; 'QURU'='Quercus rubra'; 'ACSA'='Acer saccharum'")
LLN2$sci_name <-as.factor(LLN2$sci_name)

summary(LLN2)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Formatting  "Run 3" ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
dir(path.CN)
LLN3 <- data.frame(read_xlsx(file.path(path.CN, "Lizer_EWLL_Run3_SH_072125.xlsx")))
LLN3 <- LLN3[,c("Name", "N.....", "C.....", "C.N..ratio")]
names(LLN3) <- c("Name", "X.N", "X.C", "C.N")
LLN3 <- LLN3[!LLN3$Name %in% c("blank-", "runIn", "standard", "stop", "test", NA, "Acetanilide", "acetanilide", "check"),]
head(LLN3)
tail(LLN3)

# Run3 Plot
LLN3$plot[substr(LLN3$Name,1,1)=="B"] <- "B-127"  
LLN3$plot[substr(LLN3$Name,1,1)=="H"] <- "HH-115"  
LLN3$plot[substr(LLN3$Name,1,1)=="N"] <- "N-115"  
LLN3$plot[substr(LLN3$Name,1,1)=="U"] <- "U-134"  
LLN3$plot <-as.factor(LLN3$plot)
summary(LLN3)

# Run3 Date
LLN3$date_collection <- unlist(lapply(strsplit(LLN3$Name, "_"), function(x){x[2]}))
LLN3$date_collection <- as.Date(LLN3$date_collection, format="%Y%m%d")
summary(LLN3)

# Run3 Species
LLN3$sci_name <- unlist(lapply(strsplit(LLN3$Name, "_"), function(x){x[3]}))
LLN3$sci_name <- car::recode(LLN3$sci_name, "'QUAL'='Quercus alba'; 'QUAB'='Quercus alba'; 'QURU'='Quercus rubra'; 'ACSA'='Acer saccharum'")
LLN3$sci_name <- as.factor(LLN3$sci_name)
summary(as.factor(LLN3$sci_name))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Formatting  "Run 4a" ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
dir(path.CN)
LLN4a <- data.frame(read_xlsx(file.path(path.CN, "Lizer_EWLL_run_4.xlsx")))
LLN4a <-LLN4a[1:70,] #Error with machine here, Sav says samples 1-70 look good, re-running samples after 70
LLN4a <- LLN4a[,c("Name", "N.....", "C.....", "C.N..ratio")]
names(LLN4a) <- c("Name", "X.N", "X.C", "C.N")
LLN4a <- LLN4a[!LLN4a$Name %in% c("Blank-","blank-", "runIn", "standard", "stop", "test", NA, "Acetanilide", "acetanilide", "check"),]
head(LLN4a)
tail(LLN4a)

# Run4a Plot
LLN4a$plot[substr(LLN4a$Name,1,1)=="B"] <- "B-127"  
LLN4a$plot[substr(LLN4a$Name,1,1)=="H"] <- "HH-115"  
LLN4a$plot[substr(LLN4a$Name,1,1)=="N"] <- "N-115"  
LLN4a$plot[substr(LLN4a$Name,1,1)=="U"] <- "U-134"  
LLN4a$plot <-as.factor(LLN4a$plot)
summary(LLN4a)

# Run4a Date
LLN4a$date_collection <- unlist(lapply(strsplit(LLN4a$Name, "_"), function(x){x[2]}))
LLN4a$date_collection <- as.Date(LLN4a$date_collection, format="%Y%m%d")
summary(LLN4a)

# Run4a Species
LLN4a$sci_name <- unlist(lapply(strsplit(LLN4a$Name, "_"), function(x){x[3]}))
LLN4a$sci_name <- car::recode(LLN4a$sci_name, "'QUAL'='Quercus alba'; 'QUAB'='Quercus alba'; 'QURU'='Quercus rubra'; 'ACSA'='Acer saccharum'; 'ASCA'='Acer saccharum'")
LLN4a$sci_name <- as.factor(LLN4a$sci_name)
summary(as.factor(LLN4a$sci_name))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Formatting  "Run 4b" ---- the rerun samples from 4a
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
dir(path.CN)
LLN4b <- data.frame(read_xlsx(file.path(path.CN, "Lizer_EWLL_plate_4_rerun.xlsx")))
LLN4b <- LLN4b[,c("Name", "N.....", "C.....", "C.N..ratio")]
names(LLN4b) <- c("Name", "X.N", "X.C", "C.N")
LLN4b <- LLN4b[!LLN4b$Name %in% c("Blank-","blank-", "runIn", "standard", "stop", "test", NA, "Acetanilide", "acetanilide", "check"),]
head(LLN4b)
tail(LLN4b)

# Run4b Plot
LLN4b$plot[substr(LLN4b$Name,1,1)=="B"] <- "B-127"  
LLN4b$plot[substr(LLN4b$Name,1,1)=="H"] <- "HH-115"  
LLN4b$plot[substr(LLN4b$Name,1,1)=="N"] <- "N-115"  
LLN4b$plot[substr(LLN4b$Name,1,1)=="U"] <- "U-134"  
LLN4b$plot <-as.factor(LLN4b$plot)
summary(LLN4b)

# Run4 Date
LLN4b$date_collection <- unlist(lapply(strsplit(LLN4b$Name, "_"), function(x){x[2]}))
LLN4b$date_collection <- as.Date(LLN4b$date_collection, format="%Y%m%d")
summary(LLN4b)

# Run4a Species
LLN4b$sci_name <- unlist(lapply(strsplit(LLN4b$Name, "_"), function(x){x[3]}))
LLN4b$sci_name <- car::recode(LLN4b$sci_name, "'QUAL'='Quercus alba'; 'QUAB'='Quercus alba'; 'QURU'='Quercus rubra'; 'ACSA'='Acer saccharum'; 'ASCA'='Acer saccharum'")
LLN4b$sci_name <- as.factor(LLN4b$sci_name)
summary(as.factor(LLN4b$sci_name))


LLN4a[LLN4a$plot=="U-134",]
LLN4b[LLN4b$plot=="U-134",]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Merging everything together!
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
datLLN <- rbind(LLN2018, LLN2122, LLN2, LLN3, LLN4a, LLN4b)
summary(datLLN)

#write.csv(datLLN, file.path(path.google, "URF REU 2025 - Lizer - Leaf Litter", "LeafLitter-Nitrogen_bySpecies_combined.csv"), row.names=F)
write.csv(datLLN, file.path ("~/Library/CloudStorage/GoogleDrive-lizer1@stolaf.edu/.shortcut-targets-by-id/1q2wvODXrDo0tgOTLpFqF7TqcWoKoHZjW/URF-REU 2025 - Lizer - Leaf Litter/", "LeafLitter-Nitrogen_bySpecies_combined.csv"), row.names=F)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
