library(googlesheets4)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)
#---- reading in sheets
B127 <- read_sheet('https://docs.google.com/spreadsheets/d/1Ez9itU4OX3aCSfRY_xTOCCJpOJj4QDlXC4fGCiRMCSc/edit?usp=drive_linkl', sheet = "B-127")
U134 <- read_sheet('https://docs.google.com/spreadsheets/d/1Ez9itU4OX3aCSfRY_xTOCCJpOJj4QDlXC4fGCiRMCSc/edit?usp=drive_link', sheet = "U-134")
N115 <- read_sheet('https://docs.google.com/spreadsheets/d/1Ez9itU4OX3aCSfRY_xTOCCJpOJj4QDlXC4fGCiRMCSc/edit?usp=drive_link', sheet = "N-115")
HH115 <- read_sheet('https://docs.google.com/spreadsheets/d/1Ez9itU4OX3aCSfRY_xTOCCJpOJj4QDlXC4fGCiRMCSc/edit?usp=drive_link', sheet = "HH-115")

#----putting them all together
allplots <-rbind(B127,U134,N115,HH115)
summary(allplots)

#---writing a function to make QAQC quicker
# this function sorts columns into tables diplaying the amount of times 
#information is displayed in the column
#it also changes missing or null values to NA

checks <- function(column) {
  if(is.list(column)) {
    column <- sapply(column, function(x) if(is.null(x)) NA else x)
  }
  if(is.character(column) || is.factor(column)) {
    column[column == ""] <- NA
    column[column == " "] <- NA
  }
  sort(table(column, useNA = "always"), decreasing = TRUE)
}

#--- Checks of how often information is appearing in the column 
# this is the janky QAQC of the script to try and catch obvious errors 
checks(allplots$`Survey Date`)
checks(allplots$`IMLS_Plot`)
checks(allplots$`Plot Location`)
checks(allplots$`Tag`)
checks(allplots$`Sp_code`)
checks(allplots$`2018 Status`)
checks(allplots$`2018 DBH`)
checks(allplots$`2025 DBH`)
checks(allplots$`2025 Canopy Class`)
checks(allplots$`2025 Vigor`)

#finding the location of the error in Plot name 
# Note I had to comment out the tag portion because the errors in plot name also
#do not contain a tag
plotcheck<- function() {
  values <- c("N-115", "B-127", "U-134", "HH-115")
  badrows <- which(!(allplots$`IMLS_Plot` %in% values) & 
                     !is.na(allplots$`IMLS_Plot`))
  rows <- data.frame(
    #Tag = allplots$Tag[badrows],
    IMLS_Plot = allplots$IMLS_Plot[badrows],
    plotcheck = allplots$`IMLS_Plot`[badrows]
  )
  rows
}
plotcheck()


#--finding the error in canopy class

cancheck<- function() {
 values <- c("C", "I", "U", "D", "DEAD")
  badrows <- which(!(allplots$`2025 Canopy Class` %in% values) & 
                      !is.na(allplots$`2025 Canopy Class`))
  rows <- data.frame(
    Tag = allplots$Tag[badrows],
    IMLS_Plot = allplots$IMLS_Plot[badrows],
    cancheck = allplots$`2025 Canopy Class`[badrows]
  )
  rows
}
cancheck()

#----Checking for repeat tags, first removing null or NA values, then checking
#which tags return true for double values
tags <- sapply(allplots$Tag, function(x) if(is.null(x)) NA else as.character(x))
any(table(tags, useNA = "no") > 1)


#--- Checking and Visualizing DBH
dat.dbh<- allplots[allplots$`2025 Canopy Class`!="DEAD" & allplots$`2018 Status` == "Alive",c("IMLS_Plot","Tag","Sp_code","Plot Location", "2018 Status","2018 DBH","2025 DBH","2025 Canopy Class")]


# Removin rows where either DBH column is NA or blank, also adding NA or blank for SP column since that was causing issues
dat.dbh <- dat.dbh[!is.na(dat.dbh$`2018 DBH`) & !is.na(dat.dbh$`2025 DBH`) & 
                     dat.dbh$`2018 DBH` != "" & dat.dbh$`2025 DBH` != "" &
                     !is.na(dat.dbh$Sp_code) & dat.dbh$Sp_code != "" & 
                     dat.dbh$Sp_code != " ", ]


# 2018 DBH and tag columns are lists so making sure dbh is numeric and Tag is chr
dat.dbh$`2018 DBH` <- as.numeric(unlist(dat.dbh$`2018 DBH`))
dat.dbh$`2025 DBH` <- as.numeric(dat.dbh$`2025 DBH`)
dat.dbh$Tag<-  as.character(unlist(dat.dbh$`Tag`))
#checking
str(dat.dbh$`2018 DBH`)
str(dat.dbh$`2025 DBH`)
str(dat.dbh$Tag)
str(dat.dbh$IMLS_Plot)

#---- Checking for DBH decrease form 2018 to 2025
holup<- which(dat.dbh$`2018 DBH` > dat.dbh$`2025 DBH`)
dbhtbl.dat<-dat.dbh[holup, c("IMLS_Plot","Plot Location", "Sp_code", "Tag", "2018 DBH", "2025 DBH")]

# Add the cm decrease in a column
dbhtbl.dat$`Decrease (cm)` <- dbhtbl.dat$`2025 DBH` - dbhtbl.dat$`2018 DBH`

grid.arrange(
  textGrob("Decrease in DBH (cm)", gp = gpar(fontsize = 14, fontface = "bold")),
  tableGrob(dbhtbl.dat, rows = NULL),
  heights = c(1, 10)
)



#----checking for DBH Increase form 2018 to 2025
itsup<- which(dat.dbh$`2018 DBH` < dat.dbh$`2025 DBH`)
dbhup.dat<-dat.dbh[itsup, c("IMLS_Plot","Plot Location", "Sp_code", "Tag", "2018 DBH", "2025 DBH")]

# Add the cm decrease in a column
dbhup.dat$`Increase (cm)` <- dbhup.dat$`2025 DBH` - dbhup.dat$`2018 DBH`

unique_plots <- unique(dbhup.dat$IMLS_Plot)

for(plot_name in unique_plots) {
  plot_data <- dbhup.dat[dbhup.dat$IMLS_Plot == unique_plots, ]
  
  grid.arrange(
    textGrob(paste("Increase in DBH (cm) -", plot_name), 
             gp = gpar(fontsize = 14, fontface = "bold")),
    tableGrob(plot_data, rows = NULL),
    heights = c(1, 100)
  )
}
dev.off()

#----plotting DBH 
ggplot(dat=dat.dbh)+
  facet_wrap(IMLS_Plot ~ ., scales = "free_x") +
  geom_col(aes(x = as.factor(Tag), y = `2018 DBH`), 
           alpha = 0.6, fill = "blue", width = 0.4, 
           position = position_nudge(x = -0.2)) +
  geom_col(aes(x = as.factor(Tag), y = `2025 DBH`), 
           alpha = 0.6, fill = "red", width = 0.4, 
           position = position_nudge(x = 0.2)) +
  labs(x = "Tag", y = "DBH (cm)", title = "DBH Comparison by Tag") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

# creating a loop to display individual plots 
unique_plots <- unique(dat.dbh$IMLS_Plot)

for(plot_name in unique_plots) {
  # Filter data
  plot_data <- dat.dbh[dat.dbh$IMLS_Plot == plot_name, ]
  
  # Create and display plot
  p <- ggplot(data = plot_data) +
    geom_col(aes(x = as.factor(Tag), y = `2018 DBH`), 
             alpha = 0.6, fill = "blue", width = 0.4, 
             position = position_nudge(x = -0.2)) +
    geom_col(aes(x = as.factor(Tag), y = `2025 DBH`), 
             alpha = 0.6, fill = "red", width = 0.4, 
             position = position_nudge(x = 0.2)) +
    labs(x = "Tag", y = "DBH (cm)", 
         title = paste("DBH Comparison by Tag", plot_name)) +
    theme(axis.text.x = element_text(angle = 75, hjust = 1))
  
  # Display the plot
  print(p)
}

dat.dbh$DBH_difference <- dat.dbh$`2025 DBH` - dat.dbh$`2018 DBH`

# Create histogram of DBH differences faceted by plot
ggplot(data = dat.dbh, aes(x = DBH_difference)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", alpha = 0.7, color = "black") +
  facet_wrap(~ IMLS_Plot, scales = "free_y") +
  labs(x = "DBH Difference (cm) [2025 - 2018]", 
       y = "Frequency", 
       title = "Distribution of DBH Change by Plot (2018 to 2025)") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.7)

# Create  new columns for years to make analysis easieee
dat.dbh.long <- data.frame(
  IMLS_Plot = rep(dat.dbh$IMLS_Plot, 2),
  Tag = rep(dat.dbh$Tag, 2),
  Sp_code = rep(dat.dbh$Sp_code, 2),
  Year = rep(c("2018", "2025"), each = nrow(dat.dbh)),
  DBH = c(dat.dbh$`2018 DBH`, dat.dbh$`2025 DBH`)
)

#  Graphin the Distribution of DBH by plot with a boxplot
ggplot(data = dat.dbh.long) +
  geom_boxplot(aes(x = IMLS_Plot, y = DBH, fill = Year)) +
  labs(x = "IMLS Plot", y = "DBH (cm)", title = "DBH Distribution by Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#graping mean DBH by plot with error
ggplot(data = dat.dbh.long) +
  stat_summary(aes(x = IMLS_Plot, y = DBH, fill = Year), 
               fun = mean, geom = "col", position = "dodge", alpha = 0.7) +
  stat_summary(aes(x = IMLS_Plot, y = DBH, group = Year), 
               fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "IMLS Plot", y = "Mean DBH (cm)", title = "Mean DBH by Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

############################
#calculate basal area for trees  and add them to column
# formula for basal area is dbh^2 * 0.005454

dat.dbh$`basal_area_25` <- dat.dbh$`2025 DBH`^2*0.005454
dat.dbh$`basal_area_18` <- dat.dbh$`2018 DBH`^2*0.005454

# Get basal area change for individual trees
dat.dbh$BA_diff <- dat.dbh$`basal_area_25` - dat.dbh$`basal_area_18`

dat.ba <- dat.dbh[dat.dbh$`Plot Location`=='core',c("IMLS_Plot","Tag","Sp_code", "Plot Location",
"basal_area_25", "basal_area_18", "BA_diff")]


# Aggreagate total basal area by plot for each year
ba_plot <- aggregate(cbind(basal_area_18, basal_area_25) ~ IMLS_Plot, 
                                data = dat.ba, 
                                FUN = sum)
# Calculate basal area change and percent change by plot
ba_plot$BA_change <- ba_plot$basal_area_25 - ba_plot$basal_area_18
ba_plot$BA_percent_change <- (ba_plot$BA_change / ba_plot$basal_area_18) * 100

ba_long <- data.frame(
  IMLS_Plot = rep(ba_plot$IMLS_Plot, 2),
  Year = rep(c("2018", "2025"), each = nrow(ba_plot)),
  Basal_Area = c(ba_plot$basal_area_18, ba_plot$basal_area_25))

# Bar chart comparing 2018 vs 2025 basal area by plot
ggplot(data = ba_long, aes(x = IMLS_Plot, y = Basal_Area, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.7, color = "black") +
  labs(x = "IMLS Plot", 
       y = "Total Basal Area (cm²)", 
       title = "Total Basal Area Comparison by Core Plot (2018 vs 2025)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_manual(values = c("2018" = "steelblue", "2025" = "coral"))

# Basal area change by plot
ggplot(data = ba_plot, aes(x = IMLS_Plot, y = BA_change)) +
  geom_col(aes(fill = ifelse(BA_change >= 0, "Increase", "Decrease")), 
           alpha = 0.7, color = "black") +
  labs(x = "IMLS Plot", 
       y = "Basal Area Change (cm²) [2025 - 2018]", 
       title = "Basal Area Change by Core Plot",
       fill = "Change Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Increase" = "steelblue", "Decrease" = "coral"))

# % change in basal area
ggplot(data = ba_plot, aes(x = IMLS_Plot, y = BA_percent_change)) +
  geom_col(aes(fill = ifelse(BA_percent_change >= 0, "Increase", "Decrease")), 
           alpha = 0.7, color = "black") +
  labs(x = "IMLS Plot", 
       y = "Basal Area Change (%)", 
       title = "Percent Change in Basal Area by Core Plot (2018 to 2025)",
       fill = "Change Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Increase" = "steelblue", "Decrease" = "coral"))


# Individual tree basal area changes by tag - faceted by plot
ggplot(data = dat.ba, aes(x = as.factor(Tag), y = BA_diff)) +
  geom_col(aes(fill = ifelse(BA_diff >= 0, "Increase", "Decrease")), 
           alpha = 0.7, color = "black", size = 0.3) +
  facet_wrap(~ IMLS_Plot, scales = "free_x") +
  labs(x = "Tag", 
       y = "Basal Area Change (cm²) [2025 - 2018]", 
       title = "Individual Tree Basal Area Change by Tag and Core Plot",
       fill = "Change Type") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 75, hjust = 1, size = 8),
        legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("Increase" = "steelblue", "Decrease" = "coral"))

# Histogram of individual tree basal area changes by plot
ggplot(data = dat.ba, aes(x = BA_diff)) +
  geom_histogram(binwidth = 0.01, fill = "lightblue", alpha = 0.7, color = "black") +
  facet_wrap(~ IMLS_Plot, scales = "free_y") +
  labs(x = "Individual Tree Basal Area Change (cm²)", 
       y = "Frequency", 
       title = "Distribution of Individual Tree Basal Area Changes by Core Plot") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")


# Getting Basal area figues by genus 

# Extractomg genus from species code by taking the first two letters
 dat.ba$Genus <- substr(dat.ba$Sp_code, 1, 2)

# Create data for stacked bar plot by genus
ba_genus_plot <- aggregate(cbind(basal_area_18, basal_area_25) ~ IMLS_Plot + Genus, 
                           data = dat.ba, 
                           FUN = sum)

# Create long format for stacked bar plot
ba_genus_long <- data.frame(
  IMLS_Plot = rep(ba_genus_plot$IMLS_Plot, 2),
  Genus = rep(ba_genus_plot$Genus, 2),
  Year = rep(c("2018", "2025"), each = nrow(ba_genus_plot)),
  Basal_Area = c(ba_genus_plot$basal_area_18, ba_genus_plot$basal_area_25)
)

# Stacked bar plot of basal area by genus, faceted by plot
ggplot(data = ba_genus_long, aes(x = Year, y = Basal_Area, fill = Genus)) +
  geom_col(position = "stack", alpha = 0.8, color = "black", size = 0.2) +
  facet_wrap(~ IMLS_Plot, scales = "free_y") +
  labs(x = "Year", 
       y = "Total Basal Area (cm²)", 
       title = "Basal Area by Genus and Year",
       fill = "Genus") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(size = 10)) +
  scale_fill_brewer(type = "qual", palette = "Set3")

# Alternative version: Stacked bar showing change (2025 - 2018) by genus
ba_genus_change <- aggregate(BA_diff ~ IMLS_Plot + Genus, 
                             data = dat.ba, 
                             FUN = sum)

ggplot(data = ba_genus_change, aes(x = IMLS_Plot, y = BA_diff, fill = Genus)) +
  geom_col(position = "stack", alpha = 0.8, color = "black", size = 0.2) +
  labs(x = "IMLS Plot", 
       y = "Basal Area Change (cm²) [2025 - 2018]", 
       title = "Basal Area Change by Genus and Core Plot",
       fill = "Genus") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_brewer(type = "qual", palette = "Set3")

