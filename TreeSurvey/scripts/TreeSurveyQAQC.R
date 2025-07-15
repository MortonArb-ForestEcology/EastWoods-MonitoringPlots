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

\)

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
