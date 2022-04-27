
midtree.data <- read.csv("../data/Old EW Survey Data/East Woods Spring vegetation data.final.csv", na.strings="")

oldtree.data <- read.csv("../data/Old EW Survey Data/Old EW Tree Data.csv")

dat.volsurv <- googlesheets4::read_sheet("161i75Il4W3u8oJ6yMpY9fMfudHCNlamcD89SLj0u-hY", sheet = "Tree/Plot Metadata")

dat.volsurv$`Plot ID` <- gsub("-","",dat.volsurv$`Plot ID`)

midtree.data <- midtree.data[midtree.data$Plot.ID.Number %in% unique(dat.volsurv$`Plot ID`), 1:10]
oldtree.data <- oldtree.data[oldtree.data$ï..Plot %in% unique(dat.volsurv$`Plot ID`), 1:10]


dat.comb <- data.frame()
dat.diff <- data.frame()
for(PLOT in unique(dat.volsurv$`Plot ID`)){
  dat.temp <- dat.volsurv[dat.volsurv$`Plot ID` == PLOT,]
  dat.mid <- midtree.data[midtree.data$Plot.ID.Number == PLOT,]
  dat.old <- oldtree.data[oldtree.data$ï..Plot == PLOT,]
  
  tbl.temp <- as.data.frame(table(dat.temp$`Species Code`))
  colnames(tbl.temp) <- c("vol.species", "vol.num")
  tbl.temp$vol.sum <- sum(tbl.temp$vol.num)
  
  if(nrow(dat.mid) == 0){
    tbl.mid <- data.frame(NA, NA, NA)
    colnames(tbl.mid) <- c("mid.species", "mid.num", "mid.sum")
  } else{
  tbl.mid <- as.data.frame(table(dat.mid$Species))
  colnames(tbl.mid) <- c("mid.species", "mid.num")
  tbl.mid$mid.sum <- sum(tbl.mid$mid.num)
  }
  
  if(nrow(dat.old) == 0){
    tbl.old <- data.frame(NA, NA, NA)
    colnames(tbl.old) <- c("old.species", "old.num", "old.sum")
  } else{
  tbl.old <- as.data.frame(table(dat.old$Species))
  colnames(tbl.old) <- c("old.species", "old.num")
  tbl.old$old.sum <- sum(tbl.old$old.num)
  }
  dat.sum <- data.frame(unique(tbl.old$old.sum), unique(tbl.mid$mid.sum), unique(tbl.temp$vol.sum))
  colnames(dat.sum) <- c("old.sum", "mid.sum", "vol.sum")
  dat.sum$plot <- PLOT
  
  dat.both <- merge(tbl.mid, tbl.temp, by.x = "mid.species", by.y = "vol.species", all.x = T, all.y = T)
  dat.both$plot <- PLOT
  dat.comb <- rbind(dat.comb,dat.both)
  dat.diff <- rbind(dat.diff, dat.sum)
}

dat.diff$init.diff <- abs(dat.diff$old.sum - dat.diff$mid.sum)
dat.diff$recent.diff <- abs(dat.diff$mid.sum - dat.diff$vol.sum)

hist(dat.diff$init.diff)
hist(dat.diff$recent.diff)

dat.check <- dat.diff[dat.diff$diff >=3, ]
