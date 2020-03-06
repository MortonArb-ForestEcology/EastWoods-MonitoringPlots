# Script to load and format Understory community data from the East Woods monitoring plots
get.understory <- function(YEAR, PLOTS=c("B-127", "U-134", "N-115", "HH-115")) {
  
  # Not elegant, but just storing the keys for spreadsheets here
  if(YEAR==2019) sheet.key = "1zAInAHJoMONezg2dM458fgNnNvYpFjygtGb1vjRq9mY"
  if(YEAR==2020) sheet.key = "1OWPKXBXvdfDZuvNPJFAViaBTVI4MO9i3Yb4uue5gzog"
  
  # Creating a blank data frame
  dat.pheno <- data.frame()
  
  # A vector with the metadata column names
  cols.meta <- c("Plot", "Subplot", "Name.Common", "Genus", "Species")
  
  for(i in 1:length(PLOTS)){
    dat.plot <- googlesheets4::read_sheet(ss=sheet.key, sheet=PLOTS[i])
    dat.plot <- dat.plot[!is.na(dat.plot$Name.Common),]
    summary(dat.plot)
    
    # Get index for columns for the different categories; [] makes it read the dot
    cols.cover <- grep("[.]Cover", names(dat.plot))
    cols.pheno <- grep("[.]Phenophase", names(dat.plot))
    cols.notes <- grep("[.]Notes", names(dat.plot))
    # cols.notes <- cols.notes[2:length(cols.notes)]
    
    # Coverting the data to a long format
    dat.long <- stack(dat.plot[,cols.cover])
    names(dat.long) <- c("Cover", "Obs.Date")
    dat.long$Obs.Date <- as.Date(substr(dat.long$Obs.Date, 1, 10), format="%Y.%m.%d")
    dat.long[, cols.meta] <- dat.plot[, cols.meta] 
    dat.long$Phenophase.Codes <- stack(dat.plot[,cols.pheno])[,1]
    dat.long$Notes <- stack(dat.plot[,cols.notes])[,1]
    dat.long <- dat.long[,c(cols.meta, "Obs.Date", "Cover", "Phenophase.Codes", "Notes")]
    # Get rid of empty values
    dat.long <- dat.long[!is.na(dat.long$Cover),]

    # Split Out Phenophases
    dat.long$Initial.Growth <- grepl("I", dat.long$Phenophase.Codes)
    dat.long$Leaves.Present <- grepl("L", dat.long$Phenophase.Codes)
    dat.long$Leaves.Colored <- grepl("C", dat.long$Phenophase.Codes)
    dat.long$Flowers.Buds <- grepl("B", dat.long$Phenophase.Codes)
    dat.long$Flowers.Open <- grepl("O", dat.long$Phenophase.Codes)
    dat.long$Fruits.Present <- grepl("F", dat.long$Phenophase.Codes)
    dat.long$Fruits.Ripe <- grepl("R", dat.long$Phenophase.Codes)
    dat.long$Fruits.Drop <- grepl("D", dat.long$Phenophase.Codes)
    summary(dat.long)
    
    dat.pheno <- rbind(dat.pheno, dat.long)
  }
  
  for(COL in cols.meta){
    dat.pheno[,COL] <- as.factor(toupper(dat.pheno[,COL]))
  }
  
  summary(dat.pheno)
  return(dat.pheno)
}


