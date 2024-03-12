# Script to load and format Understory community data from the East Woods monitoring plots
get.understory <- function(YEAR, PLOTS=c("B-127", "U-134", "N-115", "HH-115")) {
  
  # Not elegant, but just storing the keys for spreadsheets here
  if(YEAR==2019) sheet.key = "1zAInAHJoMONezg2dM458fgNnNvYpFjygtGb1vjRq9mY"
  if(YEAR==2020) sheet.key = "1OWPKXBXvdfDZuvNPJFAViaBTVI4MO9i3Yb4uue5gzog"
  if(YEAR==2021) sheet.key = "1Dw5vM2QvGuXlv1NxH64wDVv2Cq-oD8E9E2vbcq500lg"
  if(YEAR==2022) sheet.key = "1E4tXvq-j0jbzdSBygsyN5ty2TfPqai5Cx_2KlVbz7K0"
  if(YEAR==2023) sheet.key = "1DeTGD_zf5hEMrGlxmjU0eavEJ7fEpxDvjHe2hTaUqYE"
  if(YEAR==2024) sheet.key = "1ELs5aMlAiQuXFha0ZtYXTMWxgyCZpSWQhnMgjRemzPQ"
  
  # Creating a blank data frame
  dat.pheno <- data.frame()
  
  # A vector with the metadata column names
  cols.meta <- c("Plot", "Subplot", "Name.Common", "Genus", "Species")
  
  for(i in 1:length(PLOTS)){
    dat.plot <- googlesheets4::read_sheet(ss=sheet.key, sheet=PLOTS[i])
    dat.plot <- as.data.frame(dat.plot[!is.na(dat.plot$Name.Common) & !dat.plot$Genus %in% c("VEG", "BARE", "RESTRICTED", "BURN"),])
    # dat.plot[is.null(dat.plot)] <- NA
    summary(dat.plot)
    
    # Get index for columns for the different categories; [] makes it read the dot
    cols.cover <- grep("[.]Cover", names(dat.plot))
    cols.pheno <- grep("[.]Phenophase", names(dat.plot))
    cols.notes <- grep("[.]Notes", names(dat.plot))
    length(cols.cover); length(cols.pheno); length(cols.notes)
    if(length(cols.cover)!=length(cols.pheno) |  length(cols.cover)!=length(cols.notes)) stop("Check column names! Uneven number of cover/phenophase/notes columns")
    # cols.notes <- cols.notes[2:length(cols.notes)]
    
    
    # Coverting the data to a long format
    if(length(cols.cover)>1){
      # NOTE: If you get issues here, there's probably a problem with the headers OR there's a text string in one of the cover columns that makes values NULL instead of NA.
      dat.long <- stack(dat.plot[,cols.cover], drop=F)
      names(dat.long) <- c("Cover", "Obs.Date")
      dat.long$Obs.Date <- as.Date(substr(dat.long$Obs.Date, 1, 10), format="%Y.%m.%d")
      dat.long[, cols.meta] <- dat.plot[, cols.meta] 
      dat.long$Phenophase.Codes <- stack(dat.plot[,cols.pheno], drop=F)[,1]
      dat.long$Notes <- stack(dat.plot[,cols.notes], drop=F)[,1]
      # # # Diagnosing problematic columns
      # test <- stack(dat.plot[,cols.cover[1:14]], drop=F) # col 15 is the problem
      # dat.plot[,cols.cover[15]]
      # names(dat.plot)[cols.cover[15]]
    } else {
      dat.long <- data.frame(Cover=dat.plot[,cols.cover],
                             Obs.Date = as.Date(substr(names(dat.plot)[cols.cover], 1, 10), format="%Y.%m.%d"))
      dat.long[, cols.meta] <- dat.plot[, cols.meta] 
      dat.long[,c("Phenophase.Codes", "Notes")] <- dat.plot[,c(cols.pheno, cols.notes)]

    }
    
    
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
  dat.pheno[is.na(dat.pheno$Species),]
  
  return(dat.pheno)
}


