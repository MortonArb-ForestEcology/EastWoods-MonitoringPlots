# LeafLitter_QAQC.R
# Standalone QAQC for the East Woods leaf litter "raw data YYYY" Google Sheets.
# Run this on its own (no need to source it from the read-in/cleaning script).
#
# What it does, for each year in yrsCheck:
#   1. Reads the "raw data YYYY" sheet
#   2. Cleans things that are safe to fix in code (stray spaces, "NA" text, name spellings,
#      renamed columns, number/date types). These fixes are NOT written back to the sheet.
#   3. Runs the checks and writes a flag file: one row per problem, with the Google Sheet row
#      number so you can go fix it at the source.
#
# Output (in path.qaqc):
#   LeafLitter_QAQC_flags_YYYY.csv      one file per year
#   LeafLitter_QAQC_flags_ALL.csv       all years together
#
# Severity levels in the flags:
#   "fix"    = almost certainly an error; correct it in the Google Sheet
#   "review" = could be fine, but someone should look at it
#
# Workflow: run this -> fix the "fix" rows in the Google Sheet -> re-run until clean ->
#           then run the read-in/cleaning script to save the archive CSVs.

library(googlesheets4)

################################################
# Settings ----
################################################
path.google <- "~/Google Drive/My Drive"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.qaqc   <- file.path(path.litter, "QAQC_flags")  # where the flag files go
if(!dir.exists(path.qaqc)) dir.create(path.qaqc, recursive=T)

#comment out other users emails for authentication
#googlesheets4::gs4_auth(email="crollinson@mortonarb.org")
googlesheets4::gs4_auth(email="breidy@mortonarb.org")

# The Google Drive key ID for the leaf litter spreadsheet
keyLeafLitter <- "1d7Py4ehN2PmrmKmyv2hDUkX4fWa95xdlQlGVBN9x20g"

yrsCheck <- 2023:2025

# TRUE = only check rows that have been weighed (skips pre-populated rows that aren't done yet)
onlyWeighed <- TRUE


################################################
# Reference values: edit these as the protocol changes ----
################################################
qaqc.plots <- c("B-127", "U-134", "N-115", "HH-115")

# Traps expected in each plot on every collection date
qaqc.traps <- list(
  "B-127"  = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
  "U-134"  = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
  "N-115"  = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
  "HH-115" = c("N", "NE30", "NE60", "E", "SE120", "SE150", "S", "W")
)

qaqc.tissues <- c("leaf", "fruit", "flower", "bract", "twig_bark", "other", "cicada", "EMPTY BAG", "MISSING DATA")

# Tissues that are recorded without a genus or species
qaqc.tissuesNoTaxon <- c("cicada")

# Genus + species combos seen so far. A new combo is flagged for review, not rejected:
# it is either a typo or a new taxon you should add here.
qaqc.taxa <- c("Acer saccharum", "Fraxinus americana", "Juglans nigra", "Ostrya virginiana",
               "Prunus serotina", "Quercus alba", "Quercus macrocarpa", "Quercus rubra",
               "Quercus unknown", "Tilia americana", "Ulmus americana", "Ulmus rubra",
               "unknown unknown")

# Spelling fixes for people's names: c("as typed" = "standard")
qaqc.nameFix <- c("Obrien" = "O'Brien", "Mcgladdery" = "McGladdery")

qaqc.colsRequired <- c("sorter", "date_collection", "plot", "trap_ID", "genus", "species", "tissue",
                       "num_fruit", "num_immature_fruit", "num_mature_fruit",
                       "weigher", "date_weighed", "mass_g", "data_entry", "notes")
qaqc.colsNumeric <- c("num_fruit", "num_immature_fruit", "num_mature_fruit", "mass_g")
qaqc.colsDate    <- c("date_collection", "date_weighed")

qaqc.collectWindow <- 7    # days; a plot collected within this many days of another plot counts as the same round

# Words in notes that suggest the trap was compromised (should be MISSING DATA, not a zero)
qaqc.notesCompromised <- "knock|fell|fallen|tipped|no bag|spill|torn|ripped|lost|missing"
# Words in notes that suggest a processing problem worth a look
qaqc.notesProblem     <- "forgot|mistak|crunch|estimat|wrong|error|redo|re-weigh|reweigh"


################################################
# cleanLeafLitter(): safe, automatic fixes ----
################################################
cleanLeafLitter <- function(dat){
  dat <- data.frame(dat, stringsAsFactors = FALSE, check.names = TRUE)
  if(!"sheet_row" %in% names(dat)){
    warning("No sheet_row column; adding one from row order. Add it right after read_sheet() so it matches the Google Sheet.")
    dat$sheet_row <- seq_len(nrow(dat)) + 1
  }
  
  # 1. Column names: rename near-matches (e.g. "Trap_ID", "trap.ID" -> "trap_ID"); add truly missing ones as NA
  normName <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
  for(COL in qaqc.colsRequired[!qaqc.colsRequired %in% names(dat)]){
    hit <- which(normName(names(dat)) == normName(COL))
    if(length(hit) == 1){
      message("Renaming column '", names(dat)[hit], "' to '", COL, "'")
      names(dat)[hit] <- COL
    } else {
      warning("Column '", COL, "' is missing from this sheet; adding it as empty (NA). Columns present: ",
              paste(names(dat), collapse = ", "))
      dat[[COL]] <- NA
    }
  }
  
  # 2. List columns (read_sheet makes these when a column has mixed text/numbers/dates)
  for(COL in names(dat)){
    if(!is.list(dat[[COL]])) next
    dat[[COL]] <- vapply(dat[[COL]], function(v){
      if(is.null(v) || length(v) == 0) return(NA_character_)
      v <- v[[1]]
      if(inherits(v, "POSIXt") || inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
      as.character(v)
    }, character(1), USE.NAMES = FALSE)
    message("Column '", COL, "' was a list column (mixed types in the sheet); converted to text")
  }
  
  # 3. Text cleanup: trim stray spaces, and treat "", "NA", "N/A" as missing
  nTrim <- integer(0)
  for(COL in names(dat)){
    if(is.factor(dat[[COL]])) dat[[COL]] <- as.character(dat[[COL]])
    if(!is.character(dat[[COL]])) next
    old <- dat[[COL]]
    new <- trimws(old)
    if(COL != "notes") new[new %in% c("", "NA", "N/A", "n/a")] <- NA
    new[new %in% ""] <- NA
    changed <- !is.na(old) & !is.na(new) & old != new
    if(any(changed)) nTrim[COL] <- sum(changed)
    dat[[COL]] <- new
  }
  if(length(nTrim) > 0) message("Trimmed extra spaces: ", paste0(names(nTrim), " (", nTrim, ")", collapse = ", "))
  
  # 4. Standardize people's names
  for(COL in intersect(c("sorter", "weigher", "data_entry"), names(dat))){
    hit <- dat[[COL]] %in% names(qaqc.nameFix)
    if(any(hit)){
      message("Standardized ", sum(hit), " name(s) in ", COL)
      dat[[COL]][hit] <- unname(qaqc.nameFix[dat[[COL]][hit]])
    }
  }
  
  # 5. Drop fully blank rows
  colsCore <- c("date_collection", "plot", "trap_ID", "genus", "tissue", "mass_g")
  blank <- rowSums(!is.na(dat[, colsCore])) == 0
  if(any(blank)){
    message("Dropped ", sum(blank), " blank row(s)")
    dat <- dat[!blank, ]
  }
  
  # 6. Numbers: convert, and report anything that isn't a number
  for(COL in qaqc.colsNumeric){
    if(is.numeric(dat[[COL]])) next
    x   <- as.character(dat[[COL]])
    num <- suppressWarnings(as.numeric(x))
    bad <- !is.na(x) & is.na(num)
    if(any(bad)) warning(COL, " has text where a number should be (set to NA). Sheet rows: ",
                         paste0(dat$sheet_row[bad], " ('", x[bad], "')", collapse = ", "))
    dat[[COL]] <- num
  }
  
  # 7. Dates: convert, and report anything that won't parse
  for(COL in qaqc.colsDate){
    if(inherits(dat[[COL]], "Date")) next
    if(inherits(dat[[COL]], "POSIXt")){ dat[[COL]] <- as.Date(dat[[COL]]); next }
    x <- as.character(dat[[COL]])
    d <- as.Date(x, format = "%Y-%m-%d")
    d[is.na(d)] <- as.Date(x[is.na(d)], format = "%m/%d/%Y")
    bad <- !is.na(x) & is.na(d)
    if(any(bad)) warning(COL, " has values that aren't dates (set to NA). Sheet rows: ",
                         paste0(dat$sheet_row[bad], " ('", x[bad], "')", collapse = ", "))
    dat[[COL]] <- d
  }
  
  rownames(dat) <- NULL
  dat
}


################################################
# qaqcLeafLitter(): checks that need a human ----
################################################
qaqcLeafLitter <- function(dat, YR){
  dat <- data.frame(dat, stringsAsFactors = FALSE)
  for(COL in names(dat)) if(is.factor(dat[[COL]])) dat[[COL]] <- as.character(dat[[COL]])
  if(!"sheet_row" %in% names(dat)) dat$sheet_row <- NA
  
  emptyFlags <- data.frame(sheet_row = integer(0), date_collection = character(0), plot = character(0),
                           trap_ID = character(0), genus = character(0), species = character(0),
                           tissue = character(0), mass_g = numeric(0), severity = character(0),
                           check = character(0), detail = character(0))
  if(nrow(dat) == 0){
    message(YR, ": no weighed rows to check")
    return(emptyFlags)
  }
  
  flags <- list()
  # Flag specific rows (idx is a logical vector the length of dat)
  addFlag <- function(idx, check, severity, detail = NA_character_){
    idx[is.na(idx)] <- FALSE
    if(!any(idx)) return(invisible(NULL))
    if(length(detail) == length(idx)) detail <- detail[idx]
    flags[[length(flags) + 1]] <<- data.frame(
      sheet_row = dat$sheet_row[idx], date_collection = as.character(dat$date_collection[idx]),
      plot = dat$plot[idx], trap_ID = dat$trap_ID[idx], genus = dat$genus[idx],
      species = dat$species[idx], tissue = dat$tissue[idx], mass_g = dat$mass_g[idx],
      severity = severity, check = check, detail = detail, stringsAsFactors = FALSE)
  }
  # Flag something that has no row (e.g. a trap that was never entered)
  addGap <- function(date_collection, plot, trap_ID, check, severity, detail){
    flags[[length(flags) + 1]] <<- data.frame(
      sheet_row = NA, date_collection = as.character(date_collection), plot = plot, trap_ID = trap_ID,
      genus = NA, species = NA, tissue = NA, mass_g = NA,
      severity = severity, check = check, detail = detail, stringsAsFactors = FALSE)
  }
  
  isEmpty    <- dat$tissue %in% "EMPTY BAG"
  isMissing  <- dat$tissue %in% "MISSING DATA"
  isMaterial <- !is.na(dat$tissue) & !isEmpty & !isMissing
  isFruit    <- dat$tissue %in% "fruit"
  trapKey    <- paste(dat$date_collection, dat$plot, dat$trap_ID)
  
  # ---- Required fields ----
  for(COL in c("date_collection", "plot", "trap_ID", "tissue"))
    addFlag(is.na(dat[[COL]]), paste("missing", COL), "fix")
  for(COL in c("sorter", "weigher", "data_entry"))
    addFlag(is.na(dat[[COL]]), paste("missing", COL), "review")
  addFlag(isMaterial & !dat$tissue %in% qaqc.tissuesNoTaxon & (is.na(dat$genus) | is.na(dat$species)),
          "missing genus or species", "fix", "use 'unknown' if it can't be identified")
  
  # ---- Valid values ----
  addFlag(!is.na(dat$plot) & !dat$plot %in% qaqc.plots, "plot not in plot list", "fix")
  trapOK <- mapply(function(p, t){
    if(is.na(p) || is.na(t) || !p %in% names(qaqc.traps)) return(TRUE)
    t %in% qaqc.traps[[p]]
  }, dat$plot, dat$trap_ID, USE.NAMES = FALSE)
  addFlag(!trapOK, "trap_ID not valid for this plot", "fix")
  addFlag(!is.na(dat$tissue) & !dat$tissue %in% qaqc.tissues, "tissue not in tissue list", "fix")
  taxon <- paste(dat$genus, dat$species)
  addFlag(isMaterial & !is.na(dat$genus) & !is.na(dat$species) & !taxon %in% qaqc.taxa,
          "new genus/species combination", "review", "typo, or a new taxon to add to qaqc.taxa")
  addFlag(dat$genus %in% "unknown" & !is.na(dat$species) & !dat$species %in% "unknown",
          "species given but genus is unknown", "fix")
  addFlag(dat$tissue %in% "bract" & !dat$genus %in% "Tilia", "bract that isn't Tilia", "review")
  
  # ---- Dates ----
  yrColl <- as.numeric(format(dat$date_collection, "%Y"))
  addFlag(!is.na(yrColl) & yrColl != YR, "collection year doesn't match sheet year", "fix",
          paste("collection year", yrColl))
  addFlag(dat$date_weighed < dat$date_collection, "weighed before it was collected", "fix")
  addFlag(dat$date_collection > Sys.Date(), "collection date is in the future", "fix")
  
  # ---- Completeness: every trap in every plot should have at least one row per collection ----
  ok <- !is.na(dat$date_collection) & dat$plot %in% names(qaqc.traps)
  datePlot <- unique(dat[ok, c("date_collection", "plot")])
  for(i in seq_len(nrow(datePlot))){
    D <- datePlot$date_collection[i]; P <- datePlot$plot[i]
    have <- unique(dat$trap_ID[ok & dat$date_collection == D & dat$plot == P])
    miss <- setdiff(qaqc.traps[[P]], have)
    if(length(miss) > 0)
      addGap(D, P, paste(miss, collapse = ", "), "trap(s) with no rows for this collection", "fix",
             "add an EMPTY BAG or MISSING DATA row, or check whether it was entered under another trap_ID")
  }
  # Plots skipped in a collection round (a plot collected within qaqc.collectWindow days counts)
  datesAll <- sort(unique(dat$date_collection[ok]))
  for(D in as.list(datesAll)){
    for(P in qaqc.plots){
      dP <- unique(dat$date_collection[ok & dat$plot == P])
      if(!any(abs(as.numeric(dP - D)) <= qaqc.collectWindow))
        addGap(D, P, NA, "plot not collected in this round", "review",
               paste0("no collection for this plot within ", qaqc.collectWindow,
                      " days; typo in the date, or a real skipped/extra collection?"))
    }
  }
  
  # ---- Duplicates ----
  # Same taxon + tissue in the same trap is normal (separate bags); same mass too suggests double entry
  key  <- paste(trapKey, dat$genus, dat$species, dat$tissue, dat$mass_g, sep = "|")
  dupe <- !is.na(dat$tissue) & (duplicated(key) | duplicated(key, fromLast = TRUE))
  addFlag(dupe, "duplicate entry (same date/plot/trap/taxon/tissue/mass)", "review",
          "probably entered twice; delete one, or check whether one belongs to a different trap")
  
  # ---- EMPTY BAG / MISSING DATA logic ----
  matKeys <- unique(trapKey[isMaterial])
  addFlag(isEmpty & trapKey %in% matKeys, "EMPTY BAG but the trap also has material", "fix")
  addFlag(isMissing & trapKey %in% matKeys, "MISSING DATA but the trap also has material", "fix")
  addFlag(isEmpty & !is.na(dat$mass_g) & dat$mass_g > 0, "EMPTY BAG has a mass", "fix")
  addFlag(isMissing & !is.na(dat$mass_g), "MISSING DATA has a mass", "fix", "should stay NA, not 0")
  addFlag((isEmpty | isMissing) & (!is.na(dat$genus) | !is.na(dat$species)),
          "EMPTY BAG / MISSING DATA row has a taxon", "review")
  addFlag(grepl(qaqc.notesCompromised, dat$notes, ignore.case = TRUE) & !isMissing,
          "notes suggest the trap was compromised", "review",
          paste0("notes: '", dat$notes, "'; should this be MISSING DATA?"))
  
  # ---- Mass ----
  addFlag(isMaterial & is.na(dat$mass_g), "material with no mass", "review",
          paste0("keep as NA (not 0); notes: '", dat$notes, "'"))
  addFlag(!is.na(dat$mass_g) & dat$mass_g < 0, "negative mass", "fix")
  addFlag(isMaterial & dat$mass_g %in% 0 & !grepl("did not mass", dat$notes, ignore.case = TRUE),
          "mass is 0 but notes don't say 'did not mass'", "fix", "add 'did not mass' to notes")
  addFlag(!is.na(dat$mass_g) & abs(dat$mass_g * 100 - round(dat$mass_g * 100)) > 1e-6,
          "mass has more than 2 decimal places", "review", "typo?")
  
  # ---- Fruit counts ----
  hasCount <- !is.na(dat$num_fruit) | !is.na(dat$num_immature_fruit) | !is.na(dat$num_mature_fruit)
  addFlag(!isFruit & hasCount, "fruit counts on a non-fruit row", "fix")
  addFlag(isFruit & is.na(dat$num_fruit), "fruit row with no num_fruit", "review")
  addFlag(isFruit & !is.na(dat$num_fruit) & (is.na(dat$num_immature_fruit) | is.na(dat$num_mature_fruit)),
          "fruit row missing immature/mature counts", "review")
  sumOK <- dat$num_immature_fruit + dat$num_mature_fruit == dat$num_fruit
  addFlag(isFruit & !sumOK, "immature + mature doesn't equal num_fruit", "fix",
          paste0(dat$num_immature_fruit, " + ", dat$num_mature_fruit, " vs ", dat$num_fruit,
                 "; notes: '", dat$notes, "'"))
  for(COL in c("num_fruit", "num_immature_fruit", "num_mature_fruit"))
    addFlag(!is.na(dat[[COL]]) & (dat[[COL]] < 0 | dat[[COL]] != round(dat[[COL]])),
            paste(COL, "is negative or not a whole number"), "fix")
  
  # ---- Notes that mention a processing problem ----
  addFlag(grepl(qaqc.notesProblem, dat$notes, ignore.case = TRUE), "notes mention a processing problem",
          "review", paste0("notes: '", dat$notes, "'"))
  
  # ---- Combine ----
  if(length(flags) == 0){
    message(YR, ": no QAQC flags")
    return(emptyFlags)
  }
  out <- do.call(rbind, flags)
  out <- out[order(out$severity != "fix", out$check, out$sheet_row), ]
  rownames(out) <- NULL
  message(YR, ": ", sum(out$severity == "fix"), " 'fix' flags and ", sum(out$severity == "review"), " 'review' flags")
  print(table(out$check, out$severity))
  out
}


################################################
# Run the QAQC ----
################################################
flagsAll <- list()
for(YR in yrsCheck){
  print(YR)
  datLeafLitter <- tryCatch(
    googlesheets4::read_sheet(ss=keyLeafLitter, sheet=paste("raw data", YR)),
    error = function(e){ message("Couldn't read 'raw data ", YR, "': ", conditionMessage(e)); NULL })
  if(is.null(datLeafLitter) || nrow(datLeafLitter)==0) next
  
  datLeafLitter <- data.frame(datLeafLitter)
  datLeafLitter$sheet_row <- seq_len(nrow(datLeafLitter)) + 1   # row number in the Google Sheet (+1 for header)
  datLeafLitter <- cleanLeafLitter(datLeafLitter)
  
  if(onlyWeighed){
    datLeafLitter <- datLeafLitter[!is.na(datLeafLitter$date_weighed) & datLeafLitter$date_weighed<=Sys.Date(),]
  }
  
  flags <- qaqcLeafLitter(datLeafLitter, YR)
  write.csv(flags, file.path(path.qaqc, paste0("LeafLitter_QAQC_flags_", YR, ".csv")), row.names=F)
  if(nrow(flags) > 0) flagsAll[[as.character(YR)]] <- cbind(sheet_year = YR, flags)
}

if(length(flagsAll) > 0){
  flagsAll <- do.call(rbind, flagsAll)
  rownames(flagsAll) <- NULL
  write.csv(flagsAll, file.path(path.qaqc, "LeafLitter_QAQC_flags_ALL.csv"), row.names=F)
  
  # Summary: number of flags per check, by year
  print(table(flagsAll$check, flagsAll$sheet_year))
  # To look through them View(flagsAll)
}
################################################