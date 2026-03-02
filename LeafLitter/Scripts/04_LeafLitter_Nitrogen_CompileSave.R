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


######################################################
# Making a standard format for each run ----
######################################################
# Assign plot based on first letter of sample Name
assign_plot <- function(df) {
  df$plot <- NA_character_
  df$plot[substr(df$Name, 1, 1) == "B"] <- "B-127"
  df$plot[substr(df$Name, 1, 1) == "H"] <- "HH-115"
  df$plot[substr(df$Name, 1, 1) == "N"] <- "N-115"
  df$plot[substr(df$Name, 1, 1) == "U"] <- "U-134"
  
  df$plot <- factor(df$plot)
  
  if (any(is.na(df$plot))) {
    warning("Some samples have NA plot assignments")
  }
  
  df
}

# Remove blanks, standards, test runs, and other non-samples
clean_samples <- function(df) {
  bad <- c(
    "Blank-", "blank-", "runIn", "RunIn", "standard",
    "stop", "test", "Test", "TEST",
    "Acetanilide", "acetanilide", "check", NA
  )
  
  df[!df$Name %in% bad, ]
}

# Recode species abbreviations to scientific names
recode_species <- function(x) {
  car::recode(
    x,
    "'QA'='Quercus alba';
     'QUAL'='Quercus alba';
     'QUAB'='Quercus alba';
     'QURU'='Quercus rubra';
     'TA'='Tilia americana';
     'TIAM'='Tilia americana';
     'AS'='Acer saccharum';
     'ACSA'='Acer saccharum';
     'ASCA'='Acer saccharum'"
  )
}


# Extract collection date from Name field
extract_date <- function(df, type = c("underscore", "2018")) {
  type <- match.arg(type)
  
  if (type == "underscore") {
    df$date_collection <- as.Date(
      sapply(strsplit(df$Name, "_"), `[`, 2),
      format = "%Y%m%d"
    )
  }
  
  if (type == "2018") {
    df$date_collection <- as.Date(
      substr(df$Name, 3, 10),
      format = "%y-%m-%d"
    )
  }
  
  if (any(is.na(df$date_collection))) {
    warning("Some dates failed to parse")
  }
  
  df
}


# Extract species code from Name field
extract_species <- function(df, type = c("underscore", "2018")) {
  type <- match.arg(type)
  
  if (type == "underscore") {
    df$sci_name <- sapply(strsplit(df$Name, "_"), `[`, 3)
  }
  
  if (type == "2018") {
    df$sci_name <- substr(df$Name, 11, 12)
  }
  
  df$sci_name <- factor(recode_species(df$sci_name))
  df
}


format_run <- function(path, file, n_rows = NULL) {
  # Read file
  df <- as.data.frame(read_xlsx(file.path(path, file)))
  # Optionally restrict rows (Run 4 had to only take 1st 70 samples)
  if (!is.null(n_rows)) {
    df <- df[seq_len(min(n_rows, nrow(df))), ]
  }
  # Keep and rename CN columns
  df <- df[, c("Name", "N  [%]", "C  [%]", "C/N  ratio")]
  names(df) <- c("Name", "X.N", "X.C", "C.N")
  # Standard CN cleaning
  df %>%
    clean_samples() %>%
    assign_plot() %>%
    extract_date("underscore") %>%
    extract_species("underscore")
}


######################################################
# Clean 2018  (ran by lucien with different naming) ----
######################################################
LLN2018 <- read.csv(file.path(path.CN, "Fall_2018_run1.csv")) %>%
  assign_plot() %>%
  extract_date(type = "2018") %>%
  extract_species(type = "2018")

summary(LLN2018)

######################################################
# Clean Cierra Runs (2018-2023) ----
######################################################

LLN2122 <- format_run(path.CN, "Lizer_EWLL_2021-2022_run.xlsx")
summary(LLN2122)
LLN2 <- format_run(path.CN, "Lizer_EWLL_run2_0715_corrected.xlsx")
summary(LLN2)
LLN3 <- format_run(path.CN, "Lizer_EWLL_Run3_SH_072125.xlsx")
summary(LLN3)
LLN4a <- format_run(path.CN, "Lizer_EWLL_run_4.xlsx", n_rows = 70)
summary(LLN4a)
LLN4b <- format_run(path.CN, "Lizer_EWLL_plate_4_rerun.xlsx")
summary(LLN4b)
LLN5 <- format_run(path.CN, "Lizer_EWLL_plate5_012626_rerun.xlsx")
summary(LLN5)


######################################################
# Plate 6-8 inconsistensies (naming and soils lab samples)
######################################################

# Normalize inconsistent Name formatting
normalize_name <- function(name) {
  # Remove leading/trailing whitespace
  name <- trimws(name)
  # Replace spaces with underscores HH15 20201029_TIAM
  name <- gsub("\\s+", "_", name)
  # Replace hyphen before species with underscore B127_20191104-ACSA -> B127_20191104_ACSA
  name <- gsub("-(QUAL|QUAB|QURU|ACSA|TIAM)$", "_\\1", name)
  # Fix dates split by space: _2023 1020_ -> _20231020_
  name <- gsub("_(\\d{4})_(\\d{4})_", "_\\1\\2_", name)
  # Fix dates split by underscore: _2021_0702_ -> _20210702_
  name <- gsub("_(\\d{4})_(\\d{2})(\\d{2})_", "_\\1\\2\\3_", name)
  # Fix missing plot digit (known issue in plate 7)
  name <- gsub("^HH15_", "HH115_", name)
  name
}

# Flag samples that follow expected naming rules
flag_invalid_samples <- function(df) {
  df$valid_sample <-
    grepl("^(B127|HH115|N115|U134)_", df$Name) &
    grepl("_(20\\d{6})_", df$Name) &
    grepl("_(QUAL|QUAB|QURU|ACSA|TIAM)$", df$Name)
  
  df
}

LLN6_raw <- read_xlsx(file.path(path.CN, "lizer_plate6_soil_am.xlsx")) %>%
  as.data.frame()

#The machine turned off in the middle, values not accurate but re-ran in run 7
LLN6_raw <- LLN6_raw[1:75,]

LLN6 <- LLN6_raw %>%
  transform(Name = normalize_name(Name)) %>%
  flag_invalid_samples()

# Inspect and drop soil samples
LLN6[!LLN6$valid_sample, ]
LLN6 <- LLN6[LLN6$valid_sample, ]
LLN6$valid_sample <- NULL

# Now format like a normal CN run
LLN6 <- LLN6 %>%
  clean_samples() %>%
  assign_plot() %>%
  extract_date("underscore") %>%
  extract_species("underscore")

LLN6 <- LLN6[, c("Name", "N  [%]", "C  [%]", "C/N  ratio", "plot", "date_collection", "sci_name")]
names(LLN6) <- c("Name", "X.N", "X.C", "C.N", "plot", "date_collection", "sci_name")


summary(LLN6)


#### Run 7
LLN7_raw <- read_xlsx(file.path(path.CN, "Lizer_EWLL_plate7.xlsx")) %>%
  as.data.frame()

LLN7 <- LLN7_raw %>%
  transform(Name = normalize_name(Name)) %>%
  flag_invalid_samples()

# Inspect and drop soil samples
LLN7[!LLN7$valid_sample, ]

LLN7 <- LLN7 %>%
  clean_samples() %>%
  assign_plot() %>%
  extract_date("underscore") %>%
  extract_species("underscore")

LLN7 <- LLN7[, c("Name", "N  [%]", "C  [%]", "C/N  ratio", "plot", "date_collection", "sci_name")]
names(LLN7) <- c("Name", "X.N", "X.C", "C.N", "plot", "date_collection", "sci_name")



#Plate 8
LLN8_raw <- read_xlsx(file.path(path.CN, "Lizer_plate8.xlsx")) %>%
  as.data.frame()


LLN8_corrected <- LLN8_raw[83, "Name"] <- "B127_20191118_ACSA" #Sav didn't name this sample but we know what it was 
LLN8_corrected <- LLN8_raw[-c(21:24), ] #these ran as blanks so we need to remove

  
LLN8 <- LLN8_corrected %>%
  transform(Name = normalize_name(Name)) %>%
  flag_invalid_samples()

LLN8 <- LLN8 %>%
  clean_samples() %>%
  assign_plot() %>%
  extract_date("underscore") %>%
  extract_species("underscore")

LLN8 <- LLN8[, c("Name", "N  [%]", "C  [%]", "C/N  ratio", "plot", "date_collection", "sci_name")]
names(LLN8) <- c("Name", "X.N", "X.C", "C.N", "plot", "date_collection", "sci_name")

summary(LLN8)


#Needed to rerun a couple samples
Run8_reruns_raw <- read_xlsx(file.path(path.CN, "FP2023_LL_9,14,16_Lizerrerun.xlsx")) %>%
  as.data.frame()


Run8_reruns <- Run8_reruns_raw[69:70, ] %>%
  mutate(Name = case_when(
    row_number() == 1 ~ "HH115_20211105_TIAM",
    row_number() == 2 ~ "HH115_20191025_TIAM"
  ))
summary(Run8_reruns)

Run8_reruns <- Run8_reruns %>%
  clean_samples() %>%
  assign_plot() %>%
  extract_date("underscore") %>%
  extract_species("underscore")

Run8_reruns <- Run8_reruns[, c("Name", "N  [%]", "C  [%]", "C/N  ratio", "plot", "date_collection", "sci_name")]
names(Run8_reruns) <- c("Name", "X.N", "X.C", "C.N", "plot", "date_collection", "sci_name")

summary(Run8_reruns)


## Removing duplicates ----

#2019 outlier in LLN4a rerun in LLN8, taking this out of LLN4a
LLN4a <- LLN4a %>%
  filter(!(Name == "B127_20191118_ACSA"))
summary(LLN4a)

LLN8 <- LLN8 %>%
  filter(!(Name %in% c("HH115_20251025_TIAM", "HH115_20211105_TIAM")))
summary(LLN8)


######################################################
# Combine all runs ----
######################################################

datLLN <- rbind(LLN2018, LLN2122, LLN2, LLN3, LLN4a, LLN4b, LLN5, LLN6, LLN7, LLN8, Run8_reruns)

stopifnot(
  all(c("Name", "X.N", "X.C", "C.N",
        "plot", "date_collection", "sci_name") %in% names(datLLN)))

summary(datLLN)

write.csv(datLLN, file.path ("~/Library/CloudStorage/GoogleDrive-lizer1@stolaf.edu/.shortcut-targets-by-id/1q2wvODXrDo0tgOTLpFqF7TqcWoKoHZjW/URF-REU 2025 - Lizer - Leaf Litter/", "LeafLitter-Nitrogen_bySpecies_combined.csv"), row.names=F) 




