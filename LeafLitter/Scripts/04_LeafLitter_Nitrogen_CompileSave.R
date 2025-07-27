

LLN2122path<-list.files(cierra.path, pattern = "Lizer_EWLL_2021-2022_run.xlsx", full.names = TRUE)
LLN2122 <- read_excel(LLN2122path)
head(LLN2122)


LLN18path<-list.files(cierra.path, pattern = "Fall_2018_run1.csv", full.names = TRUE)
LLN18 <- read_csv(LLN18path)
head(LLN18)

LLN2122_clean <- LLN2122 %>%
  rename(
    CN_ratio = `C/N  ratio`,
    N_percent = `N  [%]`,
    C_percent = `C  [%]`
  ) %>%
  
  mutate(
    spec_id = str_sub(Name, -4), # Get the last 4 characters
    date_raw = str_sub(Name, -13, -5), #finds the 8 characters before the last underscore (5th to last character)
    #note: will give year, month, day in a string together
    plot = str_extract(Name, "^[^_]+") # Extracts characters from start (^) until the first underscore (_)
  ) %>%
  #convert date_raw into a readable year format
  mutate(
    date = ymd(date_raw),
    year = year(date),
    week = isoweek(date)
  ) %>%
  mutate(year = factor(year)) %>%
  select(
    Name, plot, date, year, week, spec_id, everything(), -date_raw # Keep original, then new columns, then rest
  )

print(LLN2122_clean)

LLN18_clean <- LLN18 %>%
  rename(
    CN_ratio = `C:N`,
    N_percent = `%N`,
    C_percent = `%C`
  ) %>%
  mutate(
    spec_id_raw = str_sub(Name, 11,12),
    year_2dig = str_sub(Name, 3, 4),
    month_2dig = str_sub(Name, 6,7),
    day_2dig = str_sub(Name, 9,10),
    plot_raw = str_sub(Name, 1,1)
  ) %>%
  mutate(
    spec_id = case_when(
      spec_id_raw == "QA" ~ "QUAL",
      spec_id_raw == "TA" ~ "TIAM",
      spec_id_raw == "AS" ~ "ACSA",
      TRUE ~ spec_id_raw
    ), 
    plot = case_when(
      plot_raw == "H" ~ "HH115",
      plot_raw == "N" ~ "N115",
      plot_raw == "U" ~ "U134",
      plot_raw == "B" ~ "B127",
      TRUE ~ plot_raw
    )
  ) %>% 
  mutate(
    date_raw = paste0("20", year_2dig, month_2dig, day_2dig)
  ) %>%
  # convert date_raw into a readable year format
  mutate(
    date = ymd(date_raw),
    year = year(date),
    week = isoweek(date)
  ) %>%
  mutate(year = factor(year)) %>%
  select(
    Name, plot, date, year, week, spec_id, CN_ratio, N_percent, C_percent, everything(),
    -year_2dig, -month_2dig, -day_2dig, -date_raw, -plot_raw, -spec_id_raw
  )

print(LLN18_clean)
unique(LLN18_clean$spec_id)
unique(LLN18_clean$plot)

LLN2122_clean <- LLN2122_clean %>%
  mutate(
    spec_id = case_when(
      spec_id == "QUAB" ~ "QUAL", # If spec_id is "QUAB", change it to "QUAL"
      TRUE ~ spec_id
    )
  )


LLN2122_clean <- LLN2122_clean %>%
  mutate(
    plot = case_when(
      plot == "H115" ~ "HH115",
      TRUE ~ plot
    )
  )

unique(LLN2122_clean$spec_id)
unique(LLN2122_clean$plot)


LLNdattot <- bind_rows(LLN2122_clean, LLN18_clean)

dim(LLNdattot)
unique(LLNdattot$year)


LLN_spec <- LLNdattot %>%
  filter(spec_id != "TIAM")

unique(LLN_spec$spec_id)
dim(LLN_spec)
