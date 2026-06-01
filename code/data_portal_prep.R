# Set folder path
library(here)

# Load configuration file (contains functions and settings used throughout project)
source(paste0(here(), "/code/config.R"))



# -------------------------
# Extract tables from Weekly Deaths Excel file
# -------------------------

# Path to latest weekly deaths Excel file
# NOTE: File will need to be updated each week
excel_file <- "T:/Projects/106 - VARS Weekly Deaths/data/Weekly_Deaths - w e 15 May 2026_0.xlsx"

# List of sheet names to extract from Excel
sheet_names <- c(
  "Table 1a",
  "Table 1b",
  "Table 2",
  "Table 3",
  "Table 4",
  "Table 5",
  "Table 6"
)


# Extract tables using custom function
# header_row = 3 specifies row containing column headers
tables <- extract_tables_from_excel(
  file_path = excel_file,
  sheets = sheet_names,
  header_row = 3
)



# -------------------------
# Clean table names and assign to environment
# -------------------------

# Function to standardise names:
# - lower case
# - replace spaces with underscores
# - remove special characters
# - add "df_" prefix for consistency

clean_names <- function(x) {
  x %>%
    tolower() %>%
    gsub("\\s+", "_", .) %>%
    gsub("[^a-z0-9_]", "", .) %>%
    paste0("df_", .)
}


# Apply cleaned names to tables
names(tables) <- clean_names(names(tables))

# Assign each table as a separate dataframe in global environment
list2env(tables, envir = .GlobalEnv)

# -------------------------
# Clean and process tables
# -------------------------


# ---- Table 1a ----
# Convert Excel serial dates to R Date format
# Remove "Total to date" row (not needed for analysis)
df_table_1a <- f_date_conversion(df_table_1a)
df_table_1a <- subset(df_table_1a, `Week Ending (Friday)` != "Total to date") 


# ---- Table 1b ----
df_table_1b <- f_date_conversion(df_table_1b)
df_table_1b <- subset(df_table_1b, `Week Ending (Friday)` != "Total to date") 


# ---- Table 2 (Sex breakdown) ----
# Split into separate datasets for Total, Male and Female
# NOTE: Row ranges are hard-coded
# If source Excel layout changes, this section must be updated
df_table_2_total <- df_table_2[1:7,] %>%
  mutate(Sex = "Total")
df_table_2_male <- df_table_2[8:14,] %>%
  mutate(Sex = "Male")
df_table_2_female <- df_table_2[15:21,] %>%
  mutate(Sex = "Female")

# ---- Table 3 ----
# Convert dates using function f_date_conversion()
df_table_3 <- f_date_conversion(df_table_3)

# ---- Table 4 ----
# Convert dates using function f_date_conversion()
df_table_4 <- f_date_conversion(df_table_4)

# ---- Table 5 ----
# Convert dates using function f_date_conversion()
df_table_5$Date <- as.Date(as.numeric(df_table_5$Date), origin = "1899-12-30")

# ---- Table 6 ----
# Convert dates using function f_date_conversion()
df_table_6 <- f_date_conversion(df_table_6)


#### -------------------------
#### Data Portal Preparation
#### -------------------------
# This section prepares cleaned datasets in the format required
# for uploading to the NISRA Data Portal

#### -------------------------
#### Weekly Deaths (wdths)
#### -------------------------

# Create dataset for total weekly deaths and related statistics
df_wdths_dp <- df_table_1a %>%
  # Remove empty rows and total row
  filter(!is.na(week_ending_date)) %>% 
  # Select and rename variables to match Data Portal specification
  transmute(
    `TLIST(W1)` = format(as.Date(week_ending_date), "%GW%V"),
    # Core statistic
    DTHSREGPROV = `Observed Deaths = total number of deaths registered in week`,
    DTHSREGPY = `Total number of deaths registered in corresponding week in 2025`,
    EXPDTHS = `Expected Deaths using current methodology [Note 4]`,
    EXCDTHS = `Excess deaths = Column C - Column J using current  methodology  [Note 4]`,
    # Confidence intervals for excess deaths
    LCIEXCDTHS = `Lower Confidence Interval (LCI) for excess deaths  using current  methodology  \r\n[Note 4]`,
    UCIEXCDTHS = `Upper Confidence Interval (UCI) for excess deaths  using current  methodology  [Note 4]`,
    # Cause-specific deaths
    FPDTHS = `Flu and / or Pneumonia Related Deaths\r\n[Note 5]`,
    CVD19DTHS = `Covid-19 related deaths registered in week  [Note 6]`
  ) %>%
  # Convert from wide format to long format (required for Data Portal)
  pivot_longer(
    cols = -`TLIST(W1)`,
    names_to = "STATISTIC",
    values_to = "VALUE"
  ) %>%
  # Add Northern Ireland geography code
  mutate(
    NI = "N92000002"
  ) %>%
  # Order columns to match required structure
  select(STATISTIC, `TLIST(W1)`, NI, VALUE)


# Export dataset to CSV for upload
# NOTE: File path uses 'data_drive' defined in config.R
write.csv(
  df_wdths_dp,
  file.path(data_drive, "df_wdths_dp.csv"),
  row.names = FALSE
)

#### -------------------------
#### Deaths by Sex and Age (wdthssxag)
#### -------------------------

df_table_2_complete <- bind_rows(
  df_table_2_female,
  df_table_2_male,
  df_table_2_total
)

# Clean week column names into standard format (e.g. YYYYWww)
colnames(df_table_2_complete) <- f_clean_week_names(colnames(df_table_2_complete))

# Create Data Portal dataset
df_wdthssxag_dp <- df_table_2_complete %>%
  # Standardise column names
  rename(AGE = Age,
         SEX = Sex) %>%
  # Remove "to date" column (not required for weekly portal data)
  select(-week_1_2026_to_date) %>%
  
  # Convert dataset to long format
  pivot_longer(
    cols = -c(SEX, AGE),
    names_to = "TLIST(W1)",
    values_to = "VALUE"
  ) %>%
  
  # Recode categories to Data Portal format
  mutate(
    SEX = recode(SEX,
                 "Total" = "ALL",
                 "Female" = "1",
                 "Male" = "2"
    ),
    # Age band coding required by portal
    AGE = recode(AGE,
                 "All" = "ALL",
                 "0-14" = "1",
                 "15-44" = "2",
                 "45-64" = "3",
                 "65-74" = "4",
                 "75-84" = "5",
                 "85+" = "6"
    ),
    # Assign statistic label
    STATISTIC = "DTHSREGPROV"
  ) %>%
  # Final column order
  select(STATISTIC, `TLIST(W1)`, SEX, AGE, VALUE)


# Export dataset to CSV for Data Portal upload
write.csv(
  df_wdthssxag_dp,
  file.path(data_drive, "df_wdthssxag_dp.csv"),
  row.names = FALSE
)


##### -------------------------
##### Weekly Deaths by LGD (wdthsldg)
##### -------------------------


# Create dataset showing weekly deaths by Local Government District (LGD)
df_wdthsldg_dp <- df_table_3 %>%
  # Select and rename LGD columns to official geographic codes
  transmute(
    `TLIST(W1)` = format(as.Date(week_ending_date), "%GW%V"),
    # Map LGD names to official codes required by Data Portal
    N09000001 = `Antrim & Newtownabbey`,
    N09000002 = `Armagh City, Banbridge & Craigavon`,
    N09000003 = `Belfast`,
    N09000004 = `Causeway Coast & Glens`,
    N09000005 = `Derry City & Strabane`,
    N09000006 = `Fermanagh & Omagh`,
    N09000007 = `Lisburn & Castlereagh`,
    N09000008 = `Mid & East Antrim`,
    N09000009 = `Mid Ulster`,
    N09000010 = `Newry, Mourne & Down`,
    N09000011 = `Ards & North Down`
  ) %>%
  
  # Convert from wide (one column per LGD) to long format
  pivot_longer(
    cols = -`TLIST(W1)`,
    names_to = "LGD2014",
    values_to = "VALUE"
  ) %>%
  
  
  # Assign statistic name
  mutate(
    STATISTIC = "DTHSREGPROV"
  ) %>%
  
  # Arrange columns to Data Portal format
  select(STATISTIC, `TLIST(W1)`, LGD2014, VALUE) 


# Ensure VALUE column is numeric (Excel import often reads as character)
df_wdthsldg_dp$VALUE <- as.numeric(df_wdthsldg_dp$VALUE)


# -------------------------
# Create Northern Ireland total (sum of all LGDs)
# -------------------------
df_wdthsldg_dp_ni <- df_wdthsldg_dp %>%
  group_by(STATISTIC, `TLIST(W1)`) %>%
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop") %>%
  
  # Assign NI geography code
  mutate(LGD2014 = "N92000002")


# Combine LGD-level data with NI totals
df_wdthsldg_dp <- bind_rows(
  df_wdthsldg_dp,
  df_wdthsldg_dp_ni
) %>%
  
  # Sort by week and geography
  arrange(`TLIST(W1)`, LGD2014)


# Export dataset for Data Portal upload
write.csv(
  df_wdthsldg_dp,
  file.path(data_drive, "df_wdthsldg_dp.csv"),
  row.names = FALSE
)


##### -------------------------
##### Weekly Deaths by Place of Death (wdthspod)
##### -------------------------


# Create dataset showing deaths by place of death (POD)
df_wdthspod_dp <- df_table_4 %>%
  
  
  # Map place of death categories to required codes
  transmute(
    `TLIST(W1)` = format(as.Date(week_ending_date), "%GW%V"),
    '1' = `Hospital`,
    '2' = `Care/nursing Home\r\n [Note 2]`,
    '3' = `Hospice`,
    '4' = `Home`,
    '5' = `Other \r\n[Note 3]`,
    'ALL' = `Total`
  ) %>%
  
  
  # Convert to long format for portal
  pivot_longer(
    cols = -`TLIST(W1)`,
    names_to = "POD",
    values_to = "VALUE"
  ) %>%
  
  
  # Add statistic label
  mutate(
    STATISTIC = "DTHSREGPROV"
  ) %>%
  select(STATISTIC, `TLIST(W1)`, POD, VALUE)


# Export CSV
write.csv(
  df_wdthspod_dp,
  file.path(data_drive, "df_wdthspod_dp.csv"),
  row.names = FALSE
)



##### -------------------------
##### Weekly Death Occurrences (wdthsocc)
##### -------------------------


# Create dataset for deaths based on date of occurrence (not registration)
df_wdthsocc_dp <- df_table_6 %>%
  transmute(
    `TLIST(W1)` = format(as.Date(week_ending_date), "%GW%V"),
    
    # Weekly occurrence values
    DTHSOCCPROV = `All deaths occurring in week`,
    
    # Cumulative occurrence values
    CUMDTHSOCC = `Cumulative number of all deaths occuring`
  ) %>%
  
  
  # Convert to long format
  pivot_longer(
    cols = -`TLIST(W1)`,
    names_to = "STATISTIC",
    values_to = "VALUE"
  ) %>%
  
  
  # Add NI geography code
  mutate(
    NI = "N92000002"
  ) %>%
  select(STATISTIC, `TLIST(W1)`, NI, VALUE)


# Export dataset
write.csv(
  df_wdthsocc_dp,
  file.path(data_drive, "df_wdthsocc_dp.csv"),
  row.names = FALSE
)
