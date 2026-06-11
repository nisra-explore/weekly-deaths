# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))


# get current YYYY
cur_year <- lubridate::year(Sys.Date())

# create list of data portal URLs
api_urls <- list(
  wdths     = "https://ws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHS/CSV/1.0/en",
  wdthssxag = "https://ws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHSSXAG/CSV/1.0/en",
  wdthspod  = "https://ws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHSPOD/CSV/1.0/en",
  wdthslgd  = "https://ws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHSLGD/CSV/1.0/en",
  wdthsocc  = "https://ws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHSOCC/CSV/1.0/en"
)

# creat function read_csv_api to read in csv(s) from API URL
read_csv_api <- function(url) {
  read.csv(
    url,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# lappy() will let you apply read_csv_api to  api_urls list
datasets <- lapply(api_urls, read_csv_api)

# add df_ prefix to name of dataframes
names(datasets) <- paste0("df_", names(datasets))

# make the dataframes available in the global environment
list2env(datasets, envir = .GlobalEnv)

# list objects in the current R environment with 'df_' prefix
ls(pattern = "^df_")

##### Deaths registered vs excess deaths #####

###### pull week ending date ######

week_ending <- df_wdths %>%
  filter(STATISTIC == "DTHSREGPROV") %>%
  arrange(desc(`TLIST(W1)`)) %>%
  slice(1) %>% # take top two rows (current and prev week)
  pull('Week ending date')


###### reg weekly deaths #######

# filter for 'Deaths Registered (Provisional)
reg_wdths_vals <- df_wdths %>%
  filter(STATISTIC == "DTHSREGPROV") %>%
  arrange(desc(`TLIST(W1)`)) %>%
  slice(1:2) %>% # take top two rows (current and prev week)
  pull(VALUE) %>% # pull values
  as.numeric()

# calc registered weekly deaths - registered weekly deaths previous and set trend indicator
reg_wdths <- reg_wdths_vals[1]
reg_wdths_prev <- reg_wdths_vals[2]


reg_wdths_diff <- reg_wdths - reg_wdths_prev

reg_wdths_arrow <- ifelse(reg_wdths_diff > 0, "more",
                          ifelse(reg_wdths_diff < 0, "fewer", "▬"))

reg_wdths_single_plural <- ifelse(reg_wdths_diff == 1, "death than previous week",
                           ifelse(reg_wdths_diff == -1, "death than previous week",
                           ifelse(reg_wdths_diff == 0, "no change from previous week", 'deaths than previous week')))

###### expected weekly deaths #######

# filter for expected deaths
expect_wdths <- df_wdths %>%
  filter(
    STATISTIC == "EXPDTHS",
    `TLIST(W1)` == max(`TLIST(W1)`, na.rm = TRUE)
  ) %>%
  pull(VALUE) %>%
  as.numeric()

# calc regsitered weekly deaths - expected deaths and set trend indicator
diff_wdths <- reg_wdths - expect_wdths

diff_arrow <- ifelse(diff_wdths > 0, "more",
                     ifelse(diff_wdths < 0, "fewer", "no change"))


# filter for 'Deaths Registered (Provisional)
reg_wdths_vals <- df_wdths %>%
  filter(STATISTIC == "DTHSREGPROV") %>%
  arrange(desc(`TLIST(W1)`)) %>%
  slice(1:2) %>%
  pull(VALUE) %>%
  as.numeric()

# set trend indicator text
expected_diff_text <- ifelse(
  diff_wdths > 0,                               # if reg_wdths - expect_wdths > 0
  paste(abs(diff_wdths), "above expected"),     # above expected
  ifelse(diff_wdths < 0,                            # if reg_wdths - expect_wdths < 0
         paste(abs(diff_wdths), "below expected"),  # else below expected
         "In line with expected")                   # else in line
)

###### flu weekly deaths #######
# filter for flu/pneumonia deaths
flu_wdths_vals <- df_wdths %>%
  filter(STATISTIC == "FPDTHS") %>%
  arrange(desc(`TLIST(W1)`)) %>%
  slice(1:2) %>%
  pull(VALUE) %>%
  as.numeric()

# calc flu deaths - flu deaths previous and set trend indicator
flu_wdths <- flu_wdths_vals[1]
flu_wdths_prev <- flu_wdths_vals[2]

flu_wdths_diff <- flu_wdths - flu_wdths_prev

flu_wdths_arrow <- ifelse(flu_wdths_diff > 0, "more",
                          ifelse(flu_wdths_diff < 0, "fewer", "▬"))

flu_wdths_single_plural <- ifelse(flu_wdths_diff == 1, "death than previous week",
                           ifelse(flu_wdths_diff == -1, "death than previous week",
                           ifelse(flu_wdths_diff == 0, "no change from previous week", 'deaths than previous week')))

###### covid weekly deaths #######
# filter for covid deaths
covid_wdths_vals <- df_wdths %>%
  filter(STATISTIC == "CVD19DTHS") %>%
  arrange(desc(`TLIST(W1)`)) %>%
  slice(1:2) %>%
  pull(VALUE) %>%
  as.numeric()

# calc covid deaths - covid deaths previous and set trend indicator
covid_wdths <- covid_wdths_vals[1]
covid_wdths_prev <- covid_wdths_vals[2]

covid_wdths_diff <- covid_wdths - covid_wdths_prev

covid_wdths_arrow <- ifelse(covid_wdths_diff > 0, "more",
                          ifelse(covid_wdths_diff < 0, "fewer", "▬"))

covid_wdths_single_plural <- ifelse(covid_wdths_diff == 1, "death than previous week",
                            ifelse(covid_wdths_diff == -1, "death than previous week",
                            ifelse(covid_wdths_diff == 0, "no change from previous week", 'deaths than previous week')))

###### reg vs expected weekly deaths #######
# filter on registered vs expected deaths
df_observe_excess <- df_wdths %>%
  filter(STATISTIC %in% c("DTHSREGPROV", "EXPDTHS")) %>%
  select(STATISTIC, `TLIST(W1)`, VALUE) %>%
  mutate(
    VALUE = as.numeric(VALUE),
    week_label = str_sub(`TLIST(W1)`, -2),
    # Convert week code to calendar date
    week_date = ISOweek::ISOweek2date(
      paste0(str_sub(`TLIST(W1)`, 1, 4), "-W", week_label, "-5")
    )
  ) %>%
  arrange(week_date) %>%
  # Create separate columns for registered and expected deaths
  pivot_wider(
    names_from = STATISTIC,
    values_from = VALUE
  ) %>%
  # Preserve week order
  mutate(
    week_label = factor(week_label, levels = unique(week_label))
  )

# create rolling 52 week dataframe
df_observe_excess <- df_observe_excess %>%
  dplyr::mutate(
    week_date = as.Date(week_date)
  ) %>%
  dplyr::arrange(week_date) %>%
  dplyr::slice_tail(n = 53) %>%
  # Add week number and bar positioning for chart
  dplyr::mutate(
    week_num = dplyr::row_number(),
    bar_left = week_num - 0.50,
    bar_right = week_num + 0.40
  )

df_observe_excess_dl <- df_observe_excess %>%
  select('TLIST(W1)', week_date, DTHSREGPROV, EXPDTHS) %>%
  rename(Week = 'TLIST(W1)',
         'Week date' = week_date,
         'Number of registered deaths' = DTHSREGPROV,
         'Number of expected deaths' = EXPDTHS)

##### Sex breakdown #####

# Extract latest week female and male deaths
# Weekly
female_wdths <- df_wdthssxag %>%
  filter(
    AGE == "ALL",
    SEX == 1,
    `TLIST(W1)` == max(`TLIST(W1)`, na.rm = TRUE)
  ) %>%
  pull(VALUE) %>%
  as.numeric()

male_wdths <- df_wdthssxag %>%
  filter(
    AGE == "ALL",
    SEX == 2,
    `TLIST(W1)` == max(`TLIST(W1)`, na.rm = TRUE)
  ) %>%
  pull(VALUE) %>%
  as.numeric()

# Calculate year-to-date totals by sex
# YTD
df_ytd_SX <- df_wdthssxag %>%
  filter(grepl(cur_year, df_wdthssxag$`Week ending date`))

female_ytd_wdths <- df_ytd_SX %>%
  filter(
    AGE == "ALL",
    SEX == 1,
    na.rm = TRUE
  ) %>%
  summarise(VALUE = sum(`VALUE`, na.rm = TRUE)) %>%
  pull(VALUE) %>%
  as.numeric()

male_ytd_wdths <- df_ytd_SX %>%
  filter(
    AGE == "ALL",
    SEX == 2,
    na.rm = TRUE
  ) %>%
  summarise(VALUE = sum(`VALUE`, na.rm = TRUE)) %>%
  pull(VALUE) %>%
  as.numeric()

# Create sex breakdown for pie charts (weekly and YTD)
# Weekly
sex_pie_chart_wdths <- tibble(
  sex = c("Female", "Male"),
  value = c(female_wdths, male_wdths)
)

# YTD
ytd_sex_pie_chart_wdths <- tibble(
  sex = c("Female", "Male"),
  value = c(female_ytd_wdths, male_ytd_wdths)
)


##### Age breakdown #####

# Extract last 7 of age-stratified deaths
df_death_age <- tail(df_wdthssxag, n=7)

# Remove metadata columns
df_death_age <- df_death_age[ , -c(1:7, 9)]

# Remove total row
df_death_age <- df_death_age[-c(7),]

# rename Value column to number of deaths
colnames(df_death_age)[2] <- "Weekly Total"

# make Weekly Total numeric 
df_death_age$`Weekly Total` <- as.numeric(df_death_age$`Weekly Total`)

# Clean age band labels
df_death_age <- df_death_age %>%
  mutate(`Age band` = str_remove(`Age band`, "^Age\\s+"))

df_death_age_dl <- df_death_age %>%
  rename('Number of deaths' = 'Weekly Total')

# Calculate percentage of deaths in age 75+

week_total = sum(df_death_age$`Weekly Total`)
over_75 = sum(df_death_age$`Weekly Total`[df_death_age$Age %in% c("75-84", "85+")])
over_75_pct = round((over_75/week_total)*100, 1) # round to 1 decimal point

# Calculate year-to-date age breakdown
cur_year <- lubridate::year(Sys.Date())
df_age_ytd <- df_wdthssxag %>%
  filter(grepl(cur_year, df_wdthssxag$`Week ending date`))

# Aggregate by age band
df_age_ytd <- df_age_ytd %>%
  select(-c(1:7, 9)) %>%
  group_by(`Age band`) %>%
  summarise(total_value = sum(`VALUE`, na.rm = TRUE))
# Remove total row
df_age_ytd <- df_age_ytd [-c(7),]

# Clean age band labels
df_age_ytd <- df_age_ytd %>%
  mutate(`Age band` = str_remove(`Age band`, "^Age\\s+"))

df_age_ytd_dl <- df_age_ytd %>%
  rename('Number of deaths' = 'total_value')

##### LGD#####
# Prepare weekly deaths by Local Government District (LGD)
# Get latest available week
latest_week <- max(df_wdthslgd$`TLIST(W1)`, na.rm = TRUE)

# Extract latest week data
df_rod <- df_wdthslgd %>%
  filter(`TLIST(W1)` == latest_week) %>%
  transmute(
    LGDCode = LGD2014,
    Number_of_Deaths = as.numeric(VALUE)
  ) %>%
  arrange(desc(Number_of_Deaths))

# Store week label for display
rod_week_ending <- latest_week

# Define map coordinates for each LGD
df_map <- data.frame(
  LGDCode = c("N09000001","N09000011","N09000002","N09000003","N09000004",
              "N09000005","N09000006","N09000007","N09000009","N09000008","N09000010"),
  lat = c(54.7294, 54.6325, 54.3668, 54.6, 55.0564, 54.85, 54.52, 54.5004, 54.6246, 54.8987, 54.1685),
  lng = c(-6.1259, -5.6565, -6.4365, -5.95, -6.5574, -7.235, -7.4033, -6.0609, -6.73, -6.15, -6.203)
)

# Load LGD boundary map
df_lgd_map <- st_read(
  paste0(here(),"/maps/lgd/lgd_loughs_removed_simplified.shp"),
  quiet = TRUE
) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

# Join deaths data to map and set threshold at 60th percentile
df_map <- df_lgd_map %>%
  left_join(df_map, by = "LGDCode") %>% 
  mutate(LGDNAME = gsub("and", "&", LGDNAME)) %>% 
  left_join(
    df_rod %>% 
      select(LGDCode, num_deaths = Number_of_Deaths),
    by = "LGDCode"
  )

threshold <- quantile(df_map$num_deaths, 0.6, na.rm = TRUE)

# Mark LGDs that need label positioning adjustment
crowded_labels <- c("N09000003", "N09000011")

# Rank LGDs and categorize into groups for visualisation
df_map <- df_map %>%
  arrange(desc(num_deaths)) %>%
  mutate(
    rank = row_number(),
    deaths_category = case_when(
      rank == 1 ~ "Highest Number of Deaths",
      rank %in% 2:3 ~ "2nd - 3rd Highest",
      rank %in% 4:5 ~ "4th - 5th Highest",
      rank %in% 6:7 ~ "6th - 7th Highest",
      rank %in% 8:9 ~ "8th - 9th Highest",
      rank %in% 10:11 ~ "10th - 11th Highest",
      TRUE ~ "Other"
    ),
    deaths_category = factor(deaths_category, 
                            levels = c("Highest Number of Deaths", 
                                     "2nd - 3rd Highest",
                                     "4th - 5th Highest", 
                                     "6th - 7th Highest",
                                     "8th - 9th Highest", 
                                     "10th - 11th Highest"))
  )

# Prepare labels with text wrapping and colour scheme
df_map <- df_map %>%
  mutate(
    lgd_wrapped = str_wrap(gsub("and", "&", LGDNAME), width = 13),
    label_text = paste0(lgd_wrapped, " ", scales::comma(num_deaths)),
    label_colour = ifelse(LGDCode %in% crowded_labels, "black",
                          ifelse(num_deaths > threshold, "white", "black"))
  )

# Separate crowded from non-crowded LGDs for plotting
crowded_df <- df_map %>% filter(LGDCode %in% crowded_labels)
noncrowded_df <- df_map %>% filter(!LGDCode %in% crowded_labels)

# Adjust label positions for crowded areas
crowded_df <- crowded_df %>%
  mutate(
    nudge_x = case_when(
      LGDCode == "N09000003" ~ 0.4,
      LGDCode == "N09000011" ~ 0.6
    ),
    nudge_y = case_when(
      LGDCode == "N09000003" ~ 0.15,
      LGDCode == "N09000011" ~ 0
    )
  )

# Prepare year-to-date LGD data using same process
# Sum year-to-date deaths by LGD
df_ytd_wdthslgd <- df_wdthslgd %>%
  filter(grepl(cur_year, df_wdthslgd$`Week ending date`)) %>%
  transmute(
    LGDCode = LGD2014,
    Number_of_Deaths = as.numeric(VALUE)
  ) %>%
  group_by(`LGDCode`) %>%
  summarise(Number_of_Deaths = sum(`Number_of_Deaths`, na.rm = TRUE)) %>%
  arrange(desc(Number_of_Deaths))

# Define coordinates for YTD map
df_ytd_map <- data.frame(
  LGDCode = c("N09000001","N09000011","N09000002","N09000003","N09000004",
              "N09000005","N09000006","N09000007","N09000009","N09000008","N09000010"),
  lat = c(54.7294, 54.6325, 54.3668, 54.6, 55.0564, 54.85, 54.52, 54.5004, 54.6246, 54.8987, 54.1685),
  lng = c(-6.1259, -5.6565, -6.4365, -5.95, -6.5574, -7.235, -7.4033, -6.0609, -6.73, -6.15, -6.203)
)

# Join YTD deaths data to map
df_ytd_map <- df_lgd_map %>%
  left_join(df_ytd_map, by = "LGDCode") %>% 
  mutate(LGDNAME = gsub("and", "&", LGDNAME)) %>% 
  left_join(
    df_ytd_wdthslgd %>% 
      select(LGDCode, num_deaths = Number_of_Deaths),
    by = "LGDCode"
  )

# Calculate 60th percentile threshold for YTD
threshold <- quantile(df_ytd_map$num_deaths, 0.6, na.rm = TRUE)

# Rank and categorize LGDs for YTD visualisation
df_ytd_map <- df_ytd_map %>%
  arrange(desc(num_deaths)) %>%
  mutate(
    rank = row_number(),
    deaths_category = case_when(
      rank == 1 ~ "Highest Number of Deaths",
      rank %in% 2:3 ~ "2nd - 3rd Highest",
      rank %in% 4:5 ~ "4th - 5th Highest",
      rank %in% 6:7 ~ "6th - 7th Highest",
      rank %in% 8:9 ~ "8th - 9th Highest",
      rank %in% 10:11 ~ "10th - 11th Highest",
      TRUE ~ "Other"
    ),
    deaths_category = factor(deaths_category, 
                             levels = c("Highest Number of Deaths", 
                                        "2nd - 3rd Highest",
                                        "4th - 5th Highest", 
                                        "6th - 7th Highest",
                                        "8th - 9th Highest", 
                                        "10th - 11th Highest"))
  )

# Prepare YTD labels and colours
df_ytd_map <- df_ytd_map %>%
  mutate(
    lgd_wrapped = str_wrap(gsub("and", "&", LGDNAME), width = 13),
    label_text = paste0(lgd_wrapped, " ", scales::comma(num_deaths)),
    label_colour = ifelse(LGDCode %in% crowded_labels, "black",
                          ifelse(num_deaths > threshold, "white", "black"))
  )

# Separate crowded and non-crowded LGDs for YTD
crowded_ytd_df <- df_ytd_map %>% filter(LGDCode %in% crowded_labels)
noncrowded_ytd_df <- df_ytd_map %>% filter(!LGDCode %in% crowded_labels)

# Adjust YTD label positions
crowded_ytd_df <- crowded_ytd_df %>%
  mutate(
    nudge_x = case_when(
      LGDCode == "N09000003" ~ 0.4,
      LGDCode == "N09000011" ~ 0.6
    ),
    nudge_y = case_when(
      LGDCode == "N09000003" ~ 0.15,
      LGDCode == "N09000011" ~ 0
    )
  )

# Prepare map data for download (remove spatial geometry columns)
df_map_weekly <- df_map

# Remove spatial class to allow export
class(df_map_weekly) <- class(df_map_weekly)[class(df_map_weekly) != "sf"]

# Drop geometry columns
df_map_weekly <- df_map_weekly[ , -c(5, 6, 8)]

# Unlist to flat data frame
df_map_weekly[] <- lapply(df_map_weekly, unlist)

df_map_weekly_dl <- df_map_weekly %>%
  select(LGDNAME, num_deaths) %>%
  rename('Local government district (LGD)' = LGDNAME,
         'Number of deaths' = num_deaths)

# Prepare YTD map data for download
df_map_ytd <- df_ytd_map

# Remove spatial class
class(df_map_ytd) <- class(df_map_ytd)[class(df_map_ytd) != "sf"]

# Drop geometry columns
df_map_ytd <- df_map_ytd[ , -c(5, 6, 8)]

# Unlist to flat data frame
df_map_ytd[] <- lapply(df_map_ytd, unlist)

df_map_ytd_dl <- df_map_ytd %>%
  select(LGDNAME, num_deaths) %>%
  rename('Local government district (LGD)' = LGDNAME,
         'Number of deaths' = num_deaths)

##### Place of death #####
# Prepare weekly place of death breakdown
df_pod <- tail(df_wdthspod, n=6)

# drop unneeded columns
# Remove metadata columns
df_pod <- df_pod[ , -c(1:5, 7)]

# Remove all places total row
df_pod <- df_pod[-c(6),]

# Rename value column
colnames(df_pod)[2] <- "Number of Deaths"

# Sort by highest deaths
df_pod <- df_pod[order(-df_pod$`Number of Deaths`), ]

# Prepare year-to-date place of death breakdown
df_ytd_pod <- df_wdthspod %>%
  filter(grepl(cur_year, df_wdthspod$`Week ending date`))

# Sum by place of death
df_ytd_pod <- df_ytd_pod %>%
  select(-c(1:5, 7)) %>%
  group_by(`Place of death`) %>%
  summarise(total_value = sum(`VALUE`, na.rm = TRUE))

# Sort by highest deaths and remove all places row
df_ytd_pod <- df_ytd_pod[order(-df_ytd_pod$`total_value`), ]
df_ytd_pod <- df_ytd_pod[-c(1),]

df_ytd_pod_dl <- df_ytd_pod %>%
  rename('Number of deaths' = total_value)

##### Registered vs Occurred #####
# Compare registered deaths with deaths that occurred in latest 52 weeks
# Extract registered deaths and parse dates
df_number_registered <- df_wdths %>%
  dplyr::filter(STATISTIC == "DTHSREGPROV") %>%
  dplyr::transmute(
    `TLIST(W1)`,
    `Week ending date` = as.Date(lubridate::parse_date_time(
      `Week ending date`,
      orders = c("dmy", "ymd", "d b Y", "d B Y")
    )),
    `Registered Deaths` = as.numeric(VALUE)
  )

# Extract deaths that occurred
df_number_occurred <- df_wdthsocc %>%
  dplyr::filter(STATISTIC == "DTHSOCCPROV") %>%
  dplyr::transmute(
    `TLIST(W1)`,
    `Deaths Occurred` = as.numeric(VALUE)
  )

# Join registered and occurred deaths, keep last 52 weeks, add date features
df_reg_vs_occ <- df_number_registered %>%
  dplyr::left_join(df_number_occurred, by = "TLIST(W1)") %>%
  dplyr::arrange(`Week ending date`) %>%
  dplyr::filter(
    `Week ending date` >= max(`Week ending date`, na.rm = TRUE) - lubridate::weeks(52)
  ) %>%
  dplyr::mutate(
    `Week Number` = dplyr::row_number(),
    month = lubridate::floor_date(`Week ending date`, "month"),
    year = lubridate::year(`Week ending date`),
    month_label = format(`Week ending date`, "%b"),
    week_label_hover = stringr::str_sub(`TLIST(W1)`, -2)
  ) %>%
  dplyr::rename(
    week_ending_date = `Week ending date`
  ) %>%
  dplyr::select(
    `Week Number`,
    `TLIST(W1)`,
    week_label_hover,
    week_ending_date,
    `Registered Deaths`,
    `Deaths Occurred`,
    month,
    year,
    month_label
  )

df_reg_vs_occ_dl <- df_reg_vs_occ %>%
  select('TLIST(W1)', week_ending_date, 'Registered Deaths', 'Deaths Occurred') %>%
  rename(Week = 'TLIST(W1)',
         'Week date' = week_ending_date,
         'Number of registered deaths' = 'Registered Deaths',
         'Number of deaths occurred' = 'Deaths Occurred')
