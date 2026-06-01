# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))


# get current YYYY
cur_year <- lubridate::year(Sys.Date())

api_urls <- list(
  wdths     = "https://ppws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHS/CSV/1.0/en",
  wdthssxag = "https://ppws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHSSXAG/CSV/1.0/en",
  wdthspod  = "https://ppws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHSPOD/CSV/1.0/en",
  wdthslgd  = "https://ppws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHSLGD/CSV/1.0/en",
  wdthsocc  = "https://ppws-data.nisra.gov.uk/public/api.restful/PxStat.Data.Cube_API.ReadDataset/WDTHSOCC/CSV/1.0/en"
)

read_csv_api <- function(url) {
  read.csv(
    url,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

datasets <- lapply(api_urls, read_csv_api)

names(datasets) <- paste0("df_", names(datasets))

list2env(datasets, envir = .GlobalEnv)

ls(pattern = "^df_")

##### Deaths registered vs excess deaths #####

###### reg weekly deaths #######

reg_wdths_vals <- df_wdths %>%
  filter(STATISTIC == "DTHSREGPROV") %>%
  arrange(desc(`TLIST(W1)`)) %>%
  slice(1:2) %>%
  pull(VALUE) %>%
  as.numeric()

reg_wdths <- reg_wdths_vals[1]
reg_wdths_prev <- reg_wdths_vals[2]

reg_wdths_diff <- reg_wdths - reg_wdths_prev

reg_wdths_arrow <- ifelse(reg_wdths_diff > 0, "more",
                          ifelse(reg_wdths_diff < 0, "fewer", "▬"))

###### expected weekly deaths #######

expect_wdths <- df_wdths %>%
  filter(
    STATISTIC == "EXPDTHS",
    `TLIST(W1)` == max(`TLIST(W1)`, na.rm = TRUE)
  ) %>%
  pull(VALUE) %>%
  as.numeric()

diff_wdths <- reg_wdths - expect_wdths

diff_arrow <- ifelse(diff_wdths > 0, "more",
                     ifelse(diff_wdths < 0, "fewer", "▬"))

reg_wdths_vals <- df_wdths %>%
  filter(STATISTIC == "DTHSREGPROV") %>%
  arrange(desc(`TLIST(W1)`)) %>%
  slice(1:2) %>%
  pull(VALUE) %>%
  as.numeric()

expected_diff_text <- ifelse(
  diff_wdths > 0,
  paste(abs(diff_wdths), "above expected"),
  ifelse(diff_wdths < 0,
         paste(abs(diff_wdths), "below expected"),
         "In line with expected")
)

###### flu weekly deaths #######

flu_wdths_vals <- df_wdths %>%
  filter(STATISTIC == "FPDTHS") %>%
  arrange(desc(`TLIST(W1)`)) %>%
  slice(1:2) %>%
  pull(VALUE) %>%
  as.numeric()


flu_wdths <- flu_wdths_vals[1]
flu_wdths_prev <- flu_wdths_vals[2]

flu_wdths_diff <- flu_wdths - flu_wdths_prev

flu_wdths_arrow <- ifelse(flu_wdths_diff > 0, "more",
                          ifelse(flu_wdths_diff < 0, "fewer", "▬"))

flu_wdths_single_plural <- ifelse(flu_wdths_diff == 1, "death",
                           ifelse(flu_wdths_diff == -1, "death",
                           ifelse(flu_wdths_diff == 0, "-", 'deaths')))

###### covid weekly deaths #######

covid_wdths_vals <- df_wdths %>%
  filter(STATISTIC == "CVD19DTHS") %>%
  arrange(desc(`TLIST(W1)`)) %>%
  slice(1:2) %>%
  pull(VALUE) %>%
  as.numeric()


covid_wdths <- covid_wdths_vals[1]
covid_wdths_prev <- covid_wdths_vals[2]

covid_wdths_diff <- covid_wdths - covid_wdths_prev

covid_wdths_arrow <- ifelse(covid_wdths_diff > 0, "more",
                          ifelse(covid_wdths_diff < 0, "fewer", "▬"))

covid_wdths_single_plural <- ifelse(covid_wdths_diff == 1, "death",
                            ifelse(covid_wdths_diff == -1, "death",
                            ifelse(covid_wdths_diff == 0, "-", 'deaths')))

###### reg vs expected weekly deaths #######
df_observe_excess <- df_wdths %>%
  filter(STATISTIC %in% c("DTHSREGPROV", "EXPDTHS")) %>%
  select(STATISTIC, `TLIST(W1)`, VALUE) %>%
  mutate(
    VALUE = as.numeric(VALUE),
    week_label = str_sub(`TLIST(W1)`, -2),
    week_date = ISOweek::ISOweek2date(
      paste0(str_sub(`TLIST(W1)`, 1, 4), "-W", week_label, "-5")
    )
  ) %>%
  arrange(week_date) %>%
  pivot_wider(
    names_from = STATISTIC,
    values_from = VALUE
  ) %>%
  mutate(
    week_label = factor(week_label, levels = unique(week_label))
  )

df_observe_excess <- df_observe_excess %>%
  dplyr::mutate(
    week_date = as.Date(week_date)
  ) %>%
  dplyr::arrange(week_date) %>%
  dplyr::slice_tail(n = 53) %>%
  dplyr::mutate(
    week_num = dplyr::row_number(),
    bar_left = week_num - 0.50,
    bar_right = week_num + 0.40
  )

##### Sex breakdown #####

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

# slice off bottom 7 rows for most recent weeks numbers
df_death_age <- tail(df_wdthssxag, n=7)

df_death_age <- df_death_age[ , -c(1:7, 9)]

# drop all places value
df_death_age <- df_death_age[-c(7),]

# rename Value column to number of deaths
colnames(df_death_age)[2] <- "Weekly Total"

# make Weekly Total numeric 
df_death_age$`Weekly Total` <- as.numeric(df_death_age$`Weekly Total`)

#Remove 'Age' from bands
df_death_age <- df_death_age %>%
  mutate(`Age band` = str_remove(`Age band`, "^Age\\s+"))



# % over 75 figure
week_total = sum(df_death_age$`Weekly Total`)
over_75 = sum(df_death_age$`Weekly Total`[df_death_age$Age %in% c("75-84", "85+")])
over_75_pct = round((over_75/week_total)*100, 1) # round to 1 decimal point

#YTD 
cur_year <- lubridate::year(Sys.Date())
df_age_ytd <- df_wdthssxag %>%
  filter(grepl(cur_year, df_wdthssxag$`Week ending date`))

# drop unneeded columns and group by place of death
df_age_ytd <- df_age_ytd %>%
  select(-c(1:7, 9)) %>%
  group_by(`Age band`) %>%
  summarise(total_value = sum(`VALUE`, na.rm = TRUE))
df_age_ytd <- df_age_ytd [-c(7),]

#Remove 'Age' from bands
df_age_ytd <- df_age_ytd %>%
  mutate(`Age band` = str_remove(`Age band`, "^Age\\s+"))


##### LGD#####
# Weekly numbers
# Get latest available week
latest_week <- max(df_wdthslgd$`TLIST(W1)`, na.rm = TRUE)

# Filter to latest week
df_rod <- df_wdthslgd %>%
  filter(`TLIST(W1)` == latest_week) %>%
  transmute(
    LGDCode = LGD2014,
    Number_of_Deaths = as.numeric(VALUE)
  ) %>%
  arrange(desc(Number_of_Deaths))

# Use latest week as week ending display
rod_week_ending <- latest_week

# Prepare base df_map
df_map <- data.frame(
  LGDCode = c("N09000001","N09000011","N09000002","N09000003","N09000004",
              "N09000005","N09000006","N09000007","N09000009","N09000008","N09000010"),
  lat = c(54.7294, 54.6325, 54.3668, 54.6, 55.0564, 54.85, 54.52, 54.5004, 54.6246, 54.8987, 54.1685),
  lng = c(-6.1259, -5.6565, -6.4365, -5.95, -6.5574, -7.235, -7.4033, -6.0609, -6.73, -6.15, -6.203)
)

# Load spatial data
df_lgd_map <- st_read(
  paste0(here(),"/maps/lgd/lgd_loughs_removed_simplified.shp"),
  quiet = TRUE
) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

# Merge and calculate thresholds
df_map <- df_lgd_map %>%
  left_join(df_map, by = "LGDCode") %>% 
  mutate(LGDNAME = gsub("and", "&", LGDNAME)) %>% 
  left_join(
    df_rod %>% 
      select(LGDCode, num_deaths = Number_of_Deaths),
    by = "LGDCode"
  )

threshold <- quantile(df_map$num_deaths, 0.6, na.rm = TRUE)

# Identify crowded LGD codes
crowded_labels <- c("N09000003", "N09000011")

# Add ranking by deaths (descending) and categorise into groups
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

# Add wrapped labels and logic
df_map <- df_map %>%
  mutate(
    lgd_wrapped = str_wrap(gsub("and", "&", LGDNAME), width = 13),
    label_text = paste0(lgd_wrapped, " ", num_deaths),
    label_colour = ifelse(LGDCode %in% crowded_labels, "black",
                          ifelse(num_deaths > threshold, "white", "black"))
  )

# Subsets for plotting
crowded_df <- df_map %>% filter(LGDCode %in% crowded_labels)
noncrowded_df <- df_map %>% filter(!LGDCode %in% crowded_labels)

# Custom nudges
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

# YTD numbers
# filter for current YYYY
df_ytd_wdthslgd <- df_wdthslgd %>%
  filter(grepl(cur_year, df_wdthslgd$`Week ending date`)) %>%
  transmute(
    LGDCode = LGD2014,
    Number_of_Deaths = as.numeric(VALUE)
  ) %>%
  group_by(`LGDCode`) %>%
  summarise(Number_of_Deaths = sum(`Number_of_Deaths`, na.rm = TRUE)) %>%
  arrange(desc(Number_of_Deaths))

# Prepare base df_map
df_ytd_map <- data.frame(
  LGDCode = c("N09000001","N09000011","N09000002","N09000003","N09000004",
              "N09000005","N09000006","N09000007","N09000009","N09000008","N09000010"),
  lat = c(54.7294, 54.6325, 54.3668, 54.6, 55.0564, 54.85, 54.52, 54.5004, 54.6246, 54.8987, 54.1685),
  lng = c(-6.1259, -5.6565, -6.4365, -5.95, -6.5574, -7.235, -7.4033, -6.0609, -6.73, -6.15, -6.203)
)

# Merge and calculate thresholds
df_ytd_map <- df_lgd_map %>%
  left_join(df_ytd_map, by = "LGDCode") %>% 
  mutate(LGDNAME = gsub("and", "&", LGDNAME)) %>% 
  left_join(
    df_ytd_wdthslgd %>% 
      select(LGDCode, num_deaths = Number_of_Deaths),
    by = "LGDCode"
  )

threshold <- quantile(df_ytd_map$num_deaths, 0.6, na.rm = TRUE)

# Add ranking by deaths (descending) and categorise into groups
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

# Add wrapped labels and logic
df_ytd_map <- df_ytd_map %>%
  mutate(
    lgd_wrapped = str_wrap(gsub("and", "&", LGDNAME), width = 13),
    label_text = paste0(lgd_wrapped, " ", num_deaths),
    label_colour = ifelse(LGDCode %in% crowded_labels, "black",
                          ifelse(num_deaths > threshold, "white", "black"))
  )

# Subsets for plotting
crowded_ytd_df <- df_ytd_map %>% filter(LGDCode %in% crowded_labels)
noncrowded_ytd_df <- df_ytd_map %>% filter(!LGDCode %in% crowded_labels)

# Custom nudges
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

# This is required for chart downloads
# copy tibble as backup for next step and drop geometery columns
df_map_weekly <- df_map

# remove sf class
class(df_map_weekly) <- class(df_map_weekly)[class(df_map_weekly) != "sf"]

# remove geometry columns before unlisting
df_map_weekly <- df_map_weekly[ , -c(5, 6, 8)]

# unlist remaining columns for download
df_map_weekly[] <- lapply(df_map_weekly, unlist)

# same again for df_map_ytd
# copy tibble as backup for next step and drop geometery columns
df_map_ytd <- df_ytd_map

# remove sf class
class(df_map_ytd) <- class(df_map_ytd)[class(df_map_ytd) != "sf"]

# remove geometry columns before unlisting
df_map_ytd <- df_map_ytd[ , -c(5, 6, 8)]

# unlist remaining columns for download
df_map_ytd[] <- lapply(df_map_ytd, unlist)



##### Place of death #####
# weekly numbers
# slice off bottom 6 rows for most recent weeks numbers
df_pod <- tail(df_wdthspod, n=6)

# drop unneeded columns
df_pod <- df_pod[ , -c(1:5, 7)]

# drop all places value
df_pod <- df_pod[-c(6),]

# rename Value column to number of deaths
colnames(df_pod)[2] <- "Number of Deaths"

# sort by descending # of deaths
df_pod <- df_pod[order(-df_pod$`Number of Deaths`), ]

# YTD numbers
# filter for current YYYY
df_ytd_pod <- df_wdthspod %>%
  filter(grepl(cur_year, df_wdthspod$`Week ending date`))

# drop unneeded columns and group by place of death
df_ytd_pod <- df_ytd_pod %>%
  select(-c(1:5, 7)) %>%
  group_by(`Place of death`) %>%
  summarise(total_value = sum(`VALUE`, na.rm = TRUE))

# sort by descending # of deaths and drop 'All places'
df_ytd_pod <- df_ytd_pod[order(-df_ytd_pod$`total_value`), ]

df_ytd_pod <- df_ytd_pod[-c(1),]


##### Registered vs Occurred #####
# Get registered adnd occurred totals and rename columns
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

df_number_occurred <- df_wdthsocc %>%
  dplyr::filter(STATISTIC == "DTHSOCCPROV") %>%
  dplyr::transmute(
    `TLIST(W1)`,
    `Deaths Occurred` = as.numeric(VALUE)
  )

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