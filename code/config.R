# configuration script

#### INPUTS AND SETTINGS ####

##### SETTINGS #####

# Set Report parameters, name of department, pre release status and logo type

nicstheme <- "dof"
prerelease <- FALSE
bilingual <- FALSE

###### parameter options:
#  nicstheme - can be any of teo, daera, dfc, de, dfe, dof, dfi, doh, doj, bso
#  prerelease - can be TRUE or FALSE
#  bilingual - can be TRUE or FALSE - Sets language for NISRA Logo

##### YEAR OF DATA & REPORT TITLE  #####
# Specify the current year your data is for.
# Set the report title and subtitle if required
# THESE SHOULD BE UPDATED FOR EACH NEW PUBLICATION
title <- "Weekly Deaths in Northern Ireland"

##### HEADER ######
# Select the Statistic type for the report - select from the list of five below

statistic_type <- "as" # options: as  (Accredited Official Statistics),
#          os  (Official Statistics),
#          osd (Official Statistics in Development),
#          mi  (Management Information),
#          rr  (Research Report)

# Set the report publication data and next publication data if required

data_drive <- "T:/Projects/106 - VARS Weekly Deaths/data/"

#### INSTALL PACKAGES  ####
# check for presence of required packages and if necessary,
# install and then load each

library(markdown)
library(broom)
library(rmarkdown)
library(yaml)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(xfun)
library(htmltools)
library(openxlsx)
library(readxl)
library(lubridate)
library(plotly)
library(here)
library(kableExtra)
library(scales)
library(ggrepel)
library(DT)
library(AMR)
library(ggpubr)
library(sf)
library(tmap)
library(htmltools)
library(formattable)
library(httpuv)
library(janitor)
library(odbc)
library(DBI)
library(foreign)
library(fontawesome)
library(ISOweek)
library(ggtext)
library(waffle)
library(patchwork)
library(shadowtext)

# turn off warning messages
options(warn = -1)

# CONFIGURE PATHS -----------------------------------------

# # Connection to Data - edit as needed
# 
# ## SQL Code
# # Set up the connection to the SQL Server database
# con <<- dbConnect(odbc(),
#                   Driver = "SQL Server",
#                   Server = "NISRA16-list",
#                   Database = "CSU_CS_Analysis",
#                   Trusted_Connection = "True")

#### DERIVED TEXT ####

statistic_type_text <- case_when(
  statistic_type == "as" ~ "Accredited Official Statistics",
  statistic_type == "os" ~ "Official Statistics",
  statistic_type == "osd" ~ "Official Statistics in Development",
  statistic_type == "mi" ~ "Management Information",
  statistic_type == "rr" ~ "Research Report",
  TRUE ~ "[UNDEFINED statistic_type_text]"
)

#### CONFIGURE FOLDER PATHS FOR DOWNLOAD BUTTONS #####

# create folder for download button csv and excel files if it doesn't exist
ifelse(!dir.exists(paste0(here(), "/outputs/")), dir.create(paste0(
  here(),
  "/outputs/"
)), "output folder already exists")
ifelse(!dir.exists(paste0(here(), "/outputs/", "figdata/")),
  dir.create(paste0(here(), "/outputs/", "figdata/")),
  "figdata folder already exists"
)


#### NISRA COLOURS AND LOGOS ####
##### LOGOS #####

# NISRA logo
if (bilingual == TRUE) {
  nisra_logo <- here("data/images/Bilingual white logo.svg")
} else {
  nisra_logo <- here("data/images/English only white logo.svg")
}

nisra_logo <- paste0(
  "data:image/svg+xml,",
  readLines(nisra_logo) %>%
    paste(collapse = " ") %>%
    encodeURIComponent()
)

nisra_alt <- "NISRA logo"

# Departmental logo
# Departmental logo
dep_logo <- base64enc::dataURI(
  file = here(paste0("data/images/dept_logos/logo-white-", nicstheme, ".png"))
)
dep_alt <- paste0(toupper(nicstheme), " logo")

# Departmental link


# Accredited Official Statistics logo
acc_official_stats <- paste0(
  "data:image/svg+xml,",
  readLines(
    here("data/images/Accredited Official Statistics Logo English.svg")
  ) %>%
    paste(collapse = " ") %>%
    encodeURIComponent()
)
nat_alt <- "Accredited Official Statistics logo"


##### COLOURS #####
nisra_green_decoration <- "#CEDC20"
nisra_blue <- "#3878c5"
nisra_navy <- "#00205b"
nisra_col3_green <- "#68a41e" # needs black text
nisra_col4_purple <- "#732777"
nisra_col5_lilac <- "#ce70d2" # needs black text

ons_blue <- "#12436d"
ons_green <- "#28a197"
ons_red <- "#801650"
ons_orange <- "#f46a25"

# DE colours
dePink <- "#ca2c93"
deBlue <- "#142062"
deGreen <- '#28A197'
deOrange <- '#E66100'

#### CALL & LOAD FUNCTIONS SCRIPTS ####
for (file in list.files(path = paste0(here(), "/code/", "functions"))) {
  source(paste0(here(), "/code/", "functions/", file))
}

utils::globalVariables(c("new_workbook", ".", "report_final"))


