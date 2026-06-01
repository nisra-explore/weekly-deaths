f_clean_week_names <- function(x) {
  x_clean <- x |>
    str_replace_all("\\r\\n|\\r|\\n", " ") |>
    str_remove_all("\\[.*?\\]") |>
    str_squish()
  
  out <- case_when(
    x_clean == "Sex" ~ "Sex",
    x_clean == "Age" ~ "Age",
    
    # keep the to-date column identifiable
    str_detect(x_clean, regex("to date", ignore_case = TRUE)) ~ 
      "week_1_2026_to_date",
    
    # convert week columns to ISO year-week like 2025W01
    str_detect(x_clean, "^Week\\s*\\d+") ~ {
      date_txt <- str_extract(x_clean, "\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}")
      dt <- dmy(date_txt)
      paste0(isoyear(dt), "W", sprintf("%02d", isoweek(dt)))
    },
    
    TRUE ~ x_clean
  )
  
  out
}