# this function will add two cols to the dataframe
# week_ending_date = proper Date values for the date rows, NA for "Total to Date"
# week_ending_display = formatted text like 09-01-26 plus "Total to Date"

f_date_conversion <- function(df) {

x <- as.character(df$`Week Ending (Friday)`)

num_x <- suppressWarnings(as.numeric(x))

df$week_ending_date <- as.Date(
  ifelse(!is.na(num_x), num_x, NA),
  origin = "1899-12-30"
)

df$week_ending_display <- ifelse(
  is.na(df$week_ending_date),
  x,
  format(df$week_ending_date, "%d-%m-%y")
 )
return(df)
}