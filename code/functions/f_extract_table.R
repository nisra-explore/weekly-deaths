f_extract_table <- function(data,header_row, n_rows) {
  # column names
  col_names <- unlist(data[header_row + 1, ])
  col_names <- col_names[!is.na(col_names)]
  
  # table rows
  table_data <- data[(header_row + 2):(n_rows + 2), ]
  names(table_data) <- col_names
  
  return(table_data)
}