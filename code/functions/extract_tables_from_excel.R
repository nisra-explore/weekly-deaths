extract_tables_from_excel <- function(
    file_path,
    sheets,
    header_row
) {
  
  tables <- lapply(sheets, function(sheet) {
    
    data <- read_excel(
      file_path,
      sheet = sheet,
      col_names = FALSE
    )
    
    # funciton will calculate where Notes start based on row index of 'Note 1' string
    # number of rows to extract is then passed to f_extract_table
    find_row <- function(data) {
      note1_row <- which(grepl("Note 1:", data[[1]]))
      
      if (length(note1_row) == 0) {
        message("'Note 1' not found - table structure changed")
        return(NULL)
      }
      
      # number of rows to extract = row index - column header index
      n_rows <- note1_row - header_row 
      
      return(n_rows)
    }
    
    n_rows <- find_row(data)
    
    f_extract_table(
      data = data,
      header_row = header_row,
      n_rows = n_rows
    )
  })
  
  names(tables) <- sheets
  
  return(tables)
}