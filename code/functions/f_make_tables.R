# ##############################################################################
# f_make_tables.R
# Creates downloadable CSV and Excel files from data frames with formatted  
# tables, generates download buttons for embedding in report
# #############################################################################


f_make_tables <- function(data,
                          title,
                          footnotes = NA,
                          data_style = ns_comma,
                          data_dir = here("outputs/figdata")) {
  require(openxlsx)
  require(janitor)
  require(htmltools)
  require(xfun)
  
  # Sheet name for excel is generated as everything before the : in title
  sheet <- gsub("(.*):.*", "\\1", title)
  
  # File name generated from sheet name
  
  csv_file <- sub(" ", "-", tolower(paste0(
    "weekly-deaths-", sheet, "-",
    week_ending, ".csv"
  )))
  excel_file <- sub(".csv", ".xlsx", csv_file, fixed = TRUE)
  
  
  # Write the dataframe to the csv file
  write.table(data,
              file = paste0(data_dir, csv_file),
              append = FALSE,
              sep = ",",
              row.names = FALSE,
              fileEncoding = "utf-16le"
  )
  
  # Write the excel file
  # Creates a new excel workbook
  wb <- createWorkbook(
    creator = "VARS",
    title = title,
    subject = "Metadata subject",
    category = "Metadata category"
  )
  
  modifyBaseFont(wb, fontSize = 12, fontName = "Arial")
  
  # Adds a sheet
  addWorksheet(wb, sheet)
  
  r <- 1
  
  # Adds a title
  writeData(wb, sheet, title, startCol = 1, startRow = r)
  
  addStyle(wb,
           sheet = as.character(sheet),
           style = ts,
           rows = r,
           cols = 1
  )
  
  r <- r + 1
  
  writeData(wb, sheet, "Source: VARS Weekly Death Dashboard", startCol = 1, startRow = r)
  
  r <- r + 1
  
  if (!is.na(footnotes)) {
    writeData(wb, sheet, c("Notes:", footnotes),
              startCol = 1, startRow = r
    )
    
    r <- r + 1 + length(footnotes)
  }
  
  
  
  # Adds the dataframe
  writeDataTable(wb, sheet, data,
                 startCol = 1, startRow = r, colNames = TRUE,
                 tableName = paste0("table_", sub("^\\D*(\\d+).*$", "\\1", title)),
                 withFilter = FALSE,
                 bandedRows = FALSE,
                 tableStyle = "none",
                 headerStyle = hs
  )
  
  addStyle(wb,
           sheet = as.character(sheet),
           style = hs2,
           rows = r,
           cols = 1
  )
  
  addStyle(wb,
           sheet = as.character(sheet),
           style = la,
           rows = r + seq_len(nrow(data)),
           cols = 1,
           gridExpand = TRUE
  )
  
  
  
  # Identify column types
  num_cols  <- which(sapply(data, is.numeric))
  date_cols <- which(sapply(data, inherits, "Date"))
  
  # Remove date columns from numeric styling
  num_cols <- setdiff(num_cols, date_cols)
  
  # Applying numeric styling ONLY to numeric columns
  if (length(num_cols) > 0) {
    addStyle(
      wb,
      sheet = as.character(sheet),
      style = data_style,
      rows = r + seq_len(nrow(data)),
      cols = num_cols,
      gridExpand = TRUE
    )
  }
  
  
  # Source is added below last table
  
  setColWidths(wb, sheet, cols = 1, widths = 28)
  setColWidths(wb, sheet, cols = 2:length(data), widths = 14)
  
  
  # Applying date styling (MUST be last)
  if (length(date_cols) > 0) {
    date_style <- createStyle(numFmt = "yyyy-mm-dd")
    
    addStyle(
      wb,
      sheet = as.character(sheet),
      style = date_style,
      rows = r + seq_len(nrow(data)),
      cols = date_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  
  
  # Workbook saved
  saveWorkbook(wb, paste0(data_dir, excel_file), overwrite = TRUE)
  
  csv_size <- round_half_up(file.size(paste0(data_dir, csv_file)) / 1000)
  
  csv_size <- if (csv_size == 0) {
    "1kB"
  } else {
    paste0(csv_size, "kB")
  }
  
  xl_size <- paste0(
    round_half_up(file.size(paste0(data_dir, excel_file)) / 1000),
    "kB"
  )
  
  
  em_csv <- embed_file(paste0(data_dir, csv_file),
                       text = paste0(
                         sub("fig", "Figure ", sheet),
                         ".CSV", " (", csv_size, ")"
                       )
  )
  em_xl <- embed_file(paste0(data_dir, excel_file),
                      text = paste0(
                        sub("fig", "Figure ", sheet),
                        ".XLSX", " (", xl_size, ")"
                      )
  )
  
  # Create one download button with a dropdown menu
  buttons <- tags$div(
    class = "table-download-container",
    
    tags$details(
      class = "download-dropdown",
      
      tags$summary(
        class = "download-dropdown-button",
        "Download data"
      ),
      
      tags$div(
        class = "download-dropdown-menu",
        
        tags$div(
          class = "download-dropdown-item",
          em_csv
        ),
        
        tags$div(
          class = "download-dropdown-item",
          em_xl
        )
      )
    )
  )
  
  # Ensure embedded HTML is rendered rather than printed as escaped text
  buttons_html <- as.character(buttons)
  
  if (grepl("&lt;", buttons_html, fixed = TRUE)) {
    buttons_html <- gsub(
      "&lt;",
      "<",
      buttons_html,
      fixed = TRUE
    )
    
    buttons_html <- gsub(
      "&gt;",
      ">",
      buttons_html,
      fixed = TRUE
    )
    
    HTML(buttons_html)
  } else {
    buttons
  }
}