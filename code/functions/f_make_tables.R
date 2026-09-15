# ##############################################################################
# f_make_tables.R
# Creates downloadable CSV and Excel files from data frames with formatted  
# tables, generates download buttons for embedding in report
# #############################################################################


f_make_tables <- function(
    data,
    title,
    footnotes = NA,
    data_style = ns_comma,
    data_dir = here::here("outputs/figdata"),
    image_id = NULL,
    plotly_id = NULL,
    plotly_ids = NULL,
    plotly_label = NULL,
    plotly_title = NULL,
    plotly_filename = NULL
) {
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
                         " (in CSV format)", " (", csv_size, ")"
                       )
  )
  em_xl <- embed_file(paste0(data_dir, excel_file),
                      text = paste0(
                        sub("fig", "Figure ", sheet),
                        " (in Excel format)", " (", xl_size, ")"
                      )
  )
  
  # Create a download item for a static image, such as a ggplot
  static_image_download <- function(label, target_id) {
    id_js <- jsonlite::toJSON(
      as.character(target_id),
      auto_unbox = TRUE
    )
    
    filename_js <- jsonlite::toJSON(
      paste0(
        gsub("[^A-Za-z0-9._-]+", "_", label),
        ".png"
      ),
      auto_unbox = TRUE
    )
    
    tags$div(
      class = "download-dropdown-item",
      tags$a(
        href = "#",
        onclick = sprintf(
          "
event.preventDefault();

const root = document.getElementById(%s);
const img = root && (
  root.matches('img') ? root : root.querySelector('img')
);

if (!img) {
  console.error('Image not found:', %s);
  return false;
}

const link = document.createElement('a');
link.href = img.currentSrc || img.src;
link.download = %s;

document.body.appendChild(link);
link.click();
link.remove();

return false;
",
  id_js,
  id_js,
  filename_js
        ),
  paste0(label, " (as image)")
      )
    )
  }

# Create a download item for a Plotly chart
plotly_image_download <- function(
    label,
    target_id,
    export_title = label,
    filename = label,
    logo_source = paste0(
      "https://nisra-tech-lab.github.io/",
      "rs-resources/img/nisra-only-colour.png"
    )
) {
  id_js <- jsonlite::toJSON(
    as.character(target_id),
    auto_unbox = TRUE
  )
  
  title_js <- jsonlite::toJSON(
    paste0("<b>", export_title, "</b>"),
    auto_unbox = TRUE
  )
  
  logo_js <- jsonlite::toJSON(
    logo_source,
    auto_unbox = TRUE
  )
  
  filename_js <- jsonlite::toJSON(
    gsub(
      "[^A-Za-z0-9._-]+",
      "_",
      filename
    ),
    auto_unbox = TRUE
  )
  
  tags$div(
    class = "download-dropdown-item",
    
    tags$a(
      href = "#",
      
      onclick = sprintf(
        "
event.preventDefault();

const root = document.getElementById(%s);

const gd = root && (
  root.matches('.js-plotly-plot')
    ? root
    : root.querySelector('.js-plotly-plot')
);

if (!gd) {
  console.error(
    'Plotly chart not found:',
    %s
  );

  return false;
}

const originalAnnotations =
  gd.layout.annotations || [];

const originalImages =
  gd.layout.images || [];

const originalMargin =
  Object.assign(
    {},
    gd.layout.margin || {}
  );

const originalPlotBackground =
  gd.layout.plot_bgcolor;

const originalPaperBackground =
  gd.layout.paper_bgcolor;

const exportTitle = {
  text: %s,
  x: 0.5,
  y: 1.12,
  xref: 'paper',
  yref: 'paper',
  showarrow: false,
  xanchor: 'center',
  yanchor: 'bottom',

  font: {
    family: 'Arial',
    size: 22,
    color: 'black'
  }
};

const exportLogo = {
  source: %s,
  xref: 'paper',
  yref: 'paper',
  x: 1,
  y: -0.16,
  sizex: 0.13,
  sizey: 0.13,
  xanchor: 'right',
  yanchor: 'bottom',
  sizing: 'contain',
  opacity: 1,
  layer: 'above'
};

Plotly.relayout(
  gd,
  {
    annotations:
      originalAnnotations.concat(
        [exportTitle]
      ),

    images:
      originalImages.concat(
        [exportLogo]
      ),

    margin:
      Object.assign(
        {},
        originalMargin,
        {
          l: 80,
          r: 40,
          b: 130,
          t: 130
        }
      ),

    plot_bgcolor: '#FFFFFF',
    paper_bgcolor: '#FFFFFF'
  }
)
.then(function() {
  return Plotly.downloadImage(
    gd,
    {
      format: 'png',
      filename: %s,
      width: 1200,
      height: 800,
      scale: 2
    }
  );
})
.catch(function(error) {
  console.error(
    'Plotly download failed:',
    error
  );
})
.finally(function() {
  return Plotly.relayout(
    gd,
    {
      annotations: originalAnnotations,
      images: originalImages,
      margin: originalMargin,

      plot_bgcolor:
        originalPlotBackground,

      paper_bgcolor:
        originalPaperBackground
    }
  );
});

return false;
",
      id_js,
      id_js,
      title_js,
      logo_js,
      filename_js
      ),
    
    # Short text displayed in the dropdown
    paste0(label, " (as image)")
    )
  )
}

image_items <- list()

# Static image download
if (!is.null(image_id)) {
  image_items <- append(
    image_items,
    list(
      static_image_download(
        label = title,
        target_id = image_id
      )
    )
  )
}

# Multiple Plotly downloads
if (!is.null(plotly_ids)) {
  if (
    is.null(names(plotly_ids)) ||
    any(names(plotly_ids) == "")
  ) {
    stop(
      paste(
        "plotly_ids must be a named",
        "vector or named list."
      )
    )
  }
  
  image_items <- append(
    image_items,
    lapply(
      names(plotly_ids),
      function(label) {
        plotly_image_download(
          label = label,
          target_id = plotly_ids[[label]],
          export_title = label,
          filename = label
        )
      }
    )
  )
  
  # Single Plotly download
} else if (!is.null(plotly_id)) {
  dropdown_label <- if (
    is.null(plotly_label)
  ) {
    title
  } else {
    plotly_label
  }
  
  export_title <- if (
    is.null(plotly_title)
  ) {
    title
  } else {
    plotly_title
  }
  
  export_filename <- if (
    is.null(plotly_filename)
  ) {
    dropdown_label
  } else {
    plotly_filename
  }
  
  image_items <- append(
    image_items,
    list(
      plotly_image_download(
        label = dropdown_label,
        target_id = plotly_id,
        export_title = export_title,
        filename = export_filename
      )
    )
  )
}

  # Create one download button with a dropdown menu
  buttons <- tags$div(
    class = "table-download-container",
    
    tags$details(
      class = "download-dropdown",
      
      tags$summary(
        class = "download-dropdown-button",
        "Download"
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
        ),
        
        do.call(
          htmltools::tagList,
          image_items
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