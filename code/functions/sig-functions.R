# Returns number of non blank values in a column "var"
f_return_n <- function(var) {
  length(var[!is.na(var)])
}

# Returns percentage of times a given "value" occurs in column "var" in data frame "data"
# "weight" is set as NA and can be customised in function call
f_return_p <- function(data, var, value, weight = NA) {
  p_data <- data %>%
    filter(!is.na(.[[var]]))
  
  if (is.na(weight)) {
    p <- p_data %>% 
      filter(.[[var]] == value) %>% 
      nrow() / f_return_n(p_data[[var]])
  } else {
    p <- sum(p_data[[weight]][p_data[[var]] == value]) / sum(p_data[[weight]])
  }
  
  return(p)
  
}

# Returns number of blank values in column "var" when "group_var" is filtered by "group_value"
f_return_n_group <- function(var, group_var, group_value) {
  length(var[!is.na(var) & !is.na(group_var) & group_var == group_value])
}

# Returns percentage of times a given "value" occurs in column "var" in data frame "data"
# when "group_var" is filtered by "group_value"
# "weight" is set as NA and can be customised in function call
f_return_p_group <- function(data, var, value, group_var, group_value, weight = NA) {
  
  p_data <- data %>%
    filter(!is.na(.[[var]]) & .[[group_var]] == group_value)
  
  if (is.na(weight)) {
    p <- p_data %>% 
      filter(.[[var]] == value) %>% 
      nrow() / f_return_n(p_data[[var]])
  } else {
    p <- sum(p_data[[weight]][p_data[[var]] == value]) / sum(p_data[[weight]])
  }
  
  return(p)
  
}

# Returns the z score given p1, n1, p2, n2
f_return_z <- function(p1, n1, p2, n2) {
  s1 <- (1 / n1) + (1 / n2)
  s2 <- p1 * n1
  s3 <- p2 * n2
  s4 <- s2 / n1
  s5 <- s3 / n2
  s6 <- s4 - s5
  s7 <- (s2 + s3) / (n1 + n2)
  s8 <- 1 - s7
  s9 <- sqrt(s7 * s8 * s1)
  return(s6 / s9)
}


# Returns data frame displaying Percentages and Base values for question
f_values <- function(data, var) {
  
  data.frame(Response = c(levels(data[[var]]), "Base (n)")) %>% 
    rowwise() %>% 
    mutate(`%` = case_when(Response != "Base (n)" ~ f_return_p(data, var, Response) * 100,
                           TRUE ~ f_return_n(data[[var]])))
  
}

# Returns data frame displaying z scores for all responses to question
f_z_scores <- function(data, var) {
  
  responses <- levels(data[[var]])
  z_scores <- data.frame(Response = responses)
  
  for (i in 1:length(responses)) {
    col <- c()
    for (j in 1:length(responses)) {
      if (i > j) {
        col[j] <- f_return_z(
          p1 = f_return_p(data, var, responses[j]),
          n1 = f_return_n(data[[var]]),
          p2 = f_return_p(data, var, responses[i]),
          n2 = f_return_n(data[[var]])
        )
      } else {
        col[j] <- NA
      }
    }
    z_scores[[responses[i]]] <- col
  }
  
  z_scores
  
}

f_values_group <- function (data, var, group_var, group_label, weight = NA) {
  
  values <- data.frame(grouping = levels(data[[group_var]]),
                       check.names = FALSE)
  
  responses <- levels(data[[var]])
  
  for (i in 1:length(responses)) {
    values <- values %>% 
      rowwise() %>% 
      mutate(response = f_return_p_group(data, var, responses[i], group_var, grouping, weight) * 100)
    
    names(values)[names(values) == "response"] <- paste(responses[i], "(%)")
  }
  
  values <- values %>% 
    rowwise() %>% 
    mutate(`Base (n)` = f_return_n_group(data[[var]], data[[group_var]], grouping))
  
  if (ncol(values) == 4) {
    for (i in 1:nrow(values)) {
      values$`Z Score`[i] <- f_return_z(p1 = values[[2]][i] / 100,
                                        n1 = values[[4]][i],
                                        p2 = values[[3]][i] / 100,
                                        n2 = values[[4]][i])
    }
  }
  
  names(values)[names(values) == "grouping"] <- group_label
  
  values
  
}

# Returns data frame displaying z scores for all responses to question
f_z_scores_group <- function(data, var, group_var, group_label, weight = NA) {
  
  responses <- levels(data[[var]])
  groups <- levels(data[[group_var]])
  dfs <- list()
  
  for (i in 1:length(responses)) {
    z_scores <- data.frame(grouping = groups)
    for (j in 1:length(groups)) {
      col <- c()
      for (k in 1:length(groups)) {
        if (j > k) {
          col[k] <- f_return_z(
            p1 = f_return_p_group(data, var, responses[i], group_var, groups[k], weight),
            n1 = f_return_n_group(data[[var]], data[[group_var]], groups[k]),
            p2 = f_return_p_group(data, var, responses[i], group_var, groups[j], weight),
            n2 = f_return_n_group(data[[var]], data[[group_var]], groups[j])
          )
        } else {
          col[k] <- NA
        }
      }
      z_scores[[groups[j]]] <- col
    }
    names(z_scores)[names(z_scores) == "grouping"] <- group_label
    dfs[[responses[i]]] <- z_scores
  }
  
  dfs
  
}


f_value_table <- function(wb, sheet, df, type) {
  
  names(df) <- gsub("/", "/\n", names(df), fixed = TRUE)
  
  writeData(wb, sheet,
            x = paste(sheet, "-", type),
            startRow = r)
  
  addStyle(wb, sheet,
           style = h3,
           rows = r,
           cols = 1)
  
  r <<- r + 1
  
  writeDataTable(wb, sheet,
                 x = df,
                 startRow = r,
                 tableStyle = "none",
                 headerStyle = hs,
                 withFilter = FALSE)
  
  addStyle(wb, sheet,
           style = hs2,
           rows = r,
           cols = 1)
  
  if (!grepl("Overall", type)) {
    addStyle(wb, sheet,
             style = ns_percent,
             rows = (r + 1):(r + nrow(df)),
             cols = 2:(ncol(df)),
             gridExpand = TRUE)
    
    addStyle(wb, sheet,
             style = ns_comma,
             rows = (r + 1):(r + nrow(df)),
             cols = which(names(df) == "Base (n)"),
             gridExpand = TRUE)
    
    if ("Z Score" %in% names(df)) {
      for (i in 1:nrow(df)) {
        if (abs(df$`Z Score`[i]) > qnorm(0.975)) {
          addStyle(wb, sheet,
                   style = sig,
                   rows = r + i,
                   cols = which(names(df) == "Z Score"))
        } else {
          addStyle(wb, sheet,
                   style = not_sig,
                   rows = r + i,
                   cols = which(names(df) == "Z Score"))
        }
      }
    }
    
  } else {
    addStyle(wb, sheet,
             style = ns_percent,
             rows = (r + 1):(r + nrow(df) - 1),
             cols = 2)
    
    addStyle(wb, sheet,
             style = ns_comma,
             rows = r + nrow(df),
             cols = 2)
  }
  
  
  
  r <<- r + nrow(df) + 2
  
}

f_z_score_table <- function(wb, sheet, df, type, c = 1) {
  
  names(df) <- gsub("/", "/\n", names(df), fixed = TRUE)
  
  writeData(wb, sheet,
            x = paste(sheet, "-", type),
            startRow = r)
  
  addStyle(wb, sheet,
           style = h3,
           rows = r,
           cols = c)
  
  r <<- r + 1
  
  writeDataTable(wb, sheet,
                 x = df,
                 startRow = r,
                 tableStyle = "none",
                 headerStyle = hs,
                 withFilter = FALSE)
  
  addStyle(wb, sheet,
           style = hs2,
           rows = r,
           cols = c)
  
  for (i in 1:nrow(df)) {
    for (j in (c + 1):ncol(df)) {
      if (!is.na(df[i, j])) {
        if (abs(df[i, j]) > qnorm(0.975)) {
          addStyle(wb, sheet,
                   style = sig,
                   rows = r + i,
                   cols = j
          )
        } else {
          addStyle(wb, sheet,
                   style = not_sig,
                   rows = r + i,
                   cols = j
          )
        }
      }
    }
  }
  
  for (i in 1:nrow(df)) {
    addStyle(wb, sheet,
             style = grey,
             rows = r + i,
             cols = c + i
    )
  }
  
  r <<- r + nrow(df) + 2
  
  
}

f_z_group_tables <- function (wb, sheet, df_list, type) {
  
  if (length(df_list) == 2) {
    f_z_score_table(wb, sheet,
                    df_list[[1]],
                    paste0('"', names(df_list)[1], '" - ', type))
  } else {
    for (i in 1:length(df_list)) {
      
      f_z_score_table(wb, sheet,
                      df_list[[i]],
                      paste0('"', names(df_list)[i], '" - ', type))
      
      
    }
  }
}

f_sig_worksheet <- function (wb, sheet, title,
                             values_df, z_scores_df, 
                             lgd_df, lgd_list,
                             urban_rural_df, urban_rural_list,
                             mdm_df, mdm_list,
                             sen_df, sen_list) {
  
  addWorksheet(wb, sheet,
               tabColour = if (length(title) == 1) "#31869B" else "#B7DEE8")
  
  setColWidths(wb, sheet,
               cols = 1:(length(levels(df_sigtest_vars$LGD2014_name)) + 1),
               widths = c(33, rep(14, length(levels(df_sigtest_vars$LGD2014_name)))))
  
  r <<- 1
  
  writeData(wb, sheet,
            x = title)
  
  addStyle(wb, sheet,
           style = ts,
           rows = r:length(title),
           cols = 1)
  
  r <<- r + length(title) + 1
  
  ## Overall Responses ####
  
  f_value_table(wb, sheet, values_df, "Overall Responses") 
  
  f_z_score_table(wb, sheet, z_scores_df, "Overall Responses")
  
  ## Comparing LGD ####
  
  f_value_table(wb, sheet, lgd_df, "Responses by Local Government District (WEIGHTED)")
  
  f_z_group_tables(wb, sheet, lgd_list, "Responses by Local Government District  (WEIGHTED)")
  
  ## Comparing Urban/Rural #### 
  
  f_value_table(wb, sheet, urban_rural_df, "Responses by Urban/Rural")
  
  f_z_group_tables(wb, sheet, urban_rural_list, "Responses by Urban/Rural")
  
  ## Comparing MDM Quintile ####
  
  f_value_table(wb, sheet, mdm_df, "Responses by MDM Quintile (WEIGHTED)")
  
  f_z_group_tables(wb, sheet, mdm_list, "Responses by MDM Quintile (WEIGHTED)")
  
  ## Comparing SENSpecial ####
  
  f_value_table(wb, sheet, sen_df, "Responses by SEN Status")
  
  f_z_group_tables(wb, sheet, sen_list, "Responses by SEN Status")
  
  contents <<- contents %>% 
    bind_rows(
      data.frame(
        `Tab name` = sheet,
        `Question` = title[1],
        `Additional info` = title[2],
        check.names = FALSE
      )
    )
  
}