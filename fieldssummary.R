#Explore_fields----
output$explore_fields <- renderUI({
  h4("Summary of each field on the occurrence.txt file")
})


# fields_table ----
output$fields_table <- DT::renderDataTable({
  
  progress8 <- shiny::Progress$new()
  progress8$set(message = "Calculating field statistics", value = 0.05)
  on.exit(progress8$close())
  
  gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
  # Get rows with issues
  fields <- dbGetQuery(gbif_db, "PRAGMA table_info(gbif)")
  
  fields <- dplyr::filter(fields, name != 'ignorerow')
  fields <- dplyr::filter(fields, name != 'gbifid')
  
  fields_summary <- data.frame()
  
  no_rows_total <- dbGetQuery(gbif_db, "SELECT count(*) from gbif")
  
  steps <- 0.95 / (dim(fields)[1])
  progress_val <- 0.05
  
  for (f in seq(1, dim(fields)[1])){
    this_field <- fields$name[f]
    
    #Replace sql keywords
    if (this_field %in% c("group", "order", "references")){
      this_field <- paste0('"', this_field, '"')
    }
    
    #print(this_field)
    no_rows_null_q <- paste0("SELECT count(*) as no_rows from gbif WHERE ", this_field, " IS NULL OR ", this_field, " = ''")
    print(no_rows_null_q)
    no_rows_null <- dbGetQuery(gbif_db, no_rows_null_q)
    
    no_rows_notnull_pc1 <- round(((no_rows_total - no_rows_null)/no_rows_total) * 100, 2)
    no_rows_notnull_pc <- paste0(no_rows_notnull_pc1, " %")
    
    not_null <- paste0("<div class=\"progress\" style=\"background-color: #ffc107;\"><div class=\"progress-bar bg-success\" role=\"progressbar\" style=\"width: ", no_rows_notnull_pc1, "%; background-color: #28a745;\" aria-valuenow=\"", no_rows_notnull_pc1, "\" aria-valuemin=\"0\" aria-valuemax=\"100\" title=\"", no_rows_notnull_pc1, "\">", no_rows_notnull_pc1, "%</div></div>")
    
    no_rows_distinct <- dbGetQuery(gbif_db, paste0("SELECT count(DISTINCT ", this_field, ") as distinct_vals from gbif"))
    
    fields_summary <- rbind(fields_summary, cbind(this_field, not_null, no_rows_distinct))
    
    progress_val <- progress_val + steps
    
    progress8$set(message = "Calculating field statistics", detail = paste0("Analyzed ", f, " of ", dim(fields)[1], " fields."), value = progress_val)
  }
  
  progress8$close()
  
  fields_summary[1] <- lapply(fields_summary[1], as.character)
  
  fields_summary[1] <- lapply(fields_summary[1], str_replace_all, pattern = '"', replacement = '')

  names(fields_summary) <- c("Field", "Rows Not Empty and Not Null", "Distinct Values")
  
  # Close db
  dbDisconnect(gbif_db)
  
  session$userData$fields_summary <- fields_summary
  
  DT::datatable(fields_summary, 
                escape = FALSE, 
                options = list(searching = TRUE, ordering = TRUE, pageLength = 25), 
                rownames = FALSE, 
                selection = 'single')
})




# details of record ----
output$fields_details_h <- renderUI({
  h4("Select a field from the table on the left to see the data values.")
})
  
  

# fields_table ----
output$fields_details <- DT::renderDataTable({
  
  req(input$fields_table_rows_selected)
  
  #Get field
  fields_summary <- session$userData$fields_summary
  
  field_to_check <- as.character(fields_summary[input$fields_table_rows_selected, 1])
  
  output$fields_details_h <- renderUI({
    h4(paste0("Unique data values in data field: '", str_replace_all(field_to_check, '"', ''), "'."))
  })
  
  
  gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
  
  #no rows
  no_rows <- dbGetQuery(gbif_db, "SELECT COUNT(*) FROM gbif")
  
  # Get rows from field
  if (field_to_check %in% c("decimallatitude", "decimallongitude")){
    f_query <- paste0("SELECT CASE WHEN ", field_to_check, " IS NULL THEN '[NULL]' WHEN ", field_to_check, " = '' THEN '[EMPTY]' WHEN length(substr(", field_to_check, ", instr(", field_to_check, ", '.'))) > 6 THEN ", field_to_check, " || ' [SEE PRECISION NOTE BELOW]' ELSE CAST(", field_to_check, " AS text) END , count(*) as no_records FROM gbif GROUP BY ", field_to_check, " ORDER BY no_records DESC")
  }else{
    f_query <- paste0("SELECT CASE WHEN ", field_to_check, " IS NULL THEN '[NULL]' WHEN ", field_to_check, " = '' THEN '[EMPTY]' ELSE ", field_to_check, " END , count(*) as no_records FROM gbif GROUP BY ", field_to_check, " ORDER BY no_records DESC")
  }
  
  print(f_query)
  
  field_data <- dbGetQuery(gbif_db, f_query)
  
  cat(summary(field_data))
  
  for (i in seq(1, dim(field_data)[1])){
    field_data[i, 3] <- round((field_data[i, 2] / no_rows) * 100, 2)
  }
  
  names(field_data) <- c("Field Value", "No. of rows", "Percent of records")

  # Close db
  dbDisconnect(gbif_db)

  DT::datatable(field_data,
                  escape = FALSE,
                  options = list(searching = TRUE, ordering = TRUE, pageLength = 25),
                  rownames = FALSE,
                  selection = 'none')
})



# details of record ----
output$fields_details_h <- renderUI({
  h4("Select a field from the table on the left to see the data values.")
})



# fields_table ----
output$precision_note <- renderUI({
  
  req(input$fields_table_rows_selected)
  
  #Get field
  fields_summary <- session$userData$fields_summary
  
  field_to_check <- as.character(fields_summary[input$fields_table_rows_selected, 1])
  
  if (field_to_check %in% c("decimallatitude", "decimallongitude")){
    HTML("<br><br><br><p><strong>Precision Note</strong>: The precision in lat and lon values with more than 5 decimal places is usually excessive. Values with 6 decimal places measure differences of less than a meter. If the values were converted from another datum, they have to be rounded to the appropriate number of decimal places.")
  }
})
    