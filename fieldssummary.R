#Explore_fields----
output$explore_fields <- renderUI({
  HTML("&nbsp;")
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
    
    print(this_field)
    
    no_rows_notnull <- dbGetQuery(gbif_db, paste0("SELECT count(*) as no_rows from gbif WHERE ", this_field, " IS NOT NULL"))
    
    no_rows_notnull_pc1 <- round((no_rows_notnull/no_rows_total) * 100, 2)
    no_rows_notnull_pc <- paste0(no_rows_notnull_pc1, " %")
    
    not_null <- paste0("<div class=\"progress\" style=\"background-color: #dc3545;\"><div class=\"progress-bar bg-success\" role=\"progressbar\" style=\"width: ", no_rows_notnull_pc1, "%; background-color: #28a745;\" aria-valuenow=\"", no_rows_notnull_pc1, "\" aria-valuemin=\"0\" aria-valuemax=\"100\" title=\"", no_rows_notnull_pc1, "\">", no_rows_notnull_pc1, "%</div></div>")
    
    no_rows_distinct <- dbGetQuery(gbif_db, paste0("SELECT count(DISTINCT ", this_field, ") as distinct_vals from gbif"))
    
    fields_summary <- rbind(fields_summary, cbind(this_field, not_null, no_rows_distinct))
    
    progress_val <- progress_val + steps
    
    progress8$set(message = "Calculating field statistics", detail = paste0("Analyzed ", f, " of ", dim(fields)[1], " fields."), value = progress_val)
  }
  
  progress8$close()
  
  names(fields_summary) <- c("Field", "No. of rows not null", "No. of distinct values")
  
  # Close db
  dbDisconnect(gbif_db)
  
  session$userData$fields_summary <- fields_summary
  
  DT::datatable(fields_summary, 
                escape = FALSE, 
                options = list(searching = TRUE, ordering = TRUE, pageLength = 25), 
                rownames = FALSE, 
                selection = 'single')
})


# fields_table ----
output$fields_details <- DT::renderDataTable({
  
  req(input$fields_table_rows_selected)
  
  #Get field
  fields_summary <- session$userData$fields_summary
  
  field_to_check <- as.character(fields_summary[input$fields_table_rows_selected, 1])
  
  gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
  
  #no rows
  no_rows <- dbGetQuery(gbif_db, "SELECT COUNT(*) FROM gbif")
  
  # Get rows from field
  f_query <- paste0("SELECT CASE WHEN ", field_to_check, " IS NULL THEN '[NULL]' WHEN ", field_to_check, " = '' THEN '[EMPTY]' ELSE ", field_to_check, " END , count(*) as no_records FROM gbif GROUP BY ", field_to_check, " ORDER BY no_records DESC")
  
  print(f_query)
  
  field_data <- dbGetQuery(gbif_db, f_query)
  
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