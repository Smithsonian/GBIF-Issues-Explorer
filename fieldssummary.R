
#explore_fields----
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
  
  fields_summary <- data.frame()
  
  no_rows_total <- dbGetQuery(gbif_db, "SELECT count(*) from gbif")
  
  steps <- 0.95 / (dim(fields)[1])
  progress_val <- 0.05
  
  for (f in seq(1, dim(fields)[1])){
    this_field <- fields$name[f]
    
    no_rows_notnull <- dbGetQuery(gbif_db, paste0("SELECT count(*) as no_rows from gbif WHERE ", this_field, " IS NOT NULL"))
    no_rows_notnull_pc <- paste0(round((no_rows_notnull/no_rows_total) * 100, 2), " %")
    
    no_rows_distinct <- dbGetQuery(gbif_db, paste0("SELECT count(DISTINCT ", this_field, ") as distinct_vals from gbif"))
    
    fields_summary <- rbind(fields_summary, cbind(fields$name, no_rows_notnull_pc, no_rows_distinct))
    
    progress_val <- progress_val + steps
    
    progress8$set(message = "Calculating field statistics", value = progress_val)
  }
  
  progress8$close()
  
  DT::datatable(fields_summary, escape = FALSE, options = list(searching = TRUE, ordering = TRUE, pageLength = 50), rownames = FALSE, selection = 'none')
})
