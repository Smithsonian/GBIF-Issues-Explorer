################################
#Load packages
################################
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(stringr)
library(leaflet)
library(XML)
library(data.table)
library(RSQLite)


################################
#Import data
################################
source("gbifissues.R")
source("database.R")


################################
#Settings
################################
gbif_ie_ver <- "0.1"
database_file <- "data/gbif.sqlite"
occ_file <- "data/occurrence.txt"
ver_file <- "data/verbatim.txt"
multi_file <- "data/multimedia.txt"

#how many rows to import at a time?
no_rows <- 20000


################################
#UI
################################
ui <- fluidPage(
  # App title ----
  titlePanel("GBIF Issues Explorer"),
  fluidRow(column(width = 5,
                  uiOutput("download_doi")
                  ),

           column(width = 7,
                  uiOutput("distinct_issues"),
                  uiOutput("downloadData"),
                  HTML('<script type="text/javascript">
                      $(document).ready(function() {
                        $("#downloadData").click(function() {
                          $("#downloadData2").text("Loading data, please wait...").attr(\'disabled\',\'disabled\');
                        });
                      });
                    </script>
                    ')
           )
      ),
  
  conditionalPanel("input.i == 'None'",
                 h3("Number of records by issue"),
                 plotOutput("summaryPlot", height = 600)
                 ),
  
  hr(), 
  
  fluidRow(column(width=7,
          fluidRow(column(width=8,                
                  HTML("<dl><dt>"),
                  strong(textOutput("issuename")),
                  HTML("</dt><dd>"),
                  textOutput("issuedescript"),
                  HTML("</dd></dl>")
                ),
              column(width=4,
                     uiOutput("no_records")
                    )
              ),

          DT::dataTableOutput("table")
          ),

          column(width=5, 
                conditionalPanel("input.table_rows_selected != ''",
                             uiOutput("recorddetail"),
                             leafletOutput("mymap")
                  )
              )
      ),
  hr(),
  HTML(paste0("<p><a href=\"http://dpo.si.edu\" target = _blank><img src=\"circlelogo.png\"> Digitization Program Office</a> | GBIF Issues Explorer ver. ", gbif_ie_ver, " | <a href=\"https://github.com/Smithsonian/GBIF-Issues-Explorer\" target = _blank>Source code</a></p>"))
)




################################
#Server
################################
server <- function(input, output) {
  
  # Create a Progress object
  # from https://shiny.rstudio.com/articles/progress.html
  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0.05)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  
  #If there is no sqlite database, create it and import occurrence file
  if (!file.exists(database_file)){
    
    progress1 <- shiny::Progress$new()
    progress1$set(message = "Loading occurrence database", value = 0.05)
    on.exit(progress1$close())
    
    progress2 <- shiny::Progress$new()
    progress2$set(message = "Loading verbatim database", value = 0.05)
    on.exit(progress2$close())
    
    progress8 <- shiny::Progress$new()
    progress8$set(message = "Loading datasets info", value = 0.1)
    on.exit(progress1$close())
    
    gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
    
    #index of issues
    n <- dbExecute(gbif_db, create_issuetable_query)
    n <- dbExecute(gbif_db, 'CREATE INDEX issue_issue ON issues(issue);')
    n <- dbExecute(gbif_db, 'CREATE INDEX gbifID_issue ON issues(gbifID);')

    #table with data
    n <- dbExecute(gbif_db, create_table_query)
    n <- dbExecute(gbif_db, 'CREATE INDEX gbifID ON gbif(gbifID);')
    n <- dbExecute(gbif_db, 'CREATE INDEX issue ON gbif(issue);')
    n <- dbExecute(gbif_db, 'CREATE INDEX gb_datasetKey ON gbif(datasetID);')
    n <- dbExecute(gbif_db, 'CREATE INDEX basisOfRecord ON gbif(basisOfRecord);')
    n <- dbExecute(gbif_db, 'CREATE INDEX scientificName ON gbif(scientificName);')
    n <- dbExecute(gbif_db, 'CREATE INDEX ignorerow ON gbif(ignorerow);')
    
    #verbatim table
    n <- dbExecute(gbif_db, verbatim_createtable_query)
    n <- dbExecute(gbif_db, 'CREATE INDEX verbatim_gbifID ON verbatim(gbifID);')
    #n <- dbExecute(gbif_db, 'CREATE INDEX verbatim_issue ON verbatim(issue);')

    #multimedia
    n <- dbExecute(gbif_db, create_multimedia_query)
    n <- dbExecute(gbif_db, 'CREATE INDEX multimedia_gbifID ON multimedia(gbifID);')
    
    #datasets table
    n <- dbExecute(gbif_db, create_datasettable_query)
    n <- dbExecute(gbif_db, 'CREATE INDEX ds_datasetKey ON datasets(datasetKey);')

    datasets_xml <- list.files("data/dataset/", pattern = "*.xml")
    no_datasets <- length(datasets_xml)
    progress_val <- 0.1
    progress_steps <- ((0.9 - progress_val) / no_datasets)
    
    for (i in 1:no_datasets){
      meta_file <- xmlToList(paste0("data/dataset/", datasets_xml[i]))
      datasetKey <- str_replace(datasets_xml[i], ".xml", "")
      datasetTitle <- meta_file$dataset$title
      datasetTitle <- stringr::str_replace_all(datasetTitle, "'", "''")
      dbExecute(gbif_db, paste0("INSERT INTO datasets (datasetKey, title) VALUES ('" , datasetKey, "', '" , datasetTitle, "')"))
      
      progress8$set(value = progress_val, message = "Loading datasets info")
      progress_val <- progress_val + progress_steps
    }
    
    progress8$set(value = 1, message = "Done reading dataset info XML")
    progress8$close()
    
    progress1$set(value = 0.1, message = "Reading occurrence records", detail = "(this will take a while and only happens once)")
    progress2$set(value = 0.1, message = "Reading verbatim records", detail = "(this will take a while and only happens once)")
    
    #how big?
    library(R.utils)
    no_lines <- R.utils::countLines(occ_file)[1]
    
    #how many steps?
    no_steps <- floor(no_lines/no_rows)
    progress_val <- 0.12
    progress_steps <- ((0.9 - progress_val) / no_steps)
    
    #loop to occ_file
    for (i in 1:no_steps){
      if (i == 1){
        gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1)
        verbatim_data <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1)
      }else{
        skip_rows <- i * no_rows
        gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows)
        verbatim_data <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows)
      }
      
      #Set names to df  
      names(gbif_data) <- col_names
      names(verbatim_data) <- verbatim_cols
      
      #write rows
      dbWriteTable(gbif_db, "gbif", gbif_data, append = TRUE)
      dbWriteTable(gbif_db, "verbatim", verbatim_data, append = TRUE)
      
      #progressbar
      progress1$set(value = progress_val, message = "Loading occurrence records", detail = "(this will take a while and only happens once)")
      progress2$set(value = progress_val, message = "Loading verbatim records", detail = "(this will take a while and only happens once)")
      progress_val <- progress_val + progress_steps
    }
    
    rm(gbif_data)
    rm(verbatim_data)
    
    progress2$set(value = 1, message = "Done reading verbatim records")
    progress2$close()
    
    progress1$set(value = 1, message = "Done reading occurrence records")

    progress$set(message = "Loading data", value = 0.4)
    
    
    
    
    
    #Multimedia
    progress3 <- shiny::Progress$new()
    progress3$set(message = "Loading multimedia database", value = 0.05)
    on.exit(progress3$close())
    
    no_lines <- R.utils::countLines(multi_file)[1]
    
    #how many steps?
    no_steps <- floor(no_lines/no_rows)
    progress_val <- 0.12
    progress_steps <- ((0.9 - progress_val) / no_steps)
    
    #loop to make loading easier
    for (i in 1:no_steps){
      if (i == 1){
        gbif_data <- data.table::fread(input = multi_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1)
      }else{
        skip_rows <- i * no_rows
        gbif_data <- data.table::fread(input = multi_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows)
      }
      
      #Set names to df  
      names(gbif_data) <- multimedia_cols
      
      #write rows
      dbWriteTable(gbif_db, "multimedia", gbif_data, append = TRUE)
      
      #progressbar
      progress3$set(value = progress_val, message = "Loading multimedia records", detail = "(this will take a while and only happens once)")
      progress_val <- progress_val + progress_steps
    }
    
    rm(gbif_data)
    
    progress3$set(value = 1, message = "Done reading multimedia records")
    progress3$close()
    
    progress$set(message = "Loading data", value = 0.6)
    
    
    
    
    
    #Statistics of issues
    progress1$set(value = 0.1, message = "Generating statistics")
    
    issue_list <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM gbif WHERE issue != ''")
    
    issues_list <- data.frame(matrix(ncol = 1, nrow = 0, data = NA))
    for (i in 1:dim(issue_list)[1]){
      a <- strsplit(issue_list[i,1], ";")
      for (j in 1:length(a[[1]])){
        issues_list <- c(issues_list, a[[1]][j])
      }
    }
    
    distinct_issues <- unique(unlist(issues_list))
    
    progress_val <- 0.1
    progress_steps <- ((1 - progress_val) / length(distinct_issues))
    
    #summary 
    for (i in 1:length(distinct_issues)){
      dbExecute(gbif_db, paste0("INSERT INTO issues (gbifID, issue) SELECT gbifID, '", distinct_issues[i], "' FROM gbif WHERE issue LIKE '%", distinct_issues[i], "%'"))
      progress1$set(value = progress_val)
      progress_val <- progress_val + progress_steps
    }
    
    progress1$set(value = 1, message = "Done reading occurrence records")
    progress1$close()
    
    
    progress$set(message = "Loading data", value = 0.8)
    
    
    
    
    #Close db to cleanup
    dbDisconnect(gbif_db)
  }
  
  
  
  #Open database
  gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
  
  
  #Download metadata
  dl_meta_file <- xmlToList("data/metadata.xml")
  output$download_doi <- renderText({
    this_doi <- dl_meta_file$additionalMetadata$metadata$`gbif`$citation$.attrs
    HTML(paste0("<a href=\"https://doi.org/", this_doi, "\" target = _blank>GBIF Occurrence Download ", this_doi,"</a>"))
  })
  
  
  #Get rows with issues
  distinct_issues <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM issues")
  distinct_issues <- unlist(distinct_issues, use.names = FALSE)
  
  #summary 
  summary_vals <- data.frame(matrix(ncol = 2, nrow = 0, data = NA))

  for (i in 1:length(distinct_issues)){
    this_issue <- dbGetQuery(gbif_db, paste0("SELECT count(*) FROM issues WHERE issue = '", distinct_issues[i], "'"))
    summary_vals <- rbind(summary_vals, cbind(distinct_issues[i], as.numeric(this_issue[1])))
  }

  names(summary_vals) <- c("issue", "no_records")
  summary_vals$no_records <- as.numeric(paste(summary_vals$no_records))
  
  
  #Sort by no of cases
  summary_vals <- summary_vals[order(-summary_vals$no_records),]
  summary_vals$issue <- factor(summary_vals$issue, levels = summary_vals$issue[order(summary_vals$no_records)])
  
  
  #Done loading data
  progress$set(value = 1.0, message = "Data ready. Loading Shiny app...")
  progress$close()
  
  
  #Plot issues
  output$summaryPlot <- renderPlot({
    ggplot(data = summary_vals, aes(x = issue, y = no_records, label = issue)) +
      geom_col() +
      coord_flip() + 
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      theme(axis.text = element_text(size=12), axis.title = element_text(size=14, face = "bold")) + 
      xlab("Issue") +
      ylab("No. of records")
  })
  
  
  #distinct_issues pulldown
  output$distinct_issues <- renderUI({
    selectInput(inputId = "i",
                label = "Select an issue:", 
                choices = append(c("None - Show summary" = "None"), distinct_issues),
                width = 360
                )
  })
  
  
  #table of records with selected issue
  output$table <- DT::renderDataTable({
      
    req(input$i)
    
    #Which cols to display by type of issue
    if (input$i == "None"){
      cols <- "gbifID, scientificName, decimalLatitude, decimalLongitude, recordedBy, locality, country"
    }else if (input$i %in% spatial_issues){
      cols <- "gbifID, scientificName, decimalLatitude, decimalLongitude, recordedBy, locality, country"
    }else if (input$i %in% depth_issues){
      cols <- "gbifID, scientificName, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth, country"
    }else if (input$i %in% elev_issues){
      cols <- "gbifID, scientificName, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation, country"
    }else if (input$i %in% date_issues){
      cols <- "gbifID, scientificName, eventDate, eventTime, startDayOfYear, endDayOfYear,year, month, day"
    }else{
      cols <- "gbifID, scientificName, recordedBy, locality, country"
    }
    
    if (input$i == "None"){
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues)"))
    }else{
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "')"))
    }
    
    DT::datatable(datarows, escape = FALSE, options = list(searching = TRUE, ordering = TRUE), rownames = FALSE, selection = 'single')
  })
  
  
  #map
  output$mymap <- renderLeaflet({
    
    req(input$i)
    
    if (input$i %in% spatial_issues){
      datarows <- dbGetQuery(gbif_db, paste0("SELECT gbifID, decimalLatitude, decimalLongitude FROM gbif WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "')"))

      points <- datarows[input$table_rows_selected,]
      
      #check if lat and lon exist
      req(points$decimalLongitude)
      req(points$decimalLatitude)
      
      leaflet() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        addMarkers(lng = points$decimalLongitude, lat = points$decimalLatitude) %>%
        setView(points$decimalLongitude, points$decimalLatitude, zoom = 04)
      }
  })
  
  
  #Some details of the record
  output$recorddetail <- renderUI({
      
    req(input$table_rows_selected)
    
    if (input$i == "None"){
      this_summary_ids <- dbGetQuery(gbif_db, "SELECT gbifID FROM gbif WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues)")
    }else{
      this_summary_ids <- dbGetQuery(gbif_db, paste0("SELECT gbifID FROM gbif WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "')"))
    }
    
    
    this_record <- dbGetQuery(gbif_db, paste0("SELECT * FROM gbif WHERE gbifID = ", this_summary_ids[input$table_rows_selected,]))
    this_record_dataset <- dbGetQuery(gbif_db, paste0("SELECT * FROM datasets WHERE datasetKey in (SELECT datasetKey FROM gbif WHERE gbifID = ", this_record$gbifID, ")"))
    
    gbif_record_url <- paste0("https://www.gbif.org/occurrence/", this_record$gbifID)
    occurrenceID <- this_record$occurrenceID
    
    html_to_print <- paste0("<br><h4>Record gbifID: ", this_record$gbifID,"</h4><dl class=\"dl-horizontal\"><dt>GBIF Record</dt><dd><a href=\"", gbif_record_url, "\" target = _blank>", gbif_record_url, "</a></dd>")
    if (occurrenceID != ""){
      html_to_print <- paste0(html_to_print, "<dt>Occurrence ID</dt>")
      #check if it is a URL, just print otherwise
      if (substr(occurrenceID, 0, 4) == "http"){
        html_to_print <- paste0(html_to_print, "<dd><a href=\"", occurrenceID, "\" target = _blank>", occurrenceID, "</a></dd>")
      }else{
        html_to_print <- paste0(html_to_print, "<dd>", occurrenceID, "</dd>")
      }
    }
    
    if (this_record$catalogNumber != ""){
      html_to_print <- paste0(html_to_print, "<dt>Catalog No.</dt><dd>", this_record$catalogNumber, "</dd>")
    }
    
    if (input$i != "None"){
      html_to_print <- paste0(html_to_print, "<dt>Issues</dt>")
      issue_list <- base::strsplit(this_record$issue, ";")[[1]]
      
      for (i in 1:length(issue_list)){
        this_issue <- dplyr::filter(gbifissues, issue == issue_list[i])
        html_to_print <- paste0(html_to_print, "<dd><abbr title=\"", this_issue$description, "\">", this_issue$issue, "</abbr></dd>")
      }
    }
    
    
    #Images
    images <- dbGetQuery(gbif_db, paste0("SELECT * FROM multimedia WHERE gbifID = ", this_record$gbifID, " AND type = 'StillImage'"))
    if (dim(images)[1] > 0){
      
      html_to_print <- paste0(html_to_print, "<dt>Images</dt><dd>")
      
      no_images <- dim(images)[1]
      if (no_images > 3){
        no_images <- 3
      }
      
      for (j in 1:no_images){
        image_url <- images$identifier[j]
        image_title <- images$title[j]
        
        html_to_print <- paste0(html_to_print, "<a href=\"", image_url, "\" target = _blank><img src=\"", image_url, "\" width = \"140px\" alt = \"" , image_title, "\" style = \"padding: 5px;\"></a>")
      }
      html_to_print <- paste0(html_to_print, "</dd>")
    }
    
    
    #Dataset
    html_to_print <- paste0(html_to_print, "<dt>datasetTitle</dt><dd>", this_record_dataset$title, "</dd>")
    html_to_print <- paste0(html_to_print, "<dt>datasetKey</dt><dd>", this_record_dataset$datasetKey, "</dd>")
    
    html_to_print <- paste0(html_to_print, "</dl>")
    HTML(html_to_print)
  })
  
  
  #print name of issue
  output$issuename <- renderText({
    
    req(input$i)
    
    if (input$i != "None"){
      this_issue <- dplyr::filter(gbifissues, issue == input$i)
      paste0("Issue: ", this_issue$issue)
    }else{
      print("Records with no issues")
    }
  })
  
  
  #Count of records
  output$no_records <- renderUI({
    
    req(input$i)
    
    if (input$i != "None"){
      this_count <- summary_vals[summary_vals$issue == input$i,]$no_records
    }else{
      this_count <- dbGetQuery(gbif_db, "SELECT count(gbifID) FROM gbif WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues)")
    }
    if (this_count > 0){
      this_count_pretty <- prettyNum(this_count, big.mark = ",", scientific=FALSE)
      HTML(paste0("<p><strong>", this_count_pretty , " records</strong><br>Click for details</p>"))
    }
  })
  
  
  #print issue description
  output$issuedescript <- renderText({
    
    req(input$i)
    
    if (input$i != "None"){
      this_issue <- dplyr::filter(gbifissues, issue == input$i)
      this_issue$description
    }
  })

  
  # Downloadable csv of selected issue
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(input$i, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dbGetQuery(gbif_db, paste0("SELECT * FROM gbif WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "')")), file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of rows with no issues
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("NO_ISSUES.csv", sep = "")
    },
    content = function(file) {
      write.csv(dbGetQuery(gbif_db, "SELECT * FROM gbif WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues)"), file, row.names = FALSE)
    }
  )
  
  
  output$downloadData <- renderUI({
    
    req(input$i)
    
    if (input$i != "None"){
      downloadButton("downloadData1", "Download GBIF records with this issue", class = "btn-primary")  
    }else{
      downloadButton("downloadData2", "Download GBIF records with no issues", class = "btn-primary")  
    }
  })

}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
