# Persistent database? ----
# T: Keep the database once built, or
# F: destroy at the end of each session
persistent_db <- FALSE

# Don't edit anything below this line


# Load shiny ----
library(shiny)

# Load/install packages ----
#from https://stackoverflow.com/a/4090208
list.of.packages <- c("DT", "dplyr", "ggplot2", "stringr", "leaflet", "XML", "data.table", "RSQLite", "jsonlite", "R.utils")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  install.packages(new.packages)
}

for (package in list.of.packages){
  library(package, character.only = TRUE)
}



# Import definitions, cols, functions ----
source("functions.R")




# Settings ----
app_name <- "GBIF Issues Explorer"
app_ver <- "0.2"
github_link <- "https://github.com/Smithsonian/GBIF-Issues-Explorer"


occ_file <- "data/occurrence.txt"
ver_file <- "data/verbatim.txt"
multi_file <- "data/multimedia.txt"

#how many rows to import at a time?
no_rows <- 20000






# UI ----
ui <- fluidPage(
  # App title ----
  #titlePanel(app_name),
  #tabsetPanel(type = "tabs",
  navbarPage(app_name,
              tabPanel("Summary", 
                fluidRow(column(width = 4,
                                br(),
                                uiOutput("ask_key")
                                )
                    ),
                
           
                fluidRow(
                  column(width = 6,
                         br(),
                         uiOutput("download_doi")
                  ),
                  column(width = 6,
                         br(),
                         #h3("Number of records by issue"),
                         plotOutput("summaryPlot", height = 600)
                         )
                ),
                
                fluidRow(
                  column(width = 6,
                         br(),
                         #h3("Related Issues"),
                         plotOutput("summaryPlot3", height = 600)
                         ),
                  column(width = 6,
                         br(),
                         #h3("Number of issues per record"),
                         #p("Percent is from total no. of rows"),
                         plotOutput("summaryPlot2", height = 600)
                         )
                ),
                
                hr()
                
                
                  ),
              tabPanel("Explore", 
                       fluidRow(
                       column(width = 4,
                              br(),
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
                                   ),
                       column(width = 4,
                              br(),
                              uiOutput("downloadOccFileInfo")
                       ),
                       column(width = 4,
                              br(),
                              uiOutput("downloadVerFileInfo")
                              
                       )
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
                                               #HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Record details</h3></div><div class=\"panel-body\">"),
                                               uiOutput("recorddetail_top"),
                                               uiOutput("recorddetail"),
                                               leafletOutput("mymap"),
                                               uiOutput("recorddetail_bot")
                                               #HTML("</div></div>")
                              )
                       )
                       )
              ),
              tabPanel("Help", 
                       br(),
                       fluidRow(
                         column(width = 6, 
                                uiOutput("help1")
                         ),
                         column(width = 6, 
                                uiOutput("help2")
                         )
                       )
              )
  ),
  hr(),
  HTML(paste0("<p><a href=\"http://dpo.si.edu\" target = _blank><img src=\"circlelogo.png\"> Digitization Program Office</a> | ", app_name, " ver. ", app_ver, " | <a href=\"", github_link, "\" target = _blank>Source code</a></p>"))
)





# Server ----
server <- function(input, output, session) {
  
  rm("registerShinyDebugHook", envir = as.environment("tools:rstudio"))
  dir.create("data", showWarnings = FALSE)
  
  if (persistent_db){
    database_file <- "data/gbif.sqlite3"
  }else{
    database_file <- paste0("data/gbif_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite3")
  }
  
  
  # ask for key ----
  output$ask_key <- renderUI({
    if (!file.exists(database_file)){
        tagList(
          textInput("gbif_key", "Enter a GBIF download Key"),
          actionButton("submitkey", 
                       label = "Submit", 
                       class = "btn btn-primary",
                       icon = icon("triangle-right", lib = "glyphicon"))
        )
     }
    })
  
  
  # Submit button ----
  observeEvent(input$submitkey, {
    cat(input$gbif_key)
    gbif_metadata <- download_gbif(input$gbif_key, "data")
    if (class(gbif_metadata) == "list"){
      gbif_metadata <<- gbif_metadata
    }else{
      cat("Could not find key")
    }
    
    if (file.exists("data/occurrence.txt")){
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Loading data", value = 0.05)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      # No db, occ file ----
      if (!file.exists(database_file)){
        
        progress0 <- shiny::Progress$new()
        progress0$set(message = "Creating database", value = 0.05)
        on.exit(progress0$close())
        
        progress1 <- shiny::Progress$new()
        progress1$set(message = "Loading occurrence table", value = 0.05)
        on.exit(progress1$close())
        
        progress2 <- shiny::Progress$new()
        progress2$set(message = "Loading verbatim table", value = 0.05)
        on.exit(progress2$close())
        
        progress3 <- shiny::Progress$new()
        progress3$set(message = "Loading multimedia table", value = 0.05)
        on.exit(progress3$close())
        
        
        db_created <- try(create_database(database_file, "data/dataset/"))
        
        
        if (class(db_created) == "try-error"){
          stop("Could not create database.")
        }
        
        gbif_db <<- dbConnect(RSQLite::SQLite(), database_file)
        
        progress1$set(value = 1, message = "Creating database")
        progress0$close()
        
        progress1$set(value = 0.1, message = "Reading occurrence records", detail = long_loading_msg)
        progress2$set(value = 0.1, message = "Reading verbatim records", detail = long_loading_msg)
        progress3$set(value = 0.1, message = "Reading multimedia records", detail = long_loading_msg)
        
        #how big?
        no_lines <- R.utils::countLines(occ_file)[1]
        
        #how many steps?
        no_steps <- ceiling(no_lines/no_rows)
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
          progress1$set(value = progress_val, message = "Loading occurrence records", detail = long_loading_msg)
          progress2$set(value = progress_val, message = "Loading verbatim records", detail = long_loading_msg)
          progress_val <- progress_val + progress_steps
        }
        
        rm(gbif_data)
        rm(verbatim_data)
        
        progress2$set(value = 1, message = "Done reading verbatim records")
        progress2$close()
        
        progress1$set(value = 1, message = "Done reading occurrence records")
        
        progress$set(message = "Loading data", value = 0.4)
        
        
        
        #multimedia file ----
        no_lines <- R.utils::countLines(multi_file)[1]
        
        if (no_lines > 1){
          #how many steps?
          no_steps <- ceiling(no_lines/no_rows)
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
            progress3$set(value = progress_val, message = "Loading multimedia records", detail = long_loading_msg)
            progress_val <- progress_val + progress_steps
          }
          
          rm(gbif_data)
        }
        
        
        progress3$set(value = 1, message = "Done reading multimedia records")
        progress3$close()
        
        progress$set(message = "Loading data", value = 0.6)
        
        
        
        
        
        # Statistics of issues ----
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
        
        progress1$set(value = 1.0, message = "Done reading occurrence records")
        progress1$close()
        
        progress$set(message = "Loading data", value = 0.9)
        
        #Delete tables
        try(unlink("data/dataset", recursive = TRUE), silent = TRUE)
        #try(unlink("data/*.txt", recursive = TRUE), silent = TRUE)
        #try(unlink("data/*.xml", recursive = TRUE), silent = TRUE)
        
        progress$set(message = "Loading data", value = 1.0)
        progress$close()
        
        # Close db ----
        dbDisconnect(gbif_db)
      }
      
      
      # Open database ----
      gbif_db <<- dbConnect(RSQLite::SQLite(), database_file)
      
      
      # Metadata of Download ----
      dl_meta_file <- xmlToList("data/metadata.xml")
      output$download_doi <- renderText({
        this_doi <- dl_meta_file$additionalMetadata$metadata$`gbif`$citation$.attrs
        gbif_key <- dl_meta_file$dataset$alternateIdentifier
        
        metadata_json <- paste0("http://api.gbif.org/v1/occurrence/download/", gbif_key)
        
        gbif_metadata <- unlist(jsonlite::fromJSON(metadata_json))
        
        html_to_print <- paste0("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">GBIF Occurrence Download Metadata</h3></div><div class=\"panel-body\"><div style = \"overflow-y: auto; overflow-x: scroll;\"><dl class=\"dl-horizontal\">")
        
        for (i in 1:length(gbif_metadata)){
          html_to_print <- paste0(html_to_print, "<dt>", names(gbif_metadata[i]), "</dt>")
          if (names(gbif_metadata[i]) == "doi"){
            html_to_print <- paste0(html_to_print, "<dd><a href=\"https://doi.org/", gbif_metadata[i], "\" target = _blank>", gbif_metadata[i], "</a></dd>")
          }else if(names(gbif_metadata[i]) == "downloadLink" || names(gbif_metadata[i]) == "license"){
            html_to_print <- paste0(html_to_print, "<dd><a href=\"", gbif_metadata[i], "\" target = _blank>", gbif_metadata[i], "</a></dd>")
          }else{
            html_to_print <- paste0(html_to_print, "<dd>", gbif_metadata[i], "</dd>")
          }
          
        }
        
        html_to_print <- paste0(html_to_print, "</dl>")
        html_to_print <- paste0(html_to_print, "<p><a href=\"", metadata_json, "\" target = _blank>Metadata JSON</a>")
        html_to_print <- paste0(html_to_print, "</div></div></div>")
        HTML(html_to_print)
      })
      
      
      # Get rows with issues ----
      distinct_issues <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM issues")
      distinct_issues <- unlist(distinct_issues, use.names = FALSE)
      
      #summary 
      summary_vals <- data.frame(matrix(ncol = 2, nrow = 0, data = NA))
      
      for (i in 1:length(distinct_issues)){
        this_issue <- dbGetQuery(gbif_db, paste0("SELECT count(*) FROM issues WHERE issue = '", distinct_issues[i], "'"))
        summary_vals <- rbind(summary_vals, cbind(distinct_issues[i], as.numeric(this_issue[1])))
      }
      
      #no issues
      this_issue <- dbGetQuery(gbif_db, paste0("SELECT count(*) FROM issues WHERE issue = ''"))
      summary_vals <- rbind(summary_vals, cbind("None", as.numeric(this_issue[1])))
      
      names(summary_vals) <- c("issue", "no_records")
      summary_vals$no_records <- as.numeric(paste(summary_vals$no_records))
      
      
      #Sort by no of cases
      summary_vals <- summary_vals[order(-summary_vals$no_records),]
      summary_vals$issue <- factor(summary_vals$issue, levels = summary_vals$issue[order(summary_vals$no_records)])
      
      summary_vals <<- summary_vals
      #Done loading data
      progress$set(value = 1.0, message = "Data ready. Loading Shiny app...")
      progress$close()
    }
    
    
    
    
    
  
    
    #distinct_issues pulldown
    output$distinct_issues <- renderUI({
      selectInput(inputId = "i",
                  label = "Select an issue:", 
                  choices = append(c("None - Show summary" = "None"), distinct_issues),
                  width = 360
      )
    })

    
    # Summary ----
    eval(parse("summary.R"))
    
    
    
  })
  
  
  
  # Persistent ----
  if (persistent_db && file.exists(database_file)){
    
    gbif_db <<- dbConnect(RSQLite::SQLite(), database_file)
    
    # Metadata of Download ----
    dl_meta_file <- xmlToList("data/metadata.xml")
    output$download_doi <- renderText({
      this_doi <- dl_meta_file$additionalMetadata$metadata$`gbif`$citation$.attrs
      gbif_key <- dl_meta_file$dataset$alternateIdentifier
      
      metadata_json <- paste0("http://api.gbif.org/v1/occurrence/download/", gbif_key)
      
      gbif_metadata <- unlist(jsonlite::fromJSON(metadata_json))
      
      html_to_print <- paste0("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">GBIF Occurrence Download Metadata</h3></div><div class=\"panel-body\"><div style = \"overflow-y: auto; overflow-x: scroll;\"><dl class=\"dl-horizontal\">")
      
      for (i in 1:length(gbif_metadata)){
        html_to_print <- paste0(html_to_print, "<dt>", names(gbif_metadata[i]), "</dt>")
        if (names(gbif_metadata[i]) == "doi"){
          html_to_print <- paste0(html_to_print, "<dd><a href=\"https://doi.org/", gbif_metadata[i], "\" target = _blank>", gbif_metadata[i], "</a></dd>")
        }else if(names(gbif_metadata[i]) == "downloadLink" || names(gbif_metadata[i]) == "license"){
          html_to_print <- paste0(html_to_print, "<dd><a href=\"", gbif_metadata[i], "\" target = _blank>", gbif_metadata[i], "</a></dd>")
        }else{
          html_to_print <- paste0(html_to_print, "<dd>", gbif_metadata[i], "</dd>")
        }
        
      }
      
      html_to_print <- paste0(html_to_print, "</dl>")
      html_to_print <- paste0(html_to_print, "<p><a href=\"", metadata_json, "\" target = _blank>Metadata JSON</a>")
      html_to_print <- paste0(html_to_print, "</div></div></div>")
      HTML(html_to_print)
    })
    
    
    # Get rows with issues
    distinct_issues <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM issues")
    distinct_issues <- unlist(distinct_issues, use.names = FALSE)
    
    #distinct_issues pulldown
    output$distinct_issues <- renderUI({
      selectInput(inputId = "i",
                  label = "Select an issue:", 
                  choices = append(c("None - Show summary" = "None"), distinct_issues),
                  width = 360
      )
    })
    
    
    # Summary ----
    eval(parse("summary.R"))
    
  }
  
  
  # Table of records with issue ----
  datarows <- reactive({
    req(input$i)
    #gbif_db <<- dbConnect(RSQLite::SQLite(), database_file)
    
    #Which cols to display by type of issue
    if (input$i == "None"){
      cols <- "gbifID, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% spatial_issues){
      cols <- "gbifID, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% depth_issues){
      cols <- "gbifID, scientificName, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth"
    }else if (input$i %in% elev_issues){
      cols <- "gbifID, scientificName, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation"
    }else if (input$i %in% date_issues){
      cols <- "gbifID, scientificName, eventDate, eventTime, startDayOfYear, endDayOfYear,year, month, day"
    }else{
      cols <- "gbifID, scientificName, recordedBy, locality, country"
    }
    
    if (input$i == "None"){
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues) and gbifID NOT IN (SELECT gbifID FROM gbif WHERE ignorerow = 1)"))
    }else{
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "') and gbifID NOT IN (SELECT gbifID FROM gbif WHERE ignorerow = 1)"))
    }
    df <- data.frame(datarows, stringsAsFactors = FALSE)
    
    df
  })

  output$table <- DT::renderDataTable({

    req(input$i)
    #gbif_db <<- dbConnect(RSQLite::SQLite(), database_file)

    #Which cols to display by type of issue
    if (input$i == "None"){
      cols <- "gbifID, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% spatial_issues){
      cols <- "gbifID, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% depth_issues){
      cols <- "gbifID, scientificName, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth"
    }else if (input$i %in% elev_issues){
      cols <- "gbifID, scientificName, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation"
    }else if (input$i %in% date_issues){
      cols <- "gbifID, scientificName, eventDate, eventTime, startDayOfYear, endDayOfYear,year, month, day"
    }else{
      cols <- "gbifID, scientificName, recordedBy, locality, country"
    }

    if (input$i == "None"){
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues) and gbifID NOT IN (SELECT gbifID FROM gbif WHERE ignorerow = 1)"))
    }else{
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "') and gbifID NOT IN (SELECT gbifID FROM gbif WHERE ignorerow = 1)"))
    }

    DT::datatable(datarows, escape = FALSE, options = list(searching = TRUE, ordering = TRUE), rownames = FALSE, selection = 'single', isolate({
      datarows()
    }))
  })
  
  
  # Show the values using an HTML table
  
  # output$table = DT::renderDataTable(escape = FALSE, options = list(searching = TRUE, ordering = TRUE), rownames = FALSE, selection = 'single', isolate({
  #   datarows()
  # }))
  
  # observe({
  #   replaceData(proxy, datarows(), resetPaging = FALSE)
  # })
  
  
  
  
  
  
  
  # Record map ----
  output$mymap <- renderLeaflet({
    
    req(input$i)
    
    #if (input$i %in% spatial_issues){
      datarows <- dbGetQuery(gbif_db, paste0("SELECT gbifID, decimalLatitude, decimalLongitude FROM gbif WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "') and ignorerow = 0"))

      points <- datarows[input$table_rows_selected,]
      
      #check if lat and lon exist
      req(points$decimalLongitude)
      req(points$decimalLatitude)
      
      leaflet() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        addMarkers(lng = points$decimalLongitude, lat = points$decimalLatitude) %>%
        setView(points$decimalLongitude, points$decimalLatitude, zoom = 04)
     # }
  })
  
  
  
  # details of record ----
  output$recorddetail <- renderUI({
      
    req(input$table_rows_selected)
    
    if (input$i == "None"){
      this_summary_ids <- dbGetQuery(gbif_db, "SELECT gbifID FROM gbif WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues) and ignorerow = 0")
    }else{
      this_summary_ids <- dbGetQuery(gbif_db, paste0("SELECT gbifID FROM gbif WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "') and ignorerow = 0"))
    }
    
    
    this_record <- dbGetQuery(gbif_db, paste0("SELECT * FROM gbif WHERE gbifID = ", this_summary_ids[input$table_rows_selected,]))
    this_record_dataset <- dbGetQuery(gbif_db, paste0("SELECT * FROM datasets WHERE datasetKey in (SELECT datasetKey FROM gbif WHERE gbifID = ", this_record$gbifID, ")"))
    
    gbif_record_url <- paste0("https://www.gbif.org/occurrence/", this_record$gbifID)
    occurrenceID <- this_record$occurrenceID
    
    html_to_print <- paste0("<h4>Record gbifID: ", this_record$gbifID)
                            
    html_to_print <- paste0(html_to_print, 
                            actionButton("delrecord", 
                            label = "Delete record", 
                            class = "btn btn-danger pull-right",
                            icon = icon("remove", lib = "glyphicon")),
                            br()
        )
                            
    html_to_print <- paste0(html_to_print, "</h4><dl class=\"dl-horizontal\"><dt>GBIF Record</dt><dd><a href=\"", gbif_record_url, "\" target = _blank>", gbif_record_url, "</a></dd>")
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
    html_to_print <- paste0(html_to_print, "<dt>Dataset Title</dt><dd><a href=\"https://www.gbif.org/dataset/", this_record_dataset$datasetKey, "\" target = \"_blank\">", this_record_dataset$title, "</a></dd>")
    html_to_print <- paste0(html_to_print, "<dt>Dataset Institution</dt><dd>", this_record_dataset$institution, "</dd>")
    
    html_to_print <- paste0(html_to_print, "</dl>")
    
    HTML(html_to_print)
    
  })
  
  
  
  # details of record_top ----
  output$recorddetail_top <- renderUI({
    req(input$table_rows_selected)
    HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Record details</h3></div><div class=\"panel-body\">")
  })
  
  # details of record_bot ----
  output$recorddetail_bot <- renderUI({
    req(input$table_rows_selected)
    HTML("</div></div>")
  })
  
 
  
  # Proxy for DT ----
  proxy = dataTableProxy('table')
  # Delete row ----
  observeEvent(input$delrecord, {
    
    req(input$table_rows_selected)
    
    if (input$i == "None"){
      this_summary_ids <- dbGetQuery(gbif_db, "SELECT gbifID FROM gbif WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues)")
    }else{
      this_summary_ids <- dbGetQuery(gbif_db, paste0("SELECT gbifID FROM gbif WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "')"))
    }
    #cat(this_summary_ids)
    this_record <- dbExecute(gbif_db, paste0("UPDATE gbif SET ignorerow = 1 WHERE gbifID = ", this_summary_ids[input$table_rows_selected,]))
    
    if (input$i == "None"){
      cols <- "gbifID, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% spatial_issues){
      cols <- "gbifID, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% depth_issues){
      cols <- "gbifID, scientificName, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth"
    }else if (input$i %in% elev_issues){
      cols <- "gbifID, scientificName, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation"
    }else if (input$i %in% date_issues){
      cols <- "gbifID, scientificName, eventDate, eventTime, startDayOfYear, endDayOfYear,year, month, day"
    }else{
      cols <- "gbifID, scientificName, recordedBy, locality, country"
    }
    
    if (input$i == "None"){
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues) and gbifID NOT IN (SELECT gbifID FROM gbif WHERE ignorerow = 1)"))
    }else{
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "') and gbifID NOT IN (SELECT gbifID FROM gbif WHERE ignorerow = 1)"))
    }
    
    datarows <- data.frame(datarows, stringsAsFactors = FALSE)
    replaceData(proxy, datarows, rownames = FALSE)
    
  })
  
  
  
  
  # Name of issue ----
  output$issuename <- renderText({
    
    req(input$i)
    
    if (input$i != "None"){
      this_issue <- dplyr::filter(gbifissues, issue == input$i)
      paste0("Issue: ", this_issue$issue)
    }else{
      print("Records with no issues")
    }
  })
  
  
  # Count of records ----
  output$no_records <- renderUI({
    
    req(input$i)
    
    if (input$i != "None"){
      this_count <- summary_vals[summary_vals$issue == input$i,]$no_records
    }else{
      this_count <- dbGetQuery(gbif_db, "SELECT count(gbifID) FROM gbif WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues)  and ignorerow = 0")
    }
    if (this_count > 0){
      this_count_pretty <- prettyNum(this_count, big.mark = ",", scientific=FALSE)
      HTML(paste0("<p><strong>", this_count_pretty , " records</strong><br>Click for details</p>"))
    }else{
      HTML(paste0("<p><strong>No records found</strong></p>"))
    }
  })
  
  
  # Print issue description ----
  output$issuedescript <- renderText({
    
    req(input$i)
    
    if (input$i != "None"){
      this_issue <- dplyr::filter(gbifissues, issue == input$i)
      this_issue$description
    }
  })

  
  
  # Downloadable csv of selected issue ----
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(input$i, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dbGetQuery(gbif_db, paste0("SELECT * FROM gbif WHERE gbifID IN (SELECT gbifID from issues WHERE issue = '", input$i, "') and ignorerow = 0")), file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of rows with no issues ----
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("NO_ISSUES.csv", sep = "")
    },
    content = function(file) {
      write.csv(dbGetQuery(gbif_db, "SELECT * FROM gbif WHERE gbifID NOT IN (SELECT DISTINCT gbifID FROM issues) and ignorerow = 0"), file, row.names = FALSE)
    }
  )
  
  # Download button ----
  output$downloadData <- renderUI({
    req(input$i)
    
    if (input$i != "None"){
      downloadButton("downloadData1", "Download records with this issue", class = "btn-primary")  
    }else{
      downloadButton("downloadData2", "Download records with no issues", class = "btn-primary")  
    }
  })

  
  
  
  
  
  # Download Occ file ----
  output$downloadOcc <- downloadHandler(
    filename = function() {
      paste("occurrence.csv", sep = "")
    },
    content = function(file) {
      write.csv(dbGetQuery(gbif_db, "SELECT * FROM gbif WHERE ignorerow = 0"), file, row.names = FALSE)
    }
  )
  
  output$downloadOccFile <- renderUI({
    req(input$i)
    downloadButton("downloadOcc", "Download Occurrence File", class = "btn-primary")
  })
  
  
  output$downloadOccFileInfo <- renderUI({
    req(input$i)
    tagList(
      HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Download Occurrence</h3></div><div class=\"panel-body\">
           <p>To download the occurrence file, in csv format, without the deleted rows:"),
      uiOutput("downloadOccFile"),
      HTML("</div></div>")
      )
  })
  
  
  
  
  # Download Occ file ----
  output$downloadOcc1 <- downloadHandler(
    filename = function() {
      paste("verbatim.csv", sep = "")
    },
    content = function(file) {
      write.csv(dbGetQuery(gbif_db, "SELECT * FROM verbatim WHERE gbifID IN (SELECT gbifID FROM gbif WHERE ignorerow = 0)"), file, row.names = FALSE)
    }
  )
  
  output$downloadOccFile1 <- renderUI({
    req(input$i)
    downloadButton("downloadOcc1", "Download Verbatim File", class = "btn-primary")
  })
  
  
  output$downloadVerFileInfo <- renderUI({
    req(input$i)
    tagList(
      HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Download Verbatim</h3></div><div class=\"panel-body\">
           <p>To download the verbatim file, in csv format, without the deleted rows:"),
      uiOutput("downloadOccFile1"),
      HTML("</div></div>")
    )
  })
  
  
  
  
  # Help1 ----
  output$help1 <- renderUI({
    HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">How to use this tool</h3></div><div class=\"panel-body\">
         <p>This system will take the string in the column \"Taxonomy\" and match it with the Taxonomy from EPICC. The process tries to find a match taking into account the variety of ways that a scientific name can appear. 
         <p>The match is done by first removing words and characters like:
         <ul>
         <li>?</li>
         <li>af.</li>
         <li>cf.</li>
         <li>spp.</li>
         <li>n. sp.</li>
         </ul>
         <p>Then, the system tries to match the string by looking at possible ways to match:
         <ul>
         <li>Genus</li>              
         <li>Genus (Subgenus)</li>
         <li>Genus species</li>
         <li>Genus species Author</li>
         <li>Genus (Subgenus) species</li>
         <li>Genus (Subgenus) species Author</li>
         <li>Synonym</li>
         </ul>
         <p>To match another CSV you don't need to reload the taxonomy file. Just browse for a new CSV, the taxonomy is already in memory.</p>
         </div></div>")
  })
  
  # Help2 ----
  output$help2 <- renderUI({
    HTML("
         <div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">GBIF Download Key</h3></div><div class=\"panel-body\">
        <p>The key is obtained when requesting a download of occurrence data from GBIF. :
          <dl class=\"dl-horizontal\">
              <dt>sciname</dt><dd>The string used to find a match</dd>
              <dt>accepted_name</dt><dd>Concatenated (Genus Species) from the taxonomy file</d>
              <dt>synonym</dt><dd>The synonym matched to the sciname</dd>
              <dt>fuzzy_match</dt><dd>Name matched to using a fuzzy matching algorithm (score in parenthesis) *</d>
              <dt>phylum</dt><dd>The Phylum of the accepted_name from the taxonomy file</dd>
              <dt>class</dt><dd>The Class of the accepted_name from the taxonomy file</dd>
              <dt>order</dt><dd>The Order of the accepted_name from the taxonomy file</dd>
              <dt>family</dt><dd>The Family of the accepted_name from the taxonomy file</dd>
              <dt>genus</dt><dd>The Genus of the accepted_name from the taxonomy file</dd>
              <dt>subgenus</dt><dd>The Subgenus of the accepted_name from the taxonomy file</dd>
              <dt>species</dt><dd>The Species of the accepted_name from the taxonomy file</dd>
              <dt>author</dt><dd>The \"AUTHOR\" of the accepted_name from the taxonomy file</dd>
         </dl>
        <p>If the Taxonomy column had more than one name, separated by pipes (|), each name is returned in a separate row.</p>
        <p>* Fuzzy matching uses the \"osa\" method in the function stringdist::stringdist(), for details see van der Loo (2014).</p>
        <pre>van der Loo, Mark (2014). The stringdist Package for Approximate String Matching</a>. The R Journal 6: 111-122.</pre>
         </div></div>")
  })
  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server, onStart = function() {
  cat("Loading\n")
  #Mount path
  onStop(function() {
    dbDisconnect(gbif_db)
    if (persistent_db == FALSE){
      dbDisconnect(gbif_db)
      Sys.sleep(1)
      try(unlink("data", recursive = TRUE), silent = TRUE)
    }
    cat("Removing objects\n")
    rm(list = ls())
  })
})


