# Load/install packages ----
library("shiny")
library("DT")
library("dplyr")
library("ggplot2")
library("stringr")
library("leaflet")
library("XML")
library("curl")
library("data.table")
library("DBI")
library("RSQLite")
library("jsonlite")
library("R.utils")
library("shinyWidgets")
library("shinycssloaders")




#Import settings----
source("settings.R")


# Import definitions, cols, functions ----
source("functions.R")




# System Settings ----
app_ver <- "0.4.0"
github_link <- "https://github.com/Smithsonian/GBIF-Issues-Explorer"

occ_file <- "data/occurrence.txt"
ver_file <- "data/verbatim.txt"
multi_file <- "data/multimedia.txt"

#how many rows to import at a time?
no_rows <- 20000





# UI ----
ui <- fluidPage(
  navbarPage(app_name,
     # Tab:Summary ----
     tabPanel("Summary", 
        br(),
        fluidRow(
          column(width = 4,
                 uiOutput("ask_key")
          ),
          column(width = 8,
                 uiOutput("messages")
          )
        ),
        fluidRow(
          column(width = 6,
                 br(),
                 plotOutput("summaryPlot3", height = 600),
                 br(),
                 plotOutput("summaryPlot2", height = 600)
          ),
          column(width = 6,
                 br(),
                 withSpinner(DT::dataTableOutput("summaryTable")),
                 br(),
                 uiOutput("download_doi")
                 )
        )#,
        # fluidRow(
        #   column(width = 6,
        #          br(),
        #          plotOutput("summaryPlot2", height = 600)
        #          ),
        #   column(width = 6,
        #          br(),
        #          plotOutput("summaryPlot3", height = 600)
        #          )
        # )
        ),
     
     # Tab:Explore ----
      tabPanel("Explore Issues", 
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
                     fluidRow(
                       column(width=8,                
                           HTML("<dl><dt>"),
                           strong(textOutput("issuename")),
                           HTML("</dt><dd>"),
                           textOutput("issuedescript"),
                           HTML("</dd></dl>")
                     ),
                      column(width=4,
                            uiOutput("clickdetails")
                            #HTML("<p class=\"pull-right\">Click a record for details</p>")
                        )
                     ),
                     withSpinner(DT::dataTableOutput("table"))
               ),
               column(width=5, 
                      conditionalPanel("input.table_rows_selected != null && input.table_rows_selected != ''",
                        shinyWidgets::panel(
                              heading = "Record detail",
                              status = "primary",
                              uiOutput("recorddetail"),
                              leafletOutput("mymap")
                           )
                      )
               )
          )
      ),
     # Tab:DataFields ----
     tabPanel("Explore Data Fields", 
              br(),
              fluidRow(
                column(width = 6, 
                       uiOutput("explore_fields"),
                       withSpinner(DT::dataTableOutput("fields_table"))
                ),
                column(width = 6, 
                       uiOutput("fields_details_h"),
                       DT::dataTableOutput("fields_details"),
                       uiOutput("precision_note")
                )
              )
              
     ),
     # Tab:Help ----
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
  #footer ----
  HTML(paste0("<br><br><br><div class=\"footer navbar-fixed-bottom\" style=\"background: #FFFFFF;\"><br><p>&nbsp;&nbsp;<a href=\"http://dpo.si.edu\" target = _blank><img src=\"dpologo.jpg\"></a> | ", app_name, ", ver. ", app_ver, " | <a href=\"", github_link, "\" target = _blank>Source code</a></p></div>"))
)





# Server ----
server <- function(input, output, session) {
  
  dir.create("data", showWarnings = FALSE)
  
  if (!exists("persistent_db")){
    persistent_db <- FALSE
  }
  
  if (persistent_db){
    database_file <- "data/gbif.sqlite3"
  }else{
    database_file <- paste0("data/gbif_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite3")
  }
  
  # ask for key ----
  output$ask_key <- renderUI({
    if (!file.exists(database_file)){
        tagList(
          h3("Explore the issues that GBIF has identified in the data in a DwC download."),
          textInput(inputId = "gbif_key", label = "GBIF Download Key:", value = "0046569-180508205500799"),
          actionButton("submitkey", 
                       label = "Submit", 
                       class = "btn btn-primary",
                       icon = icon("triangle-right", lib = "glyphicon"))
        )
     }
    })
  
  
  # Submit button ----
  observeEvent(input$submitkey, {
    
    # Downloading GBIF file ---- 
    #add progress by using https://stackoverflow.com/a/42632792?
    gbif_check <- check_gbif(input$gbif_key)
    if (class(gbif_check) == "list"){
      dl_size <- gbif_check$size
      dl_size_human <- hsize(dl_size)
    }else{
      #Messages ----
      output$messages <- renderUI({
        res <- try(jsonlite::fromJSON(paste0("http://api.gbif.org/v1/occurrence/download/", input$gbif_key)), silent = TRUE)
        HTML(paste0("<p class=\"alert alert-danger\" role=\"alert\">Error: Could not find a download with that key</p>"))
      })
      req(FALSE)
    }
    
    
    #Check if DwC ----
    if (!check_gbif_dw(input$gbif_key)){
      #Messages ----
      output$messages <- renderUI({
        HTML(paste0("<p class=\"alert alert-danger\" role=\"alert\">Error: Download is not Darwin Core Archive.</p>"))
      })
      req(FALSE)
    }
    
    progress0 <- shiny::Progress$new()
    progress0$set(message = paste0("Downloading file of size: ", dl_size_human, ". Please wait."), value = 0.1)
    on.exit(progress0$close())
    
    gbif_metadata <- download_gbif(input$gbif_key, "data")
    if (class(gbif_metadata) == "list"){
      gbif_metadata <<- gbif_metadata
    }else{
      #Messages ----
      output$messages <- renderUI({
        HTML(paste0("<p class=\"alert alert-danger\" role=\"alert\">Error: Could not download the file.</p>"))
      })
      req(FALSE)
    }
    
    progress0$set(message = "Downloading file", value = 1)
    progress0$close()
    
    if (file.exists("data/occurrence.txt")){
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Loading data", value = 0.1)
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
        
        #Check if cols changed
        gbif_check <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1, skip = 1)
        if (dim(gbif_check)[2] == 235){
          db_created <- try(create_database(database_file, "data/dataset/"))
        }else if(dim(gbif_check)[2] == 237){
          db_created <- try(create_database(database_file, "data/dataset/"))
        }else{
          stop("Could not create database.")
        }
        
        if (class(db_created) == "try-error"){
          progress0$set(message = "Could not create database.", detail = "System needs to be updated because the data does not match", value = 0.99)
          stop("Could not create database.")
        }
        
        gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
        session$userData$gbif_db <- gbif_db
        
        progress0$set(value = 1, message = "Creating database")
        progress0$close()
        
        progress1$set(value = 0.1, message = "Reading occurrence records", detail = long_loading_msg)
        progress2$set(value = 0.1, message = "Reading verbatim records", detail = long_loading_msg)
        
        #how big?
        no_lines <- R.utils::countLines(occ_file)[1]
        
        #how many steps?
        no_steps <- floor(no_lines/no_rows)
        progress_val <- 0.12
        progress_steps <- ((0.9 - progress_val) / no_steps)
        
        if (no_steps ==0){
          no_steps <- 1
        }
        
        #loop to occ_file
        for (i in 1:no_steps){
          cat(paste0("Step loading db: ", i, "\n"))
          if (i == 1){
            gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1)
            verbatim_data <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1)
          }else{
            skip_rows <- i * no_rows
            gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows)
            verbatim_data <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows)
          }
          
          #Set names to df  
          gbif_cols <- data.table::fread(input = "data/occurrence.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1)
          
          if (dim(gbif_data)[2] == 238){
            gbif_data <- cbind(gbif_data, NA)
          }
          
          names(gbif_data) <- tolower(unlist(gbif_cols))
          
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
        progress1$close()
        
        progress$set(message = "Loading data", value = 0.4)
        
        
        
        #multimedia file ----
        progress3 <- shiny::Progress$new()
        on.exit(progress3$close())
        progress3$set(value = 0.1, message = "Reading multimedia records", detail = long_loading_msg)
        
        no_lines <- R.utils::countLines(multi_file)[1]
        
        if (no_lines > 1){
          #how many steps?
          no_steps <- floor(no_lines/no_rows)
          progress_val <- 0.12
          progress_steps <- ((0.9 - progress_val) / no_steps)
          
          if (no_steps == 0){
            no_steps <- 1
          }
          
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
        
        progress$set(message = "Loading data", value = 0.7)
        
        
        
        
        
        # Statistics of issues ----
        progress$set(value = 0.8, message = "Generating statistics")
        
        issue_list <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM gbif WHERE issue != ''")
        
        issues_list <- data.frame(matrix(ncol = 1, nrow = 0, data = NA))
        for (i in 1:dim(issue_list)[1]){
          a <- strsplit(issue_list[i,1], ";")
          for (j in 1:length(a[[1]])){
            issues_list <- c(issues_list, a[[1]][j])
          }
        }
        
        distinct_issues <<- unique(unlist(issues_list))

        #summary 
        for (i in 1:length(distinct_issues)){
          dbExecute(gbif_db, paste0("INSERT INTO issues (gbifid, issue) SELECT gbifid, '", distinct_issues[i], "' FROM gbif WHERE issue LIKE '%", distinct_issues[i], "%'"))
        }
        
        progress$set(message = "Loading data", value = 0.95)
        
        #Delete files no longer needed
        try(unlink("data/dataset", recursive = TRUE), silent = TRUE)
        try(unlink("data/*.txt", recursive = TRUE), silent = TRUE)
        try(unlink("data/meta.xml", recursive = TRUE), silent = TRUE)
        
        progress$set(message = "Done loading data... Loading app.", value = 1.0)
        progress$close()
        
        # Close db ----
        dbDisconnect(gbif_db)
        rm(gbif_db)
      }
      
      # Open database ----
      gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
      session$userData$gbif_db <- gbif_db
      
      # Metadata of Download ----
      eval(parse("dlmeta.R"))
      
      # Get rows with issues ----
      distinct_issues <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM issues")
      distinct_issues <- unlist(distinct_issues, use.names = FALSE)
      
      #summary_vals ---- 
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
    
    
    
    #distinct_issues pulldown ----
    output$distinct_issues <- renderUI({
      selectInput(inputId = "i",
                  label = "Select an issue:", 
                  choices = distinct_issues,
                  width = 360
      )
    })
    
    # Summary ----
    eval(parse("summary.R"))
    
    
    eval(parse("fieldssummary.R"))
    
    
  })
  
  
  
  # Persistent ----
  if (persistent_db && file.exists(database_file)){
    
    gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
    session$userData$gbif_db <- gbif_db
    
    # Metadata of Download ----
    eval(parse("dlmeta.R"))
    
    # Get rows with issues
    distinct_issues <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM issues")
    distinct_issues <- unlist(distinct_issues, use.names = FALSE)
    session$userData$distinct_issues <- distinct_issues
    
    #distinct_issues pulldown ----
    output$distinct_issues <- renderUI({
      selectInput(inputId = "i",
                  label = "Select an issue:", 
                  choices = distinct_issues,
                  width = 360
      )
    })
    
    # Summary ----
    eval(parse("summary.R"))
  }
  
  eval(parse("fieldssummary.R"))
  
  # Table of records with issue ----
  datarows <- reactive({
    gbif_db <- session$userData$gbif_db
    
    req(input$i)
    cat(input$i)
    #Which cols to display by type of issue
    if (input$i %in% spatial_issues){
      cols <- "gbifid, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% depth_issues){
      cols <- "gbifid, scientificName, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth"
    }else if (input$i %in% elev_issues){
      cols <- "gbifid, scientificName, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation"
    }else if (input$i %in% date_issues){
      cols <- "gbifid, scientificName, eventDate, eventTime, startDayOfYear, endDayOfYear,year, month, day"
    }else if (input$i %in% taxo_issues){
      cols <- "gbifid, scientificName, kingdom, phylum, class, \"order\", family, genus, higherclassification"
    }else if (input$i == "BASIS_OF_RECORD_INVALID"){
      cols <- "gbifid, scientificName, basisOfRecord, recordedBy, locality, country"
    }else if (input$i == "REFERENCES_URI_INVALID"){
      cols <- "gbifid, scientificName, \"references\", recordedBy, locality, country"
    }else if (input$i == "INDIVIDUAL_COUNT_INVALID"){
      cols <- "gbifid, scientificName, individualcount, recordedBy, locality, country"
    }else if (input$i == "TYPE_STATUS_INVALID"){
      cols <- "gbifid, scientificName, typestatus, recordedBy, locality, country"
    }else{
      cols <- "gbifid, scientificName, recordedBy, locality, country"
    }
    
    
    
    
    query <- paste0("SELECT ", cols, " FROM verbatim WHERE gbifid IN (SELECT gbifid from issues WHERE issue = '", input$i, "') and gbifid NOT IN (SELECT gbifid FROM gbif WHERE ignorerow = 1)")
    print(query)
    datarows <- dbGetQuery(gbif_db, query)

    df <- data.frame(datarows, stringsAsFactors = FALSE)
    
    df
  })

  output$table <- DT::renderDataTable({
    req(input$i)
    DT::datatable(datarows(), escape = FALSE, options = list(searching = TRUE, ordering = TRUE, pageLength = 10), rownames = FALSE, selection = 'single')
  }, server = TRUE)
  
  
  
  
  # details of record ----
  output$recorddetail <- renderUI({
    req(input$table_rows_selected)
    
    gbif_db <- session$userData$gbif_db
    
    if (input$i == "None"){
      this_summary_ids <- dbGetQuery(gbif_db, "SELECT gbifid FROM gbif WHERE gbifid NOT IN (SELECT DISTINCT gbifid FROM issues) and ignorerow = 0")
    }else{
      this_summary_ids <- dbGetQuery(gbif_db, paste0("SELECT gbifid FROM gbif WHERE gbifid IN (SELECT gbifid from issues WHERE issue = '", input$i, "') and ignorerow = 0"))
    }
    
    this_record <- dbGetQuery(gbif_db, paste0("SELECT * FROM gbif WHERE gbifid = ", this_summary_ids[input$table_rows_selected,]))
    this_record_dataset <- dbGetQuery(gbif_db, paste0("SELECT * FROM datasets WHERE datasetKey in (SELECT datasetKey FROM gbif WHERE gbifid = ", this_record$gbifid, ")"))
    
    gbif_record_url <- paste0("https://www.gbif.org/occurrence/", this_record$gbifid)
    occurrenceID <- this_record$occurrenceid
    
    html_to_print <- paste0("<h4>Record gbifid: ", this_record$gbifid)
                            
    html_to_print <- paste0(html_to_print, 
                            actionButton("delrecord", 
                            label = "Hide record", 
                            class = "btn btn-danger pull-right",
                            icon = icon("remove", lib = "glyphicon")),
                            br()
        )
                            
    html_to_print <- paste0(html_to_print, "</h4><dl class=\"dl-horizontal\"><dt>GBIF Record</dt><dd><a href=\"", gbif_record_url, "\" target = _blank>", gbif_record_url, "</a></dd>")
    if (!is.na(occurrenceID)){
      html_to_print <- paste0(html_to_print, "<dt>Occurrence ID</dt>")
      #check if it is a URL, just print otherwise
      if (substr(occurrenceID, 0, 4) == "http"){
        html_to_print <- paste0(html_to_print, "<dd><a href=\"", occurrenceID, "\" target = _blank>", occurrenceID, "</a></dd>")
      }else{
        html_to_print <- paste0(html_to_print, "<dd>", occurrenceID, "</dd>")
      }
    }
    
    if (this_record$catalognumber != ""){
      html_to_print <- paste0(html_to_print, "<dt>Catalog No.</dt><dd>", this_record$catalognumber, "</dd>")
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
    images <- dbGetQuery(gbif_db, paste0("SELECT * FROM multimedia WHERE gbifid = ", this_record$gbifid, " AND type = 'StillImage'"))
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
    
    verbatim_json <- paste0("http://api.gbif.org/v1/occurrence/", this_record$gbifid, "/verbatim")
    
    html_to_print <- paste0(html_to_print, "<a href=\"", verbatim_json, "\" target = _blank>Verbatim JSON</a><br>")
    
    HTML(html_to_print)
  })
  

  # Proxy for DT ----
  proxy = dataTableProxy('table')
  # Delete row ----
  observeEvent(input$delrecord, {
    req(input$table_rows_selected)
    
    if (input$i == "None"){
      this_summary_ids <- dbGetQuery(gbif_db, "SELECT gbifid FROM gbif WHERE gbifid NOT IN (SELECT DISTINCT gbifid FROM issues) AND ignorerow = 0")
    }else{
      this_summary_ids <- dbGetQuery(gbif_db, paste0("SELECT gbifid FROM gbif WHERE gbifid IN (SELECT gbifid from issues WHERE issue = '", input$i, "') AND ignorerow = 0"))
    }
    #cat(this_summary_ids)
    this_record <- dbExecute(gbif_db, paste0("UPDATE gbif SET ignorerow = 1 WHERE gbifid = ", this_summary_ids[input$table_rows_selected,]))
    
    if (input$i == "None"){
      cols <- "gbifid, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% spatial_issues){
      cols <- "gbifid, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% depth_issues){
      cols <- "gbifid, scientificName, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth"
    }else if (input$i %in% elev_issues){
      cols <- "gbifid, scientificName, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation"
    }else if (input$i %in% date_issues){
      cols <- "gbifid, scientificName, eventDate, eventTime, startDayOfYear, endDayOfYear,year, month, day"
    }else{
      cols <- "gbifid, scientificName, recordedBy, locality, country"
    }
    
    if (input$i == "None"){
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifid NOT IN (SELECT DISTINCT gbifid FROM issues) and gbifid NOT IN (SELECT gbifid FROM gbif WHERE ignorerow = 1)"))
    }else{
      datarows <- dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM verbatim WHERE gbifid IN (SELECT gbifid from issues WHERE issue = '", input$i, "') and gbifid NOT IN (SELECT gbifid FROM gbif WHERE ignorerow = 1)"))
    }
    
    datarows <- data.frame(datarows, stringsAsFactors = FALSE)
    replaceData(proxy, datarows, rownames = FALSE)
  })
  
  
  
  # Record map ----
  output$mymap <- renderLeaflet({
    req(input$i)
    
    gbif_db <- session$userData$gbif_db
    
    datarows <- dbGetQuery(gbif_db, paste0("SELECT gbifid, decimalLatitude, decimalLongitude FROM gbif WHERE gbifid IN (SELECT gbifid from issues WHERE issue = '", input$i, "') and ignorerow = 0"))
    points <- datarows[input$table_rows_selected,]
    
    #check if lat and lon exist
    req(points$decimalLongitude)
    req(points$decimalLatitude)
    
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addMarkers(lng = points$decimalLongitude, lat = points$decimalLatitude) %>%
      setView(points$decimalLongitude, points$decimalLatitude, zoom = 04)
  })
  
  
  
  
  
  # Name of issue ----
  output$issuename <- renderText({
    req(input$i)
    this_issue <- dplyr::filter(gbifissues, issue == input$i)
    paste0("Issue: ", this_issue$issue)
  })
  
  
  # Print issue description ----
  output$issuedescript <- renderText({
    req(input$i)
    
    if (input$i != "None"){
      this_issue <- dplyr::filter(gbifissues, issue == input$i)
      paste0("Description: ", this_issue$description)
    }
  })

  
  
  # Downloadable csv of selected issue ----
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(input$i, ".csv", sep = "")
    },
    content = function(file) {
      
      gbif_db <- session$userData$gbif_db
      
      write.csv(dbGetQuery(gbif_db, paste0("SELECT * FROM gbif WHERE gbifid IN (SELECT gbifid from issues WHERE issue = '", input$i, "') and ignorerow = 0")), file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of rows with no issues ----
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("NO_ISSUES.csv", sep = "")
    },
    content = function(file) {
      
      gbif_db <- session$userData$gbif_db
      
      write.csv(dbGetQuery(gbif_db, "SELECT * FROM gbif WHERE gbifid NOT IN (SELECT DISTINCT gbifid FROM issues) and ignorerow = 0"), file, row.names = FALSE)
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
      gbif_db <- session$userData$gbif_db
      
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
           <p>To download the occurrence file, in csv format, without the hidden rows:"),
      uiOutput("downloadOccFile"),
      HTML("</div></div>")
      )
  })
  
  
  
  
  # Download Verbatim file ----
  output$downloadOcc1 <- downloadHandler(
    filename = function() {
      paste("verbatim.csv", sep = "")
    },
    content = function(file) {
      gbif_db <- session$userData$gbif_db
      
      write.csv(dbGetQuery(gbif_db, "SELECT * FROM verbatim WHERE gbifid IN (SELECT gbifid FROM gbif WHERE ignorerow = 0)"), file, row.names = FALSE)
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
           <p>To download the verbatim file, in csv format, without the hidden rows:"),
      uiOutput("downloadOccFile1"),
      HTML("</div></div>")
    )
  })
  
  
  
  # Help1 ----
  output$help1 <- renderUI({
    HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">How to use this tool</h3></div><div class=\"panel-body\">
         <p>Occurrence records in GBIF can be tagged with a number of issues that their system has detected. However, like the 
         <a href=\"https://www.gbif.org/article/5i3CQEZ6DuWiycgMaaakCo/gbif-infrastructure-data-processing\" target = _blank>
         processing information page</a> indicates:</p>
         <pre>Not all issues indicate bad data. Some are merley flagging the fact that GBIF has altered values during processing.</pre>
         <p>This tool allows collection and data managers, as well as researchers, to explore issues in GBIF Darwin Core Archive downloads in an easy web-based interface. Just enter a GBIF download key and the tool will download the zip file, create a local database, and display the issues in the data contained. Once provided with the GBIF key, this tool will:</p>
         <ul>
         <li>Download the zip archive</li>
         <li>Extract the files</li>
         <li>Create a local database</li>
         <li>Load the data from the occurrence, verbatim, multimedia, and dataset tables to the database</li>
         <li>Generate summary statistics of the issues</li>
         </ul>
         <p>Then, you can click on the 'Explore' tab to see how many records have been tagged with a particular issue.
         <p>Once you select an issue, a table will display the rows that have been tagged with that issue. If you click on a row, more details of the occurrence record will be shown, including a map if the records has coordinates. You can choose to delete the row from the local database.</p>
         </div></div>")
  })
  
  # Help2 ----
  output$help2 <- renderUI({
    HTML("
         <div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">GBIF Download Key</h3></div><div class=\"panel-body\">
        <p>The key is obtained when requesting a download of occurrence data from GBIF. It can be requested via their API or on the website. If your download URL is:</p>
          <pre>https://www.gbif.org/occurrence/download/0001419-180824113759888</pre>
          <p>Then, the last part, '0001419-180824113759888' is the GBIF key you will need to provide this tool.</p>
          <p>This tool works only with Darwin Core Archives, not with CSV archives.</p>
         </div></div>
         
         <div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Deleting Records</h3></div><div class=\"panel-body\">
        <p>You can delete individual records from the local database by clicking on the 'Delete record' button.</p>
        <p>Deleted records will not show up in the table of results or when downloading the Occurrence or Verbatim files. This option can be used to remove records, for example, where:</p>
         <ul>
            <li>The issue is not a real problem and can be ignored</li>
            <li>The issue has been fixed in the collection database</li>
            <li>For researchers, the record can not be used in an analysis</li>
         </ul>
         </div></div>")
  })
  
  
  output$clickdetails <- renderUI({
    req(input$i)
    HTML("<p class=\"pull-right\">Click a record for details</p>")
  })
  
}


# Run Shiny app ----
shinyApp(ui, server)
