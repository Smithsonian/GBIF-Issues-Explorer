
#Load DwC zip file (in "data" folder)

library("DT")
library("dplyr")
library("stringr")
library("XML")
library("data.table")
library("RSQLite")
library("R.utils")

source("functions.R")

database_file <- "data/gbif.sqlite3"

occ_file <- "data/occurrence.txt"
ver_file <- "data/verbatim.txt"
multi_file <- "data/multimedia.txt"

#how many rows to import at a time?
no_rows <- 20000


#extract zip in data
zipfile <- list.files('data', '*.zip')

if (length(zipfile) == 0){
  stop("Could not find zip file in data")
}else if (length(zipfile) > 1){
  stop("There are more than 1 zipfiles in data")
}else{
  
  #unzip has issues with large files
  #unzip(paste0('data/', zipfile), exdir = 'data')
  system2("unzip", args = c("-d", "data", paste0('data/', zipfile)))
  
  #Check if cols changed
  gbif_check <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1)
  
  print("Creating database file...")
  
  #create_database <- function(database_file, dataset_xml_path){
    
    dataset_xml_path <- "data/dataset/"
    
    if (file.exists(database_file)){
      try(unlink(database_file), silent = TRUE)
    }
    gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
    
    #index of issues ----
    n <- dbExecute(gbif_db, create_issuetable_query)
    
    #Get cols
    gbif_cols <- data.table::fread(input = "data/occurrence.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1)
    
    gbif_cols_q <- paste0(gbif_cols, collapse = ", ")
    
    gbif_cols_q <- tolower(stringr::str_replace(gbif_cols_q, "gbifID", "gbifid INTEGER PRIMARY KEY"))
    
    gbif_cols_q <- paste0(gbif_cols_q, ", ignorerow BOOLEAN DEFAULT 0")
    
    #Replace sql keywords
    gbif_cols_q <- stringr::str_replace(gbif_cols_q, fixed(", group"), ", \"group\"")
    gbif_cols_q <- stringr::str_replace(gbif_cols_q, "island\"group\"", "islandgroup")
    gbif_cols_q <- stringr::str_replace(gbif_cols_q, "order", "\"order\"")
    gbif_cols_q <- stringr::str_replace(gbif_cols_q, "references", "\"references\"")
    
    n <- dbExecute(gbif_db, paste0("CREATE TABLE gbif(", gbif_cols_q, ")"))
    
    #verbatim table ----
    n <- dbExecute(gbif_db, verbatim_createtable_query)
    
    #multimedia ----
    n <- dbExecute(gbif_db, create_multimedia_query)
    
    #datasets ----
    datasets_xml <- list.files(dataset_xml_path, pattern = "*.xml", full.names = TRUE)
    no_datasets <- length(datasets_xml)
    
    create_datasettable_query <- 'CREATE TABLE datasets(datasetKey PRIMARY KEY, title, institution);'
    
    n <- dbExecute(gbif_db, create_datasettable_query)
    n <- dbExecute(gbif_db, 'CREATE INDEX ds_datasetKey ON datasets(datasetKey);')
    
    for (i in 1:no_datasets){
      #cat(paste0(i, "\n"))
      meta_file <- xmlToList(datasets_xml[i])
      datasetKey <- str_replace(basename(datasets_xml[i]), ".xml", "")
      datasetTitle <- stringr::str_replace_all(meta_file$dataset$title, "'", "''")
      datasetInst <- stringr::str_replace_all(meta_file$dataset$creator$organizationName, "'", "''")
      insert_query <- paste0("INSERT INTO datasets (datasetKey, title, institution) VALUES ('", datasetKey, "', '", datasetTitle, "', '", datasetInst, "');")
      #cat(insert_query)
      n <- dbExecute(gbif_db, insert_query)
    }
    # Close db ----
    dbDisconnect(gbif_db)
  #}
  #db_created <- try(create_database(database_file, "data/dataset/"))
  
}



#how big?
no_lines <- R.utils::countLines(occ_file)[1]

#how many steps?
no_steps <- floor(no_lines/no_rows)

if (no_steps ==0){
  no_steps <- 1
}

gbif_db <- dbConnect(RSQLite::SQLite(), database_file)

#Set names to df  
gbif_cols <- data.table::fread(input = "data/occurrence.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1)

#loop to occ_file
for (i in 1:no_steps){
  
  print(paste0("Loading data... (", i, " of ", no_steps, " steps)"))
  
  if (i == 1){
    gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1)
    verbatim_data <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1)
  }else{
    skip_rows <- i * no_rows
    gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows)
    verbatim_data <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows)
  }

  if (dim(gbif_data)[2] == 238){
    gbif_data <- cbind(gbif_data, NA)
  }
  
  names(gbif_data) <- tolower(unlist(gbif_cols))
  
  names(verbatim_data) <- verbatim_cols
  
  #write rows
  dbWriteTable(gbif_db, "gbif", gbif_data, append = TRUE)
  dbWriteTable(gbif_db, "verbatim", verbatim_data, append = TRUE)
}


n <- dbExecute(gbif_db, 'CREATE INDEX issue_issue ON issues(issue);')
n <- dbExecute(gbif_db, 'CREATE INDEX gbifID_issue ON issues(gbifID);')
n <- dbExecute(gbif_db, 'CREATE INDEX gbifID ON gbif(gbifID);')
n <- dbExecute(gbif_db, 'CREATE INDEX issue ON gbif(issue);')
n <- dbExecute(gbif_db, 'CREATE INDEX gb_datasetKey ON gbif(datasetID);')
n <- dbExecute(gbif_db, 'CREATE INDEX basisOfRecord ON gbif(basisOfRecord);')
n <- dbExecute(gbif_db, 'CREATE INDEX scientificName ON gbif(scientificName);')
n <- dbExecute(gbif_db, 'CREATE INDEX ignorerow ON gbif(ignorerow);')
n <- dbExecute(gbif_db, 'CREATE INDEX verbatim_gbifID ON verbatim(gbifID);')
n <- dbExecute(gbif_db, 'CREATE INDEX multimedia_gbifID ON multimedia(gbifID);')


rm(gbif_data)
rm(verbatim_data)

print("Getting issues...")

issue_list <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM gbif WHERE issue != ''")

issues_list <- data.frame(matrix(ncol = 1, nrow = 0, data = NA))
for (i in 1:dim(issue_list)[1]){
  a <- strsplit(issue_list[i,1], ";")
  for (j in 1:length(a[[1]])){
    issues_list <- c(issues_list, a[[1]][j])
  }
}

distinct_issues <- unique(unlist(issues_list))

#summary 
for (i in 1:length(distinct_issues)){
  print(paste0("Generating summary of issues... (", i, " of ", length(distinct_issues), ")"))
  dbExecute(gbif_db, paste0("INSERT INTO issues (gbifID, issue) SELECT gbifID, '", distinct_issues[i], "' FROM gbif WHERE issue LIKE '%", distinct_issues[i], "%'"))
}



#indices
print("Calculating field statistics...")

fields <- dbGetQuery(gbif_db, "PRAGMA table_info(gbif)")

fields <- dplyr::filter(fields, name != 'ignorerow')
fields <- dplyr::filter(fields, name != 'gbifid')

for (f in seq(1, dim(fields)[1])){
  
  print(paste0("Calculating field statistics... (", f, " of ", dim(fields)[1], ")"))
  
  this_field <- stringr::str_replace(fields$name[f], fixed("group"), "\"group\"")
  this_field <- stringr::str_replace(this_field, "island\"group\"", "islandgroup")
  this_field <- stringr::str_replace(this_field, "order", "\"order\"")
  this_field <- stringr::str_replace(this_field, fixed("references"), "\"references\"")
  this_field <- stringr::str_replace(this_field, fixed("associated\"references\""), "associatedreferences")
  this_field <- stringr::str_replace(this_field, fixed("geo\"references\"ources"), "georeferencesources")
  this_field <- stringr::str_replace(this_field, fixed("identification\"references\""), "identificationreferences")
  this_field <- stringr::str_replace(this_field, fixed("\"order\"key"), "orderkey")
  
  n <- dbSendQuery(gbif_db, paste0("CREATE INDEX IF NOT EXISTS gbif_", fields$name[f], " ON gbif(", this_field, ")"))
  dbClearResult(n)
}

# Close db ----
dbDisconnect(gbif_db)
