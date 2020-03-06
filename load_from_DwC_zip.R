
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
  
  unzip(paste0('data/', zipfile), exdir = 'data')
  
  #Check if cols changed
  gbif_check <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1)
  
  db_created <- try(create_database(database_file, "data/dataset/"))
  
}



#how big?
no_lines <- R.utils::countLines(occ_file)[1]

#how many steps?
no_steps <- floor(no_lines/no_rows)

if (no_steps ==0){
  no_steps <- 1
}

gbif_db <- dbConnect(RSQLite::SQLite(), database_file)

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
  gbif_cols <- data.table::fread(input = "data/occurrence.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1)
  
  if (dim(gbif_data)[2] == 238){
    gbif_data <- cbind(gbif_data, NA)
  }
  
  names(gbif_data) <- tolower(unlist(gbif_cols))
  
  names(verbatim_data) <- verbatim_cols
  
  #write rows
  dbWriteTable(gbif_db, "gbif", gbif_data, append = TRUE)
  dbWriteTable(gbif_db, "verbatim", verbatim_data, append = TRUE)
}

rm(gbif_data)
rm(verbatim_data)



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
  dbExecute(gbif_db, paste0("INSERT INTO issues (gbifID, issue) SELECT gbifID, '", distinct_issues[i], "' FROM gbif WHERE issue LIKE '%", distinct_issues[i], "%'"))
}



#indices
print("Calculating field statistics")

fields <- dbGetQuery(gbif_db, "PRAGMA table_info(gbif)")

fields <- dplyr::filter(fields, name != 'ignorerow')
fields <- dplyr::filter(fields, name != 'gbifid')

for (f in seq(225, dim(fields)[1])){
  
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
