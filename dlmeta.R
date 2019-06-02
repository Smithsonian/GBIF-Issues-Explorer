# Metadata of Download ----
dl_meta_file <- XML::xmlToList("data/metadata.xml")
output$download_doi <- renderText({
  this_doi <- dl_meta_file$additionalMetadata$metadata$`gbif`$citation$.attrs
  gbif_key <- dl_meta_file$dataset$alternateIdentifier
  metadata_json <- paste0("http://api.gbif.org/v1/occurrence/download/", gbif_key)
  gbif_metadata <- unlist(jsonlite::fromJSON(metadata_json))
  
  html_to_print <- paste0("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">GBIF Occurrence Download Metadata</h3></div><div class=\"panel-body\"><div style = \"overflow-y: auto; overflow-x: auto;\"><dl>")
  
  for (i in 1:length(gbif_metadata)){
    html_to_print <- paste0(html_to_print, "<dt>", names(gbif_metadata[i]), "</dt>")
    if (names(gbif_metadata[i]) == "doi"){
      html_to_print <- paste0(html_to_print, "<dd><a href=\"https://doi.org/", gbif_metadata[i], "\" target = _blank>", gbif_metadata[i], "</a></dd>")
    }else if(names(gbif_metadata[i]) == "downloadLink" || names(gbif_metadata[i]) == "license"){
      html_to_print <- paste0(html_to_print, "<dd><a href=\"", gbif_metadata[i], "\" target = _blank>", gbif_metadata[i], "</a></dd>")
    }else if(names(gbif_metadata[i]) == "size"){
      html_to_print <- paste0(html_to_print, "<dd>", utils:::format.object_size(as.numeric(gbif_metadata[i]), "auto"), "</dd>")
    }else if(names(gbif_metadata[i]) == "totalRecords" || names(gbif_metadata[i]) == "numberDatasets"){
      html_to_print <- paste0(html_to_print, "<dd>", prettyNum(gbif_metadata[i], big.mark = ","), "</dd>")
    }else{
      html_to_print <- paste0(html_to_print, "<dd>", gbif_metadata[i], "</dd>")
    }
  }
  
  html_to_print <- paste0(html_to_print, "</dl>")
  #html_to_print <- paste0(html_to_print, "<p><a href=\"", metadata_json, "\" target = _blank>Metadata in JSON</a>")
  
  # collapsed_json <- paste0("
  # <a href=\"#demo\" class=\"btn btn-info\" data-toggle=\"collapse\">Metadata in JSON</a>
  #   <div id=\"demo\" class=\"collapse\">", pre(jsonlite::prettify(jsonlite::toJSON(jsonlite::fromJSON(metadata_json)))))
  # 
  # html_to_print <- paste0(html_to_print, collapsed_json, "</div>")
  
  
  
  html_to_print <- paste0(html_to_print, "<button type=\"button\" class=\"btn btn-info\" data-toggle=\"modal\" data-target=\"#myModal\">Metadata JSON</button>
    <!-- Modal -->
    <div class=\"modal fade\" id=\"myModal\" role=\"dialog\">
      <div class=\"modal-dialog\">
        <!-- Modal content-->
        <div class=\"modal-content\">
          <div class=\"modal-header\">
            <button type=\"button\" class=\"close\" data-dismiss=\"modal\">&times;</button>
              <h4 class=\"modal-title\">Archive Metadata</h4>
                </div>
                <div class=\"modal-body\">
                  ", pre(jsonlite::prettify(jsonlite::toJSON(jsonlite::fromJSON(metadata_json)))), "
                  </div>
                <div class=\"modal-footer\">
                  <button type=\"button\" class=\"btn btn-default\" data-dismiss=\"modal\">Close</button>
                </div>
              </div>
          </div>
          </div>")
  
  
  html_to_print <- paste0(html_to_print, "</div></div></div>")

  #Messages ----
  output$messages <- renderUI({
    req(input$gbif_key)
    metadata_json <- jsonlite::fromJSON(metadata_json)
    no_datasets <- prettyNum(metadata_json$numberDatasets, big.mark = ",", scientific=FALSE)
    
    if (metadata_json$numberDatasets > 1){
      datasets_text <- "datasets"
    }else{
      datasets_text <- "dataset"
    }
    
    HTML(paste0("<p class=\"alert alert-success\" role=\"alert\">Loaded download ", metadata_json$key, " (doi: ", metadata_json$doi, ") with ", no_datasets," ", datasets_text, " and ", prettyNum(metadata_json$totalRecords, big.mark = ",", scientific=FALSE), " occurrence records.</p>"))
  })
  
  HTML(html_to_print)
})
