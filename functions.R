#gbif_issues ----
# from rgbif::gbif_issues()
# https://github.com/ropensci/rgbif/blob/master/R/gbif_issues.R
gbifissues <- structure(list(
  code = c("bri", "ccm", "cdc", "conti", "cdiv",
           "cdout", "cdrep", "cdrepf", "cdreps", "cdround", "cucdmis", "cudc",
           "cuiv", "cum", "depmms", "depnn", "depnmet", "depunl", "elmms",
           "elnn", "elnmet", "elunl", "gass84", "gdativ", "iddativ", "iddatunl",
           "mdativ", "mdatunl", "muldativ", "muluriiv", "preneglat", "preneglon",
           "preswcd", "rdativ", "rdatm", "rdatunl", "refuriiv", "txmatfuz",
           "txmathi", "txmatnon", "typstativ", "zerocd"),
  issue = c("BASIS_OF_RECORD_INVALID",
            "CONTINENT_COUNTRY_MISMATCH", "CONTINENT_DERIVED_FROM_COORDINATES",
            "CONTINENT_INVALID", "COORDINATE_INVALID", "COORDINATE_OUT_OF_RANGE",
            "COORDINATE_REPROJECTED", "COORDINATE_REPROJECTION_FAILED", "COORDINATE_REPROJECTION_SUSPICIOUS",
            "COORDINATE_ROUNDED", "COUNTRY_COORDINATE_MISMATCH", "COUNTRY_DERIVED_FROM_COORDINATES",
            "COUNTRY_INVALID", "COUNTRY_MISMATCH", "DEPTH_MIN_MAX_SWAPPED",
            "DEPTH_NON_NUMERIC", "DEPTH_NOT_METRIC", "DEPTH_UNLIKELY", "ELEVATION_MIN_MAX_SWAPPED",
            "ELEVATION_NON_NUMERIC", "ELEVATION_NOT_METRIC", "ELEVATION_UNLIKELY",
            "GEODETIC_DATUM_ASSUMED_WGS84", "GEODETIC_DATUM_INVALID", "IDENTIFIED_DATE_INVALID",
            "IDENTIFIED_DATE_UNLIKELY", "MODIFIED_DATE_INVALID", "MODIFIED_DATE_UNLIKELY",
            "MULTIMEDIA_DATE_INVALID", "MULTIMEDIA_URI_INVALID", "PRESUMED_NEGATED_LATITUDE",
            "PRESUMED_NEGATED_LONGITUDE", "PRESUMED_SWAPPED_COORDINATE",
            "RECORDED_DATE_INVALID", "RECORDED_DATE_MISMATCH", "RECORDED_DATE_UNLIKELY",
            "REFERENCES_URI_INVALID", "TAXON_MATCH_FUZZY", "TAXON_MATCH_HIGHERRANK",
            "TAXON_MATCH_NONE", "TYPE_STATUS_INVALID", "ZERO_COORDINATE"),
  description = c("The given basis of record is impossible to interpret or seriously different from the recommended vocabulary.",
                  "The interpreted continent and country do not match up.",
                  "The interpreted continent is based on the coordinates, not the verbatim string information.",
                  "Uninterpretable continent values found.", "Coordinate value given in some form but GBIF is unable to interpret it.",
                  "Coordinate has invalid lat/lon values out of their decimal max range.",
                  "The original coordinate was successfully reprojected from a different geodetic datum to WGS84.",
                  "The given decimal latitude and longitude could not be reprojected to WGS84 based on the provided datum.",
                  "Indicates successful coordinate reprojection according to provided datum, but which results in a datum shift larger than 0.1 decimal degrees.",
                  "Original coordinate modified by rounding to 5 decimals.",
                  "The interpreted occurrence coordinates fall outside of the indicated country.",
                  "The interpreted country is based on the coordinates, not the verbatim string information.",
                  "Uninterpretable country values found.", "Interpreted country for dwc:country and dwc:countryCode contradict each other.",
                  "Set if supplied min>max", "Set if depth is a non numeric value",
                  "Set if supplied depth is not given in the metric system, for example using feet instead of meters",
                  "Set if depth is larger than 11.000m or negative.", "Set if supplied min > max elevation",
                  "Set if elevation is a non numeric value", "Set if supplied elevation is not given in the metric system, for example using feet instead of meters",
                  "Set if elevation is above the troposphere (17km) or below 11km (Mariana Trench).",
                  "Indicating that the interpreted coordinates assume they are based on WGS84 datum as the datum was either not indicated or interpretable.",
                  "The geodetic datum given could not be interpreted.", "The date given for dwc:dateIdentified is invalid and cant be interpreted at all.",
                  "The date given for dwc:dateIdentified is in the future or before Linnean times (1700).",
                  "A (partial) invalid date is given for dc:modified, such as a non existing date, invalid zero month, etc.",
                  "The date given for dc:modified is in the future or predates unix time (1970).",
                  "An invalid date is given for dc:created of a multimedia object.",
                  "An invalid uri is given for a multimedia object.", "Latitude appears to be negated, e.g. 32.3 instead of -32.3",
                  "Longitude appears to be negated, e.g. 32.3 instead of -32.3",
                  "Latitude and longitude appear to be swapped.", "A (partial) invalid date is given, such as a non existing date, invalid zero month, etc.",
                  "The recording date specified as the eventDate string and the individual year, month, day are contradicting.",
                  "The recording date is highly unlikely, falling either into the future or represents a very old date before 1600 that predates modern taxonomy.",
                  "An invalid uri is given for dc:references.", "Matching to the taxonomic backbone can only be done using a fuzzy, non exact match.",
                  "Matching to the taxonomic backbone can only be done on a higher rank and not the scientific name.",
                  "Matching to the taxonomic backbone cannot be done cause there was no match at all or several matches with too little information to keep them apart (homonyms).",
                  "The given type status is impossible to interpret or seriously different from the recommended vocabulary.",
                  "Coordinate is the exact 0/0 coordinate, often indicating a bad null coordinate."
  )), .Names = c("code", "issue", "description"), class = "data.frame", row.names = c(NA, -42L))


#issues dealing with coordinates ----
spatial_issues <- c("CONTINENT_COUNTRY_MISMATCH",
                    "CONTINENT_DERIVED_FROM_COORDINATES",
                    "CONTINENT_INVALID",
                    "COORDINATE_INVALID",
                    "COORDINATE_OUT_OF_RANGE",
                    "COORDINATE_REPROJECTED",
                    "COORDINATE_REPROJECTION_FAILED",
                    "COORDINATE_REPROJECTION_SUSPICIOUS",
                    "COORDINATE_ROUNDED",
                    "COUNTRY_COORDINATE_MISMATCH",
                    "COUNTRY_DERIVED_FROM_COORDINATES",
                    "COUNTRY_INVALID",
                    "GEODETIC_DATUM_ASSUMED_WGS84",
                    "GEODETIC_DATUM_INVALID",
                    "PRESUMED_NEGATED_LATITUDE",
                    "PRESUMED_NEGATED_LONGITUDE",
                    "PRESUMED_SWAPPED_COORDINATE",
                    "ZERO_COORDINATE")


#issues dealing with depth ----
depth_issues <- c("DEPTH_MIN_MAX_SWAPPED",
                  "DEPTH_NON_NUMERIC",
                  "DEPTH_NOT_METRIC",
                  "DEPTH_UNLIKELY")


#issues dealing with elevation ----
elev_issues <- c("ELEVATION_MIN_MAX_SWAPPED",
                 "ELEVATION_NON_NUMERIC",
                 "ELEVATION_NOT_METRIC",
                 "ELEVATION_UNLIKELY")


#issues dealing with dates ----
date_issues <- c("RECORDED_DATE_INVALID",
                 "RECORDED_DATE_MISMATCH",
                 "RECORDED_DATE_UNLIKELY,
                  IDENTIFIED_DATE_UNLIKELY")



# Create table query ----
create_table_query <- 'CREATE TABLE gbif(gbifID INTEGER PRIMARY KEY, abstract, accessRights, accrualMethod, accrualPeriodicity, accrualPolicy, alternative, audience, available, bibliographicCitation, conformsTo, contributor, coverage, created, creator, date, dateAccepted, dateCopyrighted, dateSubmitted, description, educationLevel, extent, format, hasFormat, hasPart, hasVersion, identifier, instructionalMethod, isFormatOf, isPartOf, isReferencedBy, isReplacedBy, isRequiredBy, isVersionOf, issued, language, license, mediator, medium, modified, provenance, publisher, "references", relation, replaces, requires, rights, rightsHolder, source, spatial, subject, tableOfContents, temporal, title, type, valid, institutionID, collectionID, datasetID, institutionCode, collectionCode, datasetName, ownerInstitutionCode, basisOfRecord, informationWithheld, dataGeneralizations, dynamicProperties, occurrenceID, catalogNumber, recordNumber, recordedBy, individualCount, organismQuantity, organismQuantityType, sex, lifeStage, reproductiveCondition, behavior, establishmentMeans, occurrenceStatus, preparations, disposition, associatedReferences, associatedSequences, associatedTaxa, otherCatalogNumbers, occurrenceRemarks, organismID, organismName, organismScope, associatedOccurrences, associatedOrganisms, previousIdentifications, organismRemarks, materialSampleID, eventID, parentEventID, fieldNumber, eventDate, eventTime, startDayOfYear, endDayOfYear, year, month, day, verbatimEventDate, habitat, samplingProtocol, samplingEffort, sampleSizeValue, sampleSizeUnit, fieldNotes, eventRemarks, locationID, higherGeographyID, higherGeography, continent, waterBody, islandGroup, island, countryCode, stateProvince, county, municipality, locality, verbatimLocality, verbatimElevation, verbatimDepth, minimumDistanceAboveSurfaceInMeters, maximumDistanceAboveSurfaceInMeters, locationAccordingTo, locationRemarks, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, coordinatePrecision, pointRadiusSpatialFit, verbatimCoordinateSystem, verbatimSRS, footprintWKT, footprintSRS, footprintSpatialFit, georeferencedBy, georeferencedDate, georeferenceProtocol, georeferenceSources, georeferenceVerificationStatus, georeferenceRemarks, geologicalContextID, earliestEonOrLowestEonothem, latestEonOrHighestEonothem, earliestEraOrLowestErathem, latestEraOrHighestErathem, earliestPeriodOrLowestSystem, latestPeriodOrHighestSystem, earliestEpochOrLowestSeries, latestEpochOrHighestSeries, earliestAgeOrLowestStage, latestAgeOrHighestStage, lowestBiostratigraphicZone, highestBiostratigraphicZone, lithostratigraphicTerms, "group", formation, member, bed, identificationID, identificationQualifier, typeStatus, identifiedBy, dateIdentified, identificationReferences, identificationVerificationStatus, identificationRemarks, taxonID, scientificNameID, acceptedNameUsageID, parentNameUsageID, originalNameUsageID, nameAccordingToID, namePublishedInID, taxonConceptID, scientificName, acceptedNameUsage, parentNameUsage, originalNameUsage, nameAccordingTo, namePublishedIn, namePublishedInYear, higherClassification, kingdom, phylum, class, "order", family, genus, subgenus, specificEpithet, infraspecificEpithet, taxonRank, verbatimTaxonRank, vernacularName, nomenclaturalCode, taxonomicStatus, nomenclaturalStatus, taxonRemarks, datasetKey, publishingCountry, lastInterpreted, elevation, elevationAccuracy, depth, depthAccuracy, distanceAboveSurface, distanceAboveSurfaceAccuracy, issue, mediaType, hasCoordinate, hasGeospatialIssues, taxonKey, kingdomKey, phylumKey, classKey, orderKey, familyKey, genusKey, subgenusKey, speciesKey, species, genericName, typifiedName, protocol, lastParsed, lastCrawled, repatriated, ignorerow BOOLEAN DEFAULT 0);'

# Column names ----
col_names <- c("gbifID","abstract","accessRights","accrualMethod","accrualPeriodicity","accrualPolicy","alternative","audience","available","bibliographicCitation","conformsTo","contributor","coverage","created","creator","date","dateAccepted","dateCopyrighted","dateSubmitted","description","educationLevel","extent","format","hasFormat","hasPart","hasVersion","identifier","instructionalMethod","isFormatOf","isPartOf","isReferencedBy","isReplacedBy","isRequiredBy","isVersionOf","issued","language","license","mediator","medium","modified","provenance","publisher","references","relation","replaces","requires","rights","rightsHolder","source","spatial","subject","tableOfContents","temporal","title","type","valid","institutionID","collectionID","datasetID","institutionCode","collectionCode","datasetName","ownerInstitutionCode","basisOfRecord","informationWithheld","dataGeneralizations","dynamicProperties","occurrenceID","catalogNumber","recordNumber","recordedBy","individualCount","organismQuantity","organismQuantityType","sex","lifeStage","reproductiveCondition","behavior","establishmentMeans","occurrenceStatus","preparations","disposition","associatedReferences","associatedSequences","associatedTaxa","otherCatalogNumbers","occurrenceRemarks","organismID","organismName","organismScope","associatedOccurrences","associatedOrganisms","previousIdentifications","organismRemarks","materialSampleID","eventID","parentEventID","fieldNumber","eventDate","eventTime","startDayOfYear","endDayOfYear","year","month","day","verbatimEventDate","habitat","samplingProtocol","samplingEffort","sampleSizeValue","sampleSizeUnit","fieldNotes","eventRemarks","locationID","higherGeographyID","higherGeography","continent","waterBody","islandGroup","island","countryCode","stateProvince","county","municipality","locality","verbatimLocality","verbatimElevation","verbatimDepth","minimumDistanceAboveSurfaceInMeters","maximumDistanceAboveSurfaceInMeters","locationAccordingTo","locationRemarks","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters","coordinatePrecision","pointRadiusSpatialFit","verbatimCoordinateSystem","verbatimSRS","footprintWKT","footprintSRS","footprintSpatialFit","georeferencedBy","georeferencedDate","georeferenceProtocol","georeferenceSources","georeferenceVerificationStatus","georeferenceRemarks","geologicalContextID","earliestEonOrLowestEonothem","latestEonOrHighestEonothem","earliestEraOrLowestErathem","latestEraOrHighestErathem","earliestPeriodOrLowestSystem","latestPeriodOrHighestSystem","earliestEpochOrLowestSeries","latestEpochOrHighestSeries","earliestAgeOrLowestStage","latestAgeOrHighestStage","lowestBiostratigraphicZone","highestBiostratigraphicZone","lithostratigraphicTerms","group","formation","member","bed","identificationID","identificationQualifier","typeStatus","identifiedBy","dateIdentified","identificationReferences","identificationVerificationStatus","identificationRemarks","taxonID","scientificNameID","acceptedNameUsageID","parentNameUsageID","originalNameUsageID","nameAccordingToID","namePublishedInID","taxonConceptID","scientificName","acceptedNameUsage","parentNameUsage","originalNameUsage","nameAccordingTo","namePublishedIn","namePublishedInYear","higherClassification","kingdom","phylum","class","order","family","genus","subgenus","specificEpithet","infraspecificEpithet","taxonRank","verbatimTaxonRank","vernacularName","nomenclaturalCode","taxonomicStatus","nomenclaturalStatus","taxonRemarks","datasetKey","publishingCountry","lastInterpreted","elevation","elevationAccuracy","depth","depthAccuracy","distanceAboveSurface","distanceAboveSurfaceAccuracy","issue","mediaType","hasCoordinate","hasGeospatialIssues","taxonKey","kingdomKey","phylumKey","classKey","orderKey","familyKey","genusKey","subgenusKey","speciesKey","species","genericName","typifiedName","protocol","lastParsed","lastCrawled","repatriated")

#verbatim_createtable_query ----
verbatim_createtable_query <- 'CREATE TABLE verbatim(gbifID INTEGER PRIMARY KEY, abstract, accessRights, accrualMethod, accrualPeriodicity, accrualPolicy, alternative, audience, available, bibliographicCitation, conformsTo, contributor, coverage, created, creator, date, dateAccepted, dateCopyrighted, dateSubmitted, description, educationLevel, extent, format, hasFormat, hasPart, hasVersion, identifier, instructionalMethod, isFormatOf, isPartOf, isReferencedBy, isReplacedBy, isRequiredBy, isVersionOf, issued, language, license, mediator, medium, modified, provenance, publisher, "references", relation, replaces, requires, rights, rightsHolder, source, spatial, subject, tableOfContents, temporal, title, type, valid, institutionID, collectionID, datasetID, institutionCode, collectionCode, datasetName, ownerInstitutionCode, basisOfRecord, informationWithheld, dataGeneralizations, dynamicProperties, occurrenceID, catalogNumber, recordNumber, recordedBy, individualCount, organismQuantity, organismQuantityType, sex, lifeStage, reproductiveCondition, behavior, establishmentMeans, occurrenceStatus, preparations, disposition, associatedMedia, associatedReferences, associatedSequences, associatedTaxa, otherCatalogNumbers, occurrenceRemarks, organismID, organismName, organismScope, associatedOccurrences, associatedOrganisms, previousIdentifications, organismRemarks, materialSampleID, eventID, parentEventID, fieldNumber, eventDate, eventTime, startDayOfYear, endDayOfYear, year, month, day, verbatimEventDate, habitat, samplingProtocol, samplingEffort, sampleSizeValue, sampleSizeUnit, fieldNotes, eventRemarks, locationID, higherGeographyID, higherGeography, continent, waterBody, islandGroup, island, country, countryCode, stateProvince, county, municipality, locality, verbatimLocality, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth, minimumDistanceAboveSurfaceInMeters, maximumDistanceAboveSurfaceInMeters, locationAccordingTo, locationRemarks, decimalLatitude, decimalLongitude, geodeticDatum, coordinateUncertaintyInMeters, coordinatePrecision, pointRadiusSpatialFit, verbatimCoordinates, verbatimLatitude, verbatimLongitude, verbatimCoordinateSystem, verbatimSRS, footprintWKT, footprintSRS, footprintSpatialFit, georeferencedBy, georeferencedDate, georeferenceProtocol, georeferenceSources, georeferenceVerificationStatus, georeferenceRemarks, geologicalContextID, earliestEonOrLowestEonothem, latestEonOrHighestEonothem, earliestEraOrLowestErathem, latestEraOrHighestErathem, earliestPeriodOrLowestSystem, latestPeriodOrHighestSystem, earliestEpochOrLowestSeries, latestEpochOrHighestSeries, earliestAgeOrLowestStage, latestAgeOrHighestStage, lowestBiostratigraphicZone, highestBiostratigraphicZone, lithostratigraphicTerms, "group", formation, member, bed, identificationID, identificationQualifier, typeStatus, identifiedBy, dateIdentified, identificationReferences, identificationVerificationStatus, identificationRemarks, taxonID, scientificNameID, acceptedNameUsageID, parentNameUsageID, originalNameUsageID, nameAccordingToID, namePublishedInID, taxonConceptID, scientificName, acceptedNameUsage, parentNameUsage, originalNameUsage, nameAccordingTo, namePublishedIn, namePublishedInYear, higherClassification, kingdom, phylum, class, "order", family, genus, subgenus, specificEpithet, infraspecificEpithet, taxonRank, verbatimTaxonRank, scientificNameAuthorship, vernacularName, nomenclaturalCode, taxonomicStatus, nomenclaturalStatus, taxonRemarks)'

#verbatim cols ----
verbatim_cols <- c("gbifID", "abstract", "accessRights", "accrualMethod", "accrualPeriodicity", "accrualPolicy", "alternative", "audience", "available", "bibliographicCitation", "conformsTo", "contributor", "coverage", "created", "creator", "date", "dateAccepted", "dateCopyrighted", "dateSubmitted", "description", "educationLevel", "extent", "format", "hasFormat", "hasPart", "hasVersion", "identifier", "instructionalMethod", "isFormatOf", "isPartOf", "isReferencedBy", "isReplacedBy", "isRequiredBy", "isVersionOf", "issued", "language", "license", "mediator", "medium", "modified", "provenance", "publisher", "references", "relation", "replaces", "requires", "rights", "rightsHolder", "source", "spatial", "subject", "tableOfContents", "temporal", "title", "type", "valid", "institutionID", "collectionID", "datasetID", "institutionCode", "collectionCode", "datasetName", "ownerInstitutionCode", "basisOfRecord", "informationWithheld", "dataGeneralizations", "dynamicProperties", "occurrenceID", "catalogNumber", "recordNumber", "recordedBy", "individualCount", "organismQuantity", "organismQuantityType", "sex", "lifeStage", "reproductiveCondition", "behavior", "establishmentMeans", "occurrenceStatus", "preparations", "disposition", "associatedMedia", "associatedReferences", "associatedSequences", "associatedTaxa", "otherCatalogNumbers", "occurrenceRemarks", "organismID", "organismName", "organismScope", "associatedOccurrences", "associatedOrganisms", "previousIdentifications", "organismRemarks", "materialSampleID", "eventID", "parentEventID", "fieldNumber", "eventDate", "eventTime", "startDayOfYear", "endDayOfYear", "year", "month", "day", "verbatimEventDate", "habitat", "samplingProtocol", "samplingEffort", "sampleSizeValue", "sampleSizeUnit", "fieldNotes", "eventRemarks", "locationID", "higherGeographyID", "higherGeography", "continent", "waterBody", "islandGroup", "island", "country", "countryCode", "stateProvince", "county", "municipality", "locality", "verbatimLocality", "minimumElevationInMeters", "maximumElevationInMeters", "verbatimElevation", "minimumDepthInMeters", "maximumDepthInMeters", "verbatimDepth", "minimumDistanceAboveSurfaceInMeters", "maximumDistanceAboveSurfaceInMeters", "locationAccordingTo", "locationRemarks", "decimalLatitude", "decimalLongitude", "geodeticDatum", "coordinateUncertaintyInMeters", "coordinatePrecision", "pointRadiusSpatialFit", "verbatimCoordinates", "verbatimLatitude", "verbatimLongitude", "verbatimCoordinateSystem", "verbatimSRS", "footprintWKT", "footprintSRS", "footprintSpatialFit", "georeferencedBy", "georeferencedDate", "georeferenceProtocol", "georeferenceSources", "georeferenceVerificationStatus", "georeferenceRemarks", "geologicalContextID", "earliestEonOrLowestEonothem", "latestEonOrHighestEonothem", "earliestEraOrLowestErathem", "latestEraOrHighestErathem", "earliestPeriodOrLowestSystem", "latestPeriodOrHighestSystem", "earliestEpochOrLowestSeries", "latestEpochOrHighestSeries", "earliestAgeOrLowestStage", "latestAgeOrHighestStage", "lowestBiostratigraphicZone", "highestBiostratigraphicZone", "lithostratigraphicTerms", "group", "formation", "member", "bed", "identificationID", "identificationQualifier", "typeStatus", "identifiedBy", "dateIdentified", "identificationReferences", "identificationVerificationStatus", "identificationRemarks", "taxonID", "scientificNameID", "acceptedNameUsageID", "parentNameUsageID", "originalNameUsageID", "nameAccordingToID", "namePublishedInID", "taxonConceptID", "scientificName", "acceptedNameUsage", "parentNameUsage", "originalNameUsage", "nameAccordingTo", "namePublishedIn", "namePublishedInYear", "higherClassification", "kingdom", "phylum", "class", "order", "family", "genus", "subgenus", "specificEpithet", "infraspecificEpithet", "taxonRank", "verbatimTaxonRank", "scientificNameAuthorship", "vernacularName", "nomenclaturalCode", "taxonomicStatus", "nomenclaturalStatus", "taxonRemarks")


create_multimedia_query <- 'CREATE TABLE multimedia(gbifID, type, format, identifier, "references", title, description, created, creator, contributor, publisher, audience, source, license, rightsHolder)'

multimedia_cols <- c("gbifID", "type", "format", "identifier", "references", "title", "description", "created", "creator", "contributor", "publisher", "audience", "source", "license", "rightsHolder")

create_issuetable_query <- 'CREATE TABLE issues(ID INTEGER PRIMARY KEY, gbifID INTEGER, issue);'


# Messages ----
long_loading_msg <- "(this may take a while depending on the size of the download)"


# Check gbif key
check_gbif <- function(gbif_key){
  res <- try(jsonlite::fromJSON(paste0("http://api.gbif.org/v1/occurrence/download/", gbif_key)), silent = TRUE)
  if (class(res) == "try-error"){
    return(FALSE)
  }else{
    return(res)
  }
}



# Download from GBIF ----
download_gbif <- function(gbif_key, export_dir){
  
  res <- try(jsonlite::fromJSON(paste0("http://api.gbif.org/v1/occurrence/download/", gbif_key)), silent = TRUE)
  if (class(res) == "try-error"){
    return(FALSE)
  }else{
    dl <- try(download.file(paste0("http://api.gbif.org/v1/occurrence/download/request/", gbif_key), destfile = paste0(gbif_key, ".zip"), mode = "wb"), silent = TRUE)
    if (class(dl) == "try-error"){
      return(FALSE)
    }else{
      #extract to data/
      unzip(zipfile = paste0(gbif_key, ".zip"), exdir = export_dir)
      file.remove(paste0(gbif_key, ".zip"))
      return(res)
    }
  }
}


# Load datasets to database ----
create_database <- function(database_file, dataset_xml_path){
  
  if (file.exists(database_file)){
    try(unlink(database_file), silent = TRUE)
  }
  gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
  
  #index of issues ----
  n <- dbExecute(gbif_db, create_issuetable_query)
  n <- dbExecute(gbif_db, 'CREATE INDEX issue_issue ON issues(issue);')
  n <- dbExecute(gbif_db, 'CREATE INDEX gbifID_issue ON issues(gbifID);')
  
  #table with data ----
  n <- dbExecute(gbif_db, create_table_query)
  n <- dbExecute(gbif_db, 'CREATE INDEX gbifID ON gbif(gbifID);')
  n <- dbExecute(gbif_db, 'CREATE INDEX issue ON gbif(issue);')
  n <- dbExecute(gbif_db, 'CREATE INDEX gb_datasetKey ON gbif(datasetID);')
  n <- dbExecute(gbif_db, 'CREATE INDEX basisOfRecord ON gbif(basisOfRecord);')
  n <- dbExecute(gbif_db, 'CREATE INDEX scientificName ON gbif(scientificName);')
  n <- dbExecute(gbif_db, 'CREATE INDEX ignorerow ON gbif(ignorerow);')
  
  #verbatim table ----
  n <- dbExecute(gbif_db, verbatim_createtable_query)
  n <- dbExecute(gbif_db, 'CREATE INDEX verbatim_gbifID ON verbatim(gbifID);')
  
  #multimedia ----
  n <- dbExecute(gbif_db, create_multimedia_query)
  n <- dbExecute(gbif_db, 'CREATE INDEX multimedia_gbifID ON multimedia(gbifID);')
  
  #datasets ----
  datasets_xml <- list.files(dataset_xml_path, pattern = "*.xml", full.names = TRUE)
  no_datasets <- length(datasets_xml)
  
  create_datasettable_query <- 'CREATE TABLE datasets(datasetKey PRIMARY KEY, title, institution);'
  
  n <- dbExecute(gbif_db, create_datasettable_query)
  n <- dbExecute(gbif_db, 'CREATE INDEX ds_datasetKey ON datasets(datasetKey);')
  
  for (i in 1:no_datasets){
    meta_file <- xmlToList(datasets_xml[i])
    datasetKey <- str_replace(basename(datasets_xml[i]), ".xml", "")
    datasetTitle <- meta_file$dataset$title
    datasetInst <- meta_file$dataset$creator$organizationName
    n <- dbExecute(gbif_db, paste0("INSERT INTO datasets (datasetKey, title, institution) VALUES ('", datasetKey, "', '", datasetTitle, "', '", datasetInst, "');"))
  }
  # Close db ----
  dbDisconnect(gbif_db)
}
