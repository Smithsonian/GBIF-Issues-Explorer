#gbif_issues ----
# from https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
gbifissues <- read.csv2("gbif_issues.csv", header = TRUE, stringsAsFactors = FALSE, sep = "|")


#issues dealing with coordinates ----
spatial_issues <- c("CONTINENT_COUNTRY_MISMATCH",
                    "CONTINENT_DERIVED_FROM_COORDINATES",
                    "CONTINENT_INVALID",
                    "COORDINATE_INVALID",
                    "COORDINATE_OUT_OF_RANGE",
                    "COORDINATE_PRECISION_INVALID",
                    "COORDINATE_REPROJECTED",
                    "COORDINATE_REPROJECTION_FAILED",
                    "COORDINATE_REPROJECTION_SUSPICIOUS",
                    "COORDINATE_ROUNDED",
                    "COORDINATE_UNCERTAINTY_METERS_INVALID",
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
                 "RECORDED_DATE_UNLIKELY",
                  "IDENTIFIED_DATE_UNLIKELY")


#issues dealing with taxonomy ----
taxo_issues <- c("TAXON_MATCH_FUZZY",
                 "TAXON_MATCH_HIGHERRANK",
                 "TAXON_MATCH_NONE")



# Create table query ----
create_table_query <- 'CREATE TABLE gbif(gbifID INTEGER PRIMARY KEY, abstract, accessRights, accrualMethod, accrualPeriodicity, accrualPolicy, alternative, audience, available, bibliographicCitation, conformsTo, contributor, coverage, created, creator, date, dateAccepted, dateCopyrighted, dateSubmitted, description, educationLevel, extent, format, hasFormat, hasPart, hasVersion, identifier, instructionalMethod, isFormatOf, isPartOf, isReferencedBy, isReplacedBy, isRequiredBy, isVersionOf, issued, language, license, mediator, medium, modified, provenance, publisher, "references", relation, replaces, requires, rights, rightsHolder, source, spatial, subject, tableOfContents, temporal, title, type, valid, institutionID, collectionID, datasetID, institutionCode, collectionCode, datasetName, ownerInstitutionCode, basisOfRecord, informationWithheld, dataGeneralizations, dynamicProperties, occurrenceID, catalogNumber, recordNumber, recordedBy, individualCount, organismQuantity, organismQuantityType, sex, lifeStage, reproductiveCondition, behavior, establishmentMeans, occurrenceStatus, preparations, disposition, associatedReferences, associatedSequences, associatedTaxa, otherCatalogNumbers, occurrenceRemarks, organismID, organismName, organismScope, associatedOccurrences, associatedOrganisms, previousIdentifications, organismRemarks, materialSampleID, eventID, parentEventID, fieldNumber, eventDate, eventTime, startDayOfYear, endDayOfYear, year, month, day, verbatimEventDate, habitat, samplingProtocol, samplingEffort, sampleSizeValue, sampleSizeUnit, fieldNotes, eventRemarks, locationID, higherGeographyID, higherGeography, continent, waterBody, islandGroup, island, countryCode, stateProvince, county, municipality, locality, verbatimLocality, verbatimElevation, verbatimDepth, minimumDistanceAboveSurfaceInMeters, maximumDistanceAboveSurfaceInMeters, locationAccordingTo, locationRemarks, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, coordinatePrecision, pointRadiusSpatialFit, verbatimCoordinateSystem, verbatimSRS, footprintWKT, footprintSRS, footprintSpatialFit, georeferencedBy, georeferencedDate, georeferenceProtocol, georeferenceSources, georeferenceVerificationStatus, georeferenceRemarks, geologicalContextID, earliestEonOrLowestEonothem, latestEonOrHighestEonothem, earliestEraOrLowestErathem, latestEraOrHighestErathem, earliestPeriodOrLowestSystem, latestPeriodOrHighestSystem, earliestEpochOrLowestSeries, latestEpochOrHighestSeries, earliestAgeOrLowestStage, latestAgeOrHighestStage, lowestBiostratigraphicZone, highestBiostratigraphicZone, lithostratigraphicTerms, "group", formation, member, bed, identificationID, identificationQualifier, typeStatus, identifiedBy, dateIdentified, identificationReferences, identificationVerificationStatus, identificationRemarks, taxonID, scientificNameID, acceptedNameUsageID, parentNameUsageID, originalNameUsageID, nameAccordingToID, namePublishedInID, taxonConceptID, scientificName, acceptedNameUsage, parentNameUsage, originalNameUsage, nameAccordingTo, namePublishedIn, namePublishedInYear, higherClassification, kingdom, phylum, class, "order", family, genus, subgenus, specificEpithet, infraspecificEpithet, taxonRank, verbatimTaxonRank, vernacularName, nomenclaturalCode, taxonomicStatus, nomenclaturalStatus, taxonRemarks, datasetKey, publishingCountry, lastInterpreted, elevation, elevationAccuracy, depth, depthAccuracy, distanceAboveSurface, distanceAboveSurfaceAccuracy, issue, mediaType, hasCoordinate, hasGeospatialIssues, taxonKey, kingdomKey, phylumKey, classKey, orderKey, familyKey, genusKey, subgenusKey, speciesKey, species, genericName, typifiedName, protocol, lastParsed, lastCrawled, repatriated, ignorerow BOOLEAN DEFAULT 0);'

create_table_query_237 <- 'CREATE TABLE gbif(gbifID INTEGER PRIMARY KEY, abstract, accessRights, accrualMethod, accrualPeriodicity, accrualPolicy, alternative, audience, available, bibliographicCitation, conformsTo, contributor, coverage, created, creator, date, dateAccepted, dateCopyrighted, dateSubmitted, description, educationLevel, extent, format, hasFormat, hasPart, hasVersion, identifier, instructionalMethod, isFormatOf, isPartOf, isReferencedBy, isReplacedBy, isRequiredBy, isVersionOf, issued, language, license, mediator, medium, modified, provenance, publisher, "references", relation, replaces, requires, rights, rightsHolder, source, spatial, subject, tableOfContents, temporal, title, type, valid, institutionID, collectionID, datasetID, institutionCode, collectionCode, datasetName, ownerInstitutionCode, basisOfRecord, informationWithheld, dataGeneralizations, dynamicProperties, occurrenceID, catalogNumber, recordNumber, recordedBy, individualCount, organismQuantity, organismQuantityType, sex, lifeStage, reproductiveCondition, behavior, establishmentMeans, occurrenceStatus, preparations, disposition, associatedReferences, associatedSequences, associatedTaxa, otherCatalogNumbers, occurrenceRemarks, organismID, organismName, organismScope, associatedOccurrences, associatedOrganisms, previousIdentifications, organismRemarks, materialSampleID, eventID, parentEventID, fieldNumber, eventDate, eventTime, startDayOfYear, endDayOfYear, year, month, day, verbatimEventDate, habitat, samplingProtocol, samplingEffort, sampleSizeValue, sampleSizeUnit, fieldNotes, eventRemarks, locationID, higherGeographyID, higherGeography, continent, waterBody, islandGroup, island, countryCode, stateProvince, county, municipality, locality, verbatimLocality, verbatimElevation, verbatimDepth, minimumDistanceAboveSurfaceInMeters, maximumDistanceAboveSurfaceInMeters, locationAccordingTo, locationRemarks, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, coordinatePrecision, pointRadiusSpatialFit, verbatimCoordinateSystem, verbatimSRS, footprintWKT, footprintSRS, footprintSpatialFit, georeferencedBy, georeferencedDate, georeferenceProtocol, georeferenceSources, georeferenceVerificationStatus, georeferenceRemarks, geologicalContextID, earliestEonOrLowestEonothem, latestEonOrHighestEonothem, earliestEraOrLowestErathem, latestEraOrHighestErathem, earliestPeriodOrLowestSystem, latestPeriodOrHighestSystem, earliestEpochOrLowestSeries, latestEpochOrHighestSeries, earliestAgeOrLowestStage, latestAgeOrHighestStage, lowestBiostratigraphicZone, highestBiostratigraphicZone, lithostratigraphicTerms, "group", formation, member, bed, identificationID, identificationQualifier, typeStatus, identifiedBy, dateIdentified, identificationReferences, identificationVerificationStatus, identificationRemarks, taxonID, scientificNameID, acceptedNameUsageID, parentNameUsageID, originalNameUsageID, nameAccordingToID, namePublishedInID, taxonConceptID, scientificName, acceptedNameUsage, parentNameUsage, originalNameUsage, nameAccordingTo, namePublishedIn, namePublishedInYear, higherClassification, kingdom, phylum, class, "order", family, genus, subgenus, specificEpithet, infraspecificEpithet, taxonRank, verbatimTaxonRank, vernacularName, nomenclaturalCode, taxonomicStatus, nomenclaturalStatus, taxonRemarks, datasetKey, publishingCountry, lastInterpreted, elevation, elevationAccuracy, depth, depthAccuracy, distanceAboveSurface, distanceAboveSurfaceAccuracy, issue, mediaType, hasCoordinate, hasGeospatialIssues, taxonKey, acceptedTaxonKey, kingdomKey, phylumKey, classKey, orderKey, familyKey, genusKey, subgenusKey, speciesKey, species, genericName, acceptedScientificName, typifiedName, protocol, lastParsed, lastCrawled, repatriated, ignorerow BOOLEAN DEFAULT 0);'


create_table_query_239 <- 'CREATE TABLE gbif(gbifID INTEGER PRIMARY KEY, abstract, accessRights, accrualMethod, accrualPeriodicity, accrualPolicy, alternative, audience, available, bibliographicCitation, conformsTo, contributor, coverage, created, creator, date, dateAccepted, dateCopyrighted, dateSubmitted, description, educationLevel, extent, format, hasFormat, hasPart, hasVersion, identifier, instructionalMethod, isFormatOf, isPartOf, isReferencedBy, isReplacedBy, isRequiredBy, isVersionOf, issued, language, license, mediator, medium, modified, provenance, publisher, "references", relation, replaces, requires, rights, rightsHolder, source, spatial, subject, tableOfContents, temporal, title, type, valid, institutionID, collectionID, datasetID, institutionCode, collectionCode, datasetName, ownerInstitutionCode, basisOfRecord, informationWithheld, dataGeneralizations, dynamicProperties, occurrenceID, catalogNumber, recordNumber, recordedBy, individualCount, organismQuantity, organismQuantityType, sex, lifeStage, reproductiveCondition, behavior, establishmentMeans, occurrenceStatus, preparations, disposition, associatedReferences, associatedSequences, associatedTaxa, otherCatalogNumbers, occurrenceRemarks, organismID, organismName, organismScope, associatedOccurrences, associatedOrganisms, previousIdentifications, organismRemarks, materialSampleID, eventID, parentEventID, fieldNumber, eventDate, eventTime, startDayOfYear, endDayOfYear, year, month, day, verbatimEventDate, habitat, samplingProtocol, samplingEffort, sampleSizeValue, sampleSizeUnit, fieldNotes, eventRemarks, locationID, higherGeographyID, higherGeography, continent, waterBody, islandGroup, island, countryCode, stateProvince, county, municipality, locality, verbatimLocality, verbatimElevation, verbatimDepth, minimumDistanceAboveSurfaceInMeters, maximumDistanceAboveSurfaceInMeters, locationAccordingTo, locationRemarks, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, coordinatePrecision, pointRadiusSpatialFit, verbatimCoordinateSystem, verbatimSRS, footprintWKT, footprintSRS, footprintSpatialFit, georeferencedBy, georeferencedDate, georeferenceProtocol, georeferenceSources, georeferenceVerificationStatus, georeferenceRemarks, geologicalContextID, earliestEonOrLowestEonothem, latestEonOrHighestEonothem, earliestEraOrLowestErathem, latestEraOrHighestErathem, earliestPeriodOrLowestSystem, latestPeriodOrHighestSystem, earliestEpochOrLowestSeries, latestEpochOrHighestSeries, earliestAgeOrLowestStage, latestAgeOrHighestStage, lowestBiostratigraphicZone, highestBiostratigraphicZone, lithostratigraphicTerms, "group", formation, member, bed, identificationID, identificationQualifier, typeStatus, identifiedBy, dateIdentified, identificationReferences, identificationVerificationStatus, identificationRemarks, taxonID, scientificNameID, acceptedNameUsageID, parentNameUsageID, originalNameUsageID, nameAccordingToID, namePublishedInID, taxonConceptID, scientificName, acceptedNameUsage, parentNameUsage, originalNameUsage, nameAccordingTo, namePublishedIn, namePublishedInYear, higherClassification, kingdom, phylum, class, "order", family, genus, subgenus, specificEpithet, infraspecificEpithet, taxonRank, verbatimTaxonRank, vernacularName, nomenclaturalCode, taxonomicStatus, nomenclaturalStatus, taxonRemarks, datasetKey, publishingCountry, lastInterpreted, elevation, elevationAccuracy, depth, depthAccuracy, distanceAboveSurface, distanceAboveSurfaceAccuracy, issue, mediaType, hasCoordinate, hasGeospatialIssues, taxonKey, acceptedTaxonKey, kingdomKey, phylumKey, classKey, orderKey, familyKey, genusKey, subgenusKey, speciesKey, species, genericName, acceptedScientificName, verbatimScientificName, typifiedName, protocol, lastParsed, lastCrawled, repatriated, relativeOrganismQuantity, ignorerow BOOLEAN DEFAULT 0);'

# Column names ----
col_names <- c("gbifID","abstract","accessRights","accrualMethod","accrualPeriodicity","accrualPolicy","alternative","audience","available","bibliographicCitation","conformsTo","contributor","coverage","created","creator","date","dateAccepted","dateCopyrighted","dateSubmitted","description","educationLevel","extent","format","hasFormat","hasPart","hasVersion","identifier","instructionalMethod","isFormatOf","isPartOf","isReferencedBy","isReplacedBy","isRequiredBy","isVersionOf","issued","language","license","mediator","medium","modified","provenance","publisher","references","relation","replaces","requires","rights","rightsHolder","source","spatial","subject","tableOfContents","temporal","title","type","valid","institutionID","collectionID","datasetID","institutionCode","collectionCode","datasetName","ownerInstitutionCode","basisOfRecord","informationWithheld","dataGeneralizations","dynamicProperties","occurrenceID","catalogNumber","recordNumber","recordedBy","individualCount","organismQuantity","organismQuantityType","sex","lifeStage","reproductiveCondition","behavior","establishmentMeans","occurrenceStatus","preparations","disposition","associatedReferences","associatedSequences","associatedTaxa","otherCatalogNumbers","occurrenceRemarks","organismID","organismName","organismScope","associatedOccurrences","associatedOrganisms","previousIdentifications","organismRemarks","materialSampleID","eventID","parentEventID","fieldNumber","eventDate","eventTime","startDayOfYear","endDayOfYear","year","month","day","verbatimEventDate","habitat","samplingProtocol","samplingEffort","sampleSizeValue","sampleSizeUnit","fieldNotes","eventRemarks","locationID","higherGeographyID","higherGeography","continent","waterBody","islandGroup","island","countryCode","stateProvince","county","municipality","locality","verbatimLocality","verbatimElevation","verbatimDepth","minimumDistanceAboveSurfaceInMeters","maximumDistanceAboveSurfaceInMeters","locationAccordingTo","locationRemarks","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters","coordinatePrecision","pointRadiusSpatialFit","verbatimCoordinateSystem","verbatimSRS","footprintWKT","footprintSRS","footprintSpatialFit","georeferencedBy","georeferencedDate","georeferenceProtocol","georeferenceSources","georeferenceVerificationStatus","georeferenceRemarks","geologicalContextID","earliestEonOrLowestEonothem","latestEonOrHighestEonothem","earliestEraOrLowestErathem","latestEraOrHighestErathem","earliestPeriodOrLowestSystem","latestPeriodOrHighestSystem","earliestEpochOrLowestSeries","latestEpochOrHighestSeries","earliestAgeOrLowestStage","latestAgeOrHighestStage","lowestBiostratigraphicZone","highestBiostratigraphicZone","lithostratigraphicTerms","group","formation","member","bed","identificationID","identificationQualifier","typeStatus","identifiedBy","dateIdentified","identificationReferences","identificationVerificationStatus","identificationRemarks","taxonID","scientificNameID","acceptedNameUsageID","parentNameUsageID","originalNameUsageID","nameAccordingToID","namePublishedInID","taxonConceptID","scientificName","acceptedNameUsage","parentNameUsage","originalNameUsage","nameAccordingTo","namePublishedIn","namePublishedInYear","higherClassification","kingdom","phylum","class","order","family","genus","subgenus","specificEpithet","infraspecificEpithet","taxonRank","verbatimTaxonRank","vernacularName","nomenclaturalCode","taxonomicStatus","nomenclaturalStatus","taxonRemarks","datasetKey","publishingCountry","lastInterpreted","elevation","elevationAccuracy","depth","depthAccuracy","distanceAboveSurface","distanceAboveSurfaceAccuracy","issue","mediaType","hasCoordinate","hasGeospatialIssues","taxonKey","kingdomKey","phylumKey","classKey","orderKey","familyKey","genusKey","subgenusKey","speciesKey","species","genericName","typifiedName","protocol","lastParsed","lastCrawled","repatriated")


# Column names ----
col_names_237 <- c("gbifID","abstract","accessRights","accrualMethod","accrualPeriodicity","accrualPolicy","alternative","audience","available","bibliographicCitation","conformsTo","contributor","coverage","created","creator","date","dateAccepted","dateCopyrighted","dateSubmitted","description","educationLevel","extent","format","hasFormat","hasPart","hasVersion","identifier","instructionalMethod","isFormatOf","isPartOf","isReferencedBy","isReplacedBy","isRequiredBy","isVersionOf","issued","language","license","mediator","medium","modified","provenance","publisher","references","relation","replaces","requires","rights","rightsHolder","source","spatial","subject","tableOfContents","temporal","title","type","valid","institutionID","collectionID","datasetID","institutionCode","collectionCode","datasetName","ownerInstitutionCode","basisOfRecord","informationWithheld","dataGeneralizations","dynamicProperties","occurrenceID","catalogNumber","recordNumber","recordedBy","individualCount","organismQuantity","organismQuantityType","sex","lifeStage","reproductiveCondition","behavior","establishmentMeans","occurrenceStatus","preparations","disposition","associatedReferences","associatedSequences","associatedTaxa","otherCatalogNumbers","occurrenceRemarks","organismID","organismName","organismScope","associatedOccurrences","associatedOrganisms","previousIdentifications","organismRemarks","materialSampleID","eventID","parentEventID","fieldNumber","eventDate","eventTime","startDayOfYear","endDayOfYear","year","month","day","verbatimEventDate","habitat","samplingProtocol","samplingEffort","sampleSizeValue","sampleSizeUnit","fieldNotes","eventRemarks","locationID","higherGeographyID","higherGeography","continent","waterBody","islandGroup","island","countryCode","stateProvince","county","municipality","locality","verbatimLocality","verbatimElevation","verbatimDepth","minimumDistanceAboveSurfaceInMeters","maximumDistanceAboveSurfaceInMeters","locationAccordingTo","locationRemarks","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters","coordinatePrecision","pointRadiusSpatialFit","verbatimCoordinateSystem","verbatimSRS","footprintWKT","footprintSRS","footprintSpatialFit","georeferencedBy","georeferencedDate","georeferenceProtocol","georeferenceSources","georeferenceVerificationStatus","georeferenceRemarks","geologicalContextID","earliestEonOrLowestEonothem","latestEonOrHighestEonothem","earliestEraOrLowestErathem","latestEraOrHighestErathem","earliestPeriodOrLowestSystem","latestPeriodOrHighestSystem","earliestEpochOrLowestSeries","latestEpochOrHighestSeries","earliestAgeOrLowestStage","latestAgeOrHighestStage","lowestBiostratigraphicZone","highestBiostratigraphicZone","lithostratigraphicTerms","group","formation","member","bed","identificationID","identificationQualifier","typeStatus","identifiedBy","dateIdentified","identificationReferences","identificationVerificationStatus","identificationRemarks","taxonID","scientificNameID","acceptedNameUsageID","parentNameUsageID","originalNameUsageID","nameAccordingToID","namePublishedInID","taxonConceptID","scientificName","acceptedNameUsage","parentNameUsage","originalNameUsage","nameAccordingTo","namePublishedIn","namePublishedInYear","higherClassification","kingdom","phylum","class","order","family","genus","subgenus","specificEpithet","infraspecificEpithet","taxonRank","verbatimTaxonRank","vernacularName","nomenclaturalCode","taxonomicStatus","nomenclaturalStatus","taxonRemarks","datasetKey","publishingCountry","lastInterpreted","elevation","elevationAccuracy","depth","depthAccuracy","distanceAboveSurface","distanceAboveSurfaceAccuracy","issue","mediaType","hasCoordinate","hasGeospatialIssues","taxonKey","acceptedTaxonKey","kingdomKey","phylumKey","classKey","orderKey","familyKey","genusKey","subgenusKey","speciesKey","species","genericName","acceptedScientificName","typifiedName","protocol","lastParsed","lastCrawled","repatriated")

col_names_239 <- c("gbifID", "abstract", "accessRights", "accrualMethod", "accrualPeriodicity", "accrualPolicy", "alternative", "audience", "available", "bibliographicCitation", "conformsTo", "contributor", "coverage", "created", "creator", "date", "dateAccepted", "dateCopyrighted", "dateSubmitted", "description", "educationLevel", "extent", "format", "hasFormat", "hasPart", "hasVersion", "identifier", "instructionalMethod", "isFormatOf", "isPartOf", "isReferencedBy", "isReplacedBy", "isRequiredBy", "isVersionOf", "issued", "language", "license", "mediator", "medium", "modified", "provenance", "publisher", "references", "relation", "replaces", "requires", "rights", "rightsHolder", "source", "spatial", "subject", "tableOfContents", "temporal", "title", "type", "valid", "institutionID", "collectionID", "datasetID", "institutionCode", "collectionCode", "datasetName", "ownerInstitutionCode", "basisOfRecord", "informationWithheld", "dataGeneralizations", "dynamicProperties", "occurrenceID", "catalogNumber", "recordNumber", "recordedBy", "individualCount", "organismQuantity", "organismQuantityType", "sex", "lifeStage", "reproductiveCondition", "behavior", "establishmentMeans", "occurrenceStatus", "preparations", "disposition", "associatedReferences", "associatedSequences", "associatedTaxa", "otherCatalogNumbers", "occurrenceRemarks", "organismID", "organismName", "organismScope", "associatedOccurrences", "associatedOrganisms", "previousIdentifications", "organismRemarks", "materialSampleID", "eventID", "parentEventID", "fieldNumber", "eventDate", "eventTime", "startDayOfYear", "endDayOfYear", "year", "month", "day", "verbatimEventDate", "habitat", "samplingProtocol", "samplingEffort", "sampleSizeValue", "sampleSizeUnit", "fieldNotes", "eventRemarks", "locationID", "higherGeographyID", "higherGeography", "continent", "waterBody", "islandGroup", "island", "countryCode", "stateProvince", "county", "municipality", "locality", "verbatimLocality", "verbatimElevation", "verbatimDepth", "minimumDistanceAboveSurfaceInMeters", "maximumDistanceAboveSurfaceInMeters", "locationAccordingTo", "locationRemarks", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "coordinatePrecision", "pointRadiusSpatialFit", "verbatimCoordinateSystem", "verbatimSRS", "footprintWKT", "footprintSRS", "footprintSpatialFit", "georeferencedBy", "georeferencedDate", "georeferenceProtocol", "georeferenceSources", "georeferenceVerificationStatus", "georeferenceRemarks", "geologicalContextID", "earliestEonOrLowestEonothem", "latestEonOrHighestEonothem", "earliestEraOrLowestErathem", "latestEraOrHighestErathem", "earliestPeriodOrLowestSystem", "latestPeriodOrHighestSystem", "earliestEpochOrLowestSeries", "latestEpochOrHighestSeries", "earliestAgeOrLowestStage", "latestAgeOrHighestStage", "lowestBiostratigraphicZone", "highestBiostratigraphicZone", "lithostratigraphicTerms", "group", "formation", "member", "bed", "identificationID", "identificationQualifier", "typeStatus", "identifiedBy", "dateIdentified", "identificationReferences", "identificationVerificationStatus", "identificationRemarks", "taxonID", "scientificNameID", "acceptedNameUsageID", "parentNameUsageID", "originalNameUsageID", "nameAccordingToID", "namePublishedInID", "taxonConceptID", "scientificName", "acceptedNameUsage", "parentNameUsage", "originalNameUsage", "nameAccordingTo", "namePublishedIn", "namePublishedInYear", "higherClassification", "kingdom", "phylum", "class", "order", "family", "genus", "subgenus", "specificEpithet", "infraspecificEpithet", "taxonRank", "verbatimTaxonRank", "vernacularName", "nomenclaturalCode", "taxonomicStatus", "nomenclaturalStatus", "taxonRemarks", "datasetKey", "publishingCountry", "lastInterpreted", "elevation", "elevationAccuracy", "depth", "depthAccuracy", "distanceAboveSurface", "distanceAboveSurfaceAccuracy", "issue", "mediaType", "hasCoordinate", "hasGeospatialIssues", "taxonKey", "acceptedTaxonKey", "kingdomKey", "phylumKey", "classKey", "orderKey", "familyKey", "genusKey", "subgenusKey", "speciesKey", "species", "genericName", "acceptedScientificName", "verbatimScientificName", "typifiedName", "protocol", "lastParsed", "lastCrawled", "repatriated", "relativeOrganismQuantity")

#verbatim_createtable_query ----
verbatim_createtable_query <- 'CREATE TABLE verbatim(gbifID INTEGER PRIMARY KEY, abstract, accessRights, accrualMethod, accrualPeriodicity, accrualPolicy, alternative, audience, available, bibliographicCitation, conformsTo, contributor, coverage, created, creator, date, dateAccepted, dateCopyrighted, dateSubmitted, description, educationLevel, extent, format, hasFormat, hasPart, hasVersion, identifier, instructionalMethod, isFormatOf, isPartOf, isReferencedBy, isReplacedBy, isRequiredBy, isVersionOf, issued, language, license, mediator, medium, modified, provenance, publisher, "references", relation, replaces, requires, rights, rightsHolder, source, spatial, subject, tableOfContents, temporal, title, type, valid, institutionID, collectionID, datasetID, institutionCode, collectionCode, datasetName, ownerInstitutionCode, basisOfRecord, informationWithheld, dataGeneralizations, dynamicProperties, occurrenceID, catalogNumber, recordNumber, recordedBy, individualCount, organismQuantity, organismQuantityType, sex, lifeStage, reproductiveCondition, behavior, establishmentMeans, occurrenceStatus, preparations, disposition, associatedMedia, associatedReferences, associatedSequences, associatedTaxa, otherCatalogNumbers, occurrenceRemarks, organismID, organismName, organismScope, associatedOccurrences, associatedOrganisms, previousIdentifications, organismRemarks, materialSampleID, eventID, parentEventID, fieldNumber, eventDate, eventTime, startDayOfYear, endDayOfYear, year, month, day, verbatimEventDate, habitat, samplingProtocol, samplingEffort, sampleSizeValue, sampleSizeUnit, fieldNotes, eventRemarks, locationID, higherGeographyID, higherGeography, continent, waterBody, islandGroup, island, country, countryCode, stateProvince, county, municipality, locality, verbatimLocality, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth, minimumDistanceAboveSurfaceInMeters, maximumDistanceAboveSurfaceInMeters, locationAccordingTo, locationRemarks, decimalLatitude, decimalLongitude, geodeticDatum, coordinateUncertaintyInMeters, coordinatePrecision, pointRadiusSpatialFit, verbatimCoordinates, verbatimLatitude, verbatimLongitude, verbatimCoordinateSystem, verbatimSRS, footprintWKT, footprintSRS, footprintSpatialFit, georeferencedBy, georeferencedDate, georeferenceProtocol, georeferenceSources, georeferenceVerificationStatus, georeferenceRemarks, geologicalContextID, earliestEonOrLowestEonothem, latestEonOrHighestEonothem, earliestEraOrLowestErathem, latestEraOrHighestErathem, earliestPeriodOrLowestSystem, latestPeriodOrHighestSystem, earliestEpochOrLowestSeries, latestEpochOrHighestSeries, earliestAgeOrLowestStage, latestAgeOrHighestStage, lowestBiostratigraphicZone, highestBiostratigraphicZone, lithostratigraphicTerms, "group", formation, member, bed, identificationID, identificationQualifier, typeStatus, identifiedBy, dateIdentified, identificationReferences, identificationVerificationStatus, identificationRemarks, taxonID, scientificNameID, acceptedNameUsageID, parentNameUsageID, originalNameUsageID, nameAccordingToID, namePublishedInID, taxonConceptID, scientificName, acceptedNameUsage, parentNameUsage, originalNameUsage, nameAccordingTo, namePublishedIn, namePublishedInYear, higherClassification, kingdom, phylum, class, "order", family, genus, subgenus, specificEpithet, infraspecificEpithet, taxonRank, verbatimTaxonRank, scientificNameAuthorship, vernacularName, nomenclaturalCode, taxonomicStatus, nomenclaturalStatus, taxonRemarks)'

#verbatim cols ----
verbatim_cols <- c("gbifID", "abstract", "accessRights", "accrualMethod", "accrualPeriodicity", "accrualPolicy", "alternative", "audience", "available", "bibliographicCitation", "conformsTo", "contributor", "coverage", "created", "creator", "date", "dateAccepted", "dateCopyrighted", "dateSubmitted", "description", "educationLevel", "extent", "format", "hasFormat", "hasPart", "hasVersion", "identifier", "instructionalMethod", "isFormatOf", "isPartOf", "isReferencedBy", "isReplacedBy", "isRequiredBy", "isVersionOf", "issued", "language", "license", "mediator", "medium", "modified", "provenance", "publisher", "references", "relation", "replaces", "requires", "rights", "rightsHolder", "source", "spatial", "subject", "tableOfContents", "temporal", "title", "type", "valid", "institutionID", "collectionID", "datasetID", "institutionCode", "collectionCode", "datasetName", "ownerInstitutionCode", "basisOfRecord", "informationWithheld", "dataGeneralizations", "dynamicProperties", "occurrenceID", "catalogNumber", "recordNumber", "recordedBy", "individualCount", "organismQuantity", "organismQuantityType", "sex", "lifeStage", "reproductiveCondition", "behavior", "establishmentMeans", "occurrenceStatus", "preparations", "disposition", "associatedMedia", "associatedReferences", "associatedSequences", "associatedTaxa", "otherCatalogNumbers", "occurrenceRemarks", "organismID", "organismName", "organismScope", "associatedOccurrences", "associatedOrganisms", "previousIdentifications", "organismRemarks", "materialSampleID", "eventID", "parentEventID", "fieldNumber", "eventDate", "eventTime", "startDayOfYear", "endDayOfYear", "year", "month", "day", "verbatimEventDate", "habitat", "samplingProtocol", "samplingEffort", "sampleSizeValue", "sampleSizeUnit", "fieldNotes", "eventRemarks", "locationID", "higherGeographyID", "higherGeography", "continent", "waterBody", "islandGroup", "island", "country", "countryCode", "stateProvince", "county", "municipality", "locality", "verbatimLocality", "minimumElevationInMeters", "maximumElevationInMeters", "verbatimElevation", "minimumDepthInMeters", "maximumDepthInMeters", "verbatimDepth", "minimumDistanceAboveSurfaceInMeters", "maximumDistanceAboveSurfaceInMeters", "locationAccordingTo", "locationRemarks", "decimalLatitude", "decimalLongitude", "geodeticDatum", "coordinateUncertaintyInMeters", "coordinatePrecision", "pointRadiusSpatialFit", "verbatimCoordinates", "verbatimLatitude", "verbatimLongitude", "verbatimCoordinateSystem", "verbatimSRS", "footprintWKT", "footprintSRS", "footprintSpatialFit", "georeferencedBy", "georeferencedDate", "georeferenceProtocol", "georeferenceSources", "georeferenceVerificationStatus", "georeferenceRemarks", "geologicalContextID", "earliestEonOrLowestEonothem", "latestEonOrHighestEonothem", "earliestEraOrLowestErathem", "latestEraOrHighestErathem", "earliestPeriodOrLowestSystem", "latestPeriodOrHighestSystem", "earliestEpochOrLowestSeries", "latestEpochOrHighestSeries", "earliestAgeOrLowestStage", "latestAgeOrHighestStage", "lowestBiostratigraphicZone", "highestBiostratigraphicZone", "lithostratigraphicTerms", "group", "formation", "member", "bed", "identificationID", "identificationQualifier", "typeStatus", "identifiedBy", "dateIdentified", "identificationReferences", "identificationVerificationStatus", "identificationRemarks", "taxonID", "scientificNameID", "acceptedNameUsageID", "parentNameUsageID", "originalNameUsageID", "nameAccordingToID", "namePublishedInID", "taxonConceptID", "scientificName", "acceptedNameUsage", "parentNameUsage", "originalNameUsage", "nameAccordingTo", "namePublishedIn", "namePublishedInYear", "higherClassification", "kingdom", "phylum", "class", "order", "family", "genus", "subgenus", "specificEpithet", "infraspecificEpithet", "taxonRank", "verbatimTaxonRank", "scientificNameAuthorship", "vernacularName", "nomenclaturalCode", "taxonomicStatus", "nomenclaturalStatus", "taxonRemarks")


create_multimedia_query <- 'CREATE TABLE multimedia(gbifID, type, format, identifier, "references", title, description, created, creator, contributor, publisher, audience, source, license, rightsHolder)'

multimedia_cols <- c("gbifID", "type", "format", "identifier", "references", "title", "description", "created", "creator", "contributor", "publisher", "audience", "source", "license", "rightsHolder")

create_issuetable_query <- 'CREATE TABLE issues(ID INTEGER PRIMARY KEY, gbifID INTEGER, issue);'


# Messages ----
long_loading_msg <- "(this may take a while depending on the size of the download)"


# Check gbif key ----
check_gbif <- function(gbif_key){
  res <- try(jsonlite::fromJSON(paste0("http://api.gbif.org/v1/occurrence/download/", gbif_key)), silent = TRUE)
  if (class(res) == "try-error"){
    return(FALSE)
  }else{
    return(res)
  }
}



# Check if gbif key if DwC ----
check_gbif_dw <- function(gbif_key){
  res <- jsonlite::fromJSON(paste0("http://api.gbif.org/v1/occurrence/download/", gbif_key))
  if (res$request$format == "DWCA"){
    return(TRUE)
  }else{
    return(FALSE)
  }
}



# Download from GBIF ----
download_gbif <- function(gbif_key, export_dir){
    dl <- try(download.file(paste0("http://api.gbif.org/v1/occurrence/download/request/", gbif_key), destfile = paste0(gbif_key, ".zip"), mode = "wb"), silent = TRUE)
    if (class(dl) == "try-error"){
      return(FALSE)
    }else{
      #extract to data/
      unzip(zipfile = paste0(gbif_key, ".zip"), exdir = export_dir)
      file.remove(paste0(gbif_key, ".zip"))
      res <- jsonlite::fromJSON(paste0("http://api.gbif.org/v1/occurrence/download/", gbif_key))
      return(res)
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
}
