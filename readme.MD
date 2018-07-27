# GBIF Issues Explorer

This [Shiny](https://shiny.rstudio.com/) app allows researchers and data/collection managers to navigate the records with issues in a GBIF Darwin Core download.

The app can be used to:

 * Determine the source of issues:
   * Researchers can determine if the data is usable for a particular analysis
   * Collection and data managers can check their own database and figure out the source of the problem and fix it in the next update to GBIF
 * Determine if an issue would affect an analysis:
   * For example, a COUNTRY_COORDINATE_MISMATCH could be because the coordinates fall just outside the country borders. Is this an error in the coordinates or an expected result of an occurrence in water?

To use, just save the contents of a Darwin Core Archive download to the `data` folder. You should have these files in the `data` folder:

 * metadata.xml
 * occurrence.txt
 * verbatim.txt
 * multimedia.txt
 * dataset/*.xml (can be one or many)

The app requires these packages:

 * shiny
 * DT
 * dplyr
 * ggplot2
 * stringr
 * leaflet
 * XML
 * data.table
 * RSQLite

To install:

```R
install.packages(c("shiny", "DT", "dplyr", "ggplot2", "stringr", "leaflet", "XML", "data.table", "RSQLite"))
```

The first time the app is run, it takes some time to create a database, in particular for large data files. Afterwards, it uses the database, so it will be faster. 

More improvements are planned:

 * Table based on querying the database to avoid slowdowns with large datasets
 * Improving speed in database creation
 * Adding options to tag or delete records from the database

Please feel free to submit issues, ideas, suggestions, and pull requests. 
