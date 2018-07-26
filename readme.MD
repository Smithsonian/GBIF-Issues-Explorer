# GBIF Issues Explorer

This Shiny app allows to navigate the records with issues in a GBIF dataset downloaded as Darwin Core.

To use, just save the contents of a Darwin Core Archive to the `data` folder. You should have in `data`:

 * metadata.xml
 * occurrence.txt
 * verbatim.txt
 * multimedia.txt
 * dataset/*.xml

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