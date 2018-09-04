# GBIF Issues Explorer

This [Shiny](https://shiny.rstudio.com/) app allows researchers and data/collection managers to navigate the records with issues in a GBIF Darwin Core Archive.

![gbif](https://user-images.githubusercontent.com/2302171/43324300-c916968c-9181-11e8-8a47-5cd262820efe.png)

The app can be used to:

 * Determine the source of issues:
   * Researchers can determine if the data is usable for a particular analysis
   * Collection and data managers can check their own database and figure out the source of the problem and fix it in the next update to GBIF
 * Determine if an issue would affect an analysis:
   * For example, a COUNTRY_COORDINATE_MISMATCH could be because the coordinates fall just outside the country borders. Is this an error in the coordinates or an expected result of an occurrence in water?

To use, just save the contents of a Darwin Core Archive download to the `data` folder. 

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
 * jsonlite
 * R.utils
 * shinyWidgets
 * shinycssloaders

To install:

```R
install.packages(c("shiny", "DT", "dplyr", "ggplot2", "stringr", "leaflet", "XML", "data.table", "RSQLite", "jsonlite", "R.utils", "shinyWidgets", "shinycssloaders"))
```

The first time the app is run, it takes some time to create a database, in particular for large data files. Afterwards, it uses the database, so it will be faster. 

Please feel free to submit issues, ideas, suggestions, and pull requests. 
