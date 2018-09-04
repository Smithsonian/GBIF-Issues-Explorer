# GBIF Issues Explorer

This [Shiny](https://shiny.rstudio.com/) app allows researchers and data/collection managers to navigate the records with issues in a GBIF Darwin Core Archive.

![screencapture-127-0-0-1-4614-2018-09-03-21_32_27](https://user-images.githubusercontent.com/2302171/45005415-3787dd80-afc1-11e8-80d1-4c12e417ad19.png)

The app can be used to:

 * Determine the source of issues:
   * Researchers can determine if the data is usable for a particular analysis
   * Collection and data managers can check their own database and figure out the source of the problem and fix it in the next update to GBIF
 * Determine if an issue would affect an analysis:
   * For example, a COUNTRY_COORDINATE_MISMATCH could be because the coordinates fall just outside the country borders. Is this an error in the coordinates or an expected result of an occurrence in water?

![screencapture-127-0-0-1-4614-2018-09-03-21_34_01](https://user-images.githubusercontent.com/2302171/45005423-466e9000-afc1-11e8-8b4e-dc3f9cc9870e.png)

To use, just provide the key to a Darwin Core Archive from GBIF. The download key can be requested via the GBIF API or on the website. If your download URL is:

`www.gbif.org/occurrence/download/0001419-180824113759888`

Then, the last part, '0001419-180824113759888' is the GBIF key you will need to provide this tool.

The app requires R 3.3, or later, and these packages:

 * shiny
 * DT
 * dplyr
 * ggplot2
 * stringr
 * leaflet
 * XML
 * curl
 * data.table
 * RSQLite
 * jsonlite
 * R.utils
 * shinyWidgets
 * shinycssloaders

To install:

```R
install.packages(
    c("shiny", "DT", "dplyr", 
      "ggplot2", "stringr", "leaflet", 
      "XML", "curl", "data.table", "RSQLite", 
      "jsonlite", "R.utils", "shinyWidgets", 
      "shinycssloaders")
    )
```

Please note that some of these packages may take some time to install due to a number of dependencies. Future improvements will try to reduce these dependencies to a minimum. 

The first time the app is run, it takes some time to create a database, in particular for large data files. Afterwards, it uses the database, so it will be faster. 

Please feel free to submit issues, ideas, suggestions, and pull requests. 
