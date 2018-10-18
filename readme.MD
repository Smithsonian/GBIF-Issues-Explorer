# GBIF Issues Explorer

## Winner of 2nd Place Award in GBIF 2018 Ebbe Nielsen Challenge!

The Global Biodiversity Information Facility ([GBIF](https://gbif.org)) Secretariat announced today that the Smithsonian Institution’s Digitization Program Office ([DPO](http://dpo.si.edu)) was selected by an expert jury as a [winner in the 2018 Ebbe Nielsen Challenge](https://www.gbif.org/news/4TuHBNfycgO4GEMOKkMi4u/six-winners-top-the-2018-ebbe-nielsen-challenge). The entry, submitted by DPO Informatics Program Officer Luis J. Villanueva, the GBIF Issues Explorer, won a Second Place award among 23 entries from countries around the world. The Challenge was open to software tools that used GBIF data or tools to promote open science and open biodiversity data.

![ebbenielsen2018](https://user-images.githubusercontent.com/2302171/47153516-e2243900-d2ad-11e8-9c62-ed6a919f2e69.png)

## Intro 

This [Shiny](https://shiny.rstudio.com/) app allows researchers and data/collection managers to navigate the records with issues in a GBIF Darwin Core Archive.

![screencapture-127-0-0-1-4614-2018-09-03-21_32_27](https://user-images.githubusercontent.com/2302171/45005415-3787dd80-afc1-11e8-80d1-4c12e417ad19.png)

The app can be used to:

 * Determine the source of issues:
   * Researchers can determine if the data is usable for a particular analysis
   * Collection and data managers can check their own database and figure out the source of the problem and fix it in the next update to GBIF
 * Determine if an issue would affect an analysis:
   * For example, a COUNTRY_COORDINATE_MISMATCH could be because the coordinates fall just outside the country borders. Is this an error in the coordinates or an expected result of an occurrence in water?

![screencapture-127-0-0-1-4614-2018-09-03-21_34_01](https://user-images.githubusercontent.com/2302171/45005423-466e9000-afc1-11e8-8b4e-dc3f9cc9870e.png)

## Testing the app in local computer

To test the app locally, without the need of a server, just install R and Shiny. Then, run a command that will download the source files from Github. 

R version 3.3 or better is required. After starting R, copy and paste these commands:

```R
install.packages(
    c("shiny", "DT", "dplyr", 
      "ggplot2", "stringr", "leaflet", 
      "XML", "curl", "data.table", "RSQLite", 
      "jsonlite", "R.utils", "shinyWidgets", 
      "shinycssloaders")
    )

library(shiny)
runGitHub("GBIF-Issues-Explorer", "Smithsonian")
```

Please note that the installation of the required packages may take a few minutes to download and install. Future versions will try to reduce the number of dependencies.

## Install

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

To install the required packages:

```R
install.packages(
    c("shiny", "DT", "dplyr", 
      "ggplot2", "stringr", "leaflet", 
      "XML", "curl", "data.table", "RSQLite", 
      "jsonlite", "R.utils", "shinyWidgets", 
      "shinycssloaders")
    )
```

The first time the app is run, it takes some time to create a database, in particular for large data files. Afterwards, it uses the database, so it will be faster. 

Please feel free to submit issues, ideas, suggestions, and pull requests. 
